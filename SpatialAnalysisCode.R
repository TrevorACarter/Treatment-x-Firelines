#### Begin - Load Dependencies and Set WD ####
library(terra)
library(sf)
library(exactextractr)
library(dplyr)
library(caret)
library(randomForest)
library(pROC)
library(viridis)
setwd("D:/Outside Boundary")


#### Load Extent for Cleaning Data ####
## study area is 13 forested ecoregions in western CONUS
temp <- list.files(path = "./Geographic Subsets/Ecoregions", pattern="*.shp") ## finding the shapefiles within file path that are in the study area
obj.names <- gsub(".shp", "", temp) ## creating a vector with the object names without .shp extension for naming each ecoregion

temp.list <- list()
for(i in 1:length(temp)){
  path <- paste("./Geographic Subsets/Ecoregions/", temp[i], sep = "")
  temp.list[[i]] <- terra::vect(path)
  temp.list[[i]]$ecoregion <- obj.names[i]
} ## for loop for loading in the shapefiles within the file path specfied in temp
rm(i);rm(path);rm(obj.names);rm(temp) ## removing temporary objects

WesternForests <- Reduce(rbind, temp.list) ## combining the SpatVectors within the list into a single object
plot(WesternForests) ## plotting the study region to double check it worked
rm(temp.list) ## keeping global environment clean
writeVector(WesternForests, "./Geographic Subsets/WesternForestEcoregions.shp", overwrite = TRUE)


#### Adding Fire Polygon data from MTBS ####
FirePoly <- vect("./mtbs_perimeter_data/mtbs_perims.shp")
FirePoly$Ig_Date <- as.numeric(substr(FirePoly$Ig_Date,1,4)) ## changing the date column to only contain the year
FirePoly <- FirePoly[FirePoly$Ig_Date >= 2018,] ## subsetting only 2018 - 2024
FirePoly$Incid_Name <- tolower(FirePoly$Incid_Name) ## making the incident names lowercase to remove potential duplicates
FirePoly <- FirePoly[order(FirePoly$BurnBndAc, decreasing = TRUE),] ## sorting by size
FirePoly <- FirePoly[!duplicated(paste(FirePoly$Incid_Name,FirePoly$Ig_Date,FirePoly$BurnBndAc)),] ## removing duplicates
length(unique(paste(FirePoly$Incid_Name,FirePoly$Ig_Date,FirePoly$BurnBndAc))) ## 5918 unique events
gc()

FirePoly <- FirePoly[,c(colnames(values(FirePoly)) == "Incid_Name" | 
                          colnames(values(FirePoly)) == "BurnBndAc" |
                          colnames(values(FirePoly)) == "Ig_Date")]
gc()

## double checking each of the column names
colnames(values(FirePoly))

table(terra::is.valid(FirePoly)) ## checking the validity of geometery
## have several thousand invalid topologies
FirePoly <- terra::makeValid(FirePoly)
table(terra::is.valid(FirePoly)) ## checking the validity of geometery
## validated
gc()

WesternForests_15km <- terra::buffer(WesternForests, 15000) ## buffering by 1km
WesternForests_15km <- terra::aggregate(WesternForests_15km) ## dissolving fields, otherwise fires that are partially within 2 ecoregions are not accounted for
rownumbers <- relate(FirePoly, WesternForests_15km, "within", pairs = T)
# WF_Fires <- terra::intersect(FirePoly,WesternForests_15km) ## this pulls out the ecoregion information in addition to cropping
WF_Fires <- FirePoly[c(rownumbers[,1]),]
vec <- values(WF_Fires)
WF_Fires <- terra::unique(WF_Fires)
gc();rm(FirePoly)

vec <- values(WF_Fires) ## currently just over 1k fires
colnames(vec)
table(vec$Ig_Date) ## all years represented
table(is.na(vec$BurnBndAc)) ## no NA values

plot(WF_Fires, add = TRUE, col="red")

count_vertices <- function(v) {
  n <- nrow(v)
  vertex_counts <- numeric(n)
  
  for(i in 1:n) {
    coords <- crds(v[i])
    # Subtract 1 because last point repeats first point
    vertex_counts[i] <- nrow(coords) - 1
    progress <- i/n*100
    if (progress %% 10 == 0) {
      print(progress)
    }
  }
  
  return(vertex_counts)
}

# Remove triangles (3 vertices)
n_vertices <- count_vertices(WF_Fires)
WF_Fires <- WF_Fires[n_vertices > 3,]
table(WF_Fires$Incid_Name)
vec <- values(WF_Fires) ## no real change
gc()

writeVector(WF_Fires,"./mtbs_perimeter_data/WF_Fires.shp", overwrite = TRUE)
rm(vec);rm(i);rm(n_vertices);rm(count_vertices);rm(rownumbers)
gc()


#### Adding Raster files from LandFire (1999-2024) ####
temp <- list.files(path = "./LandFire TIFs", pattern="*.tif") ## creating a vector that has all the files in the working directory with .tif extensions (i.e., treatment history data)
temp <- temp[1:26] ## keeping the first 26 variables (only relevant if rerunning code, as we save files to this location with the .tif extension that are not needed for this part of the code)

temp.list <- list()
for(i in 1:length(temp)){
  path <- paste("./LandFire TIFs/", temp[i], sep = "")
  temp.list[[i]] <- terra::rast(path)
} ## loading in the raster files within the LandFire TIFS folder
rm(i);rm(path);rm(temp) ## keeping the global environment clean

dist_stack <- Reduce(c, temp.list) ## combining the SpatRasters within the list into a single object
rm(temp.list)

## instead of cropping to western Forests, I can crop to a buffered version of the fire polygon data, much less data
Fires_Buff <- terra::aggregate(terra::buffer(WF_Fires, 60)) ## buffering by 60m and dissolved layers

dist_stack <- terra::crop(dist_stack, Fires_Buff) ## cropping raw data (CONUS coverage at 30 m) to the fires of interest
WF_dist <- terra::mask(dist_stack, Fires_Buff) ## breaking into two lines because doing crop and mask in the same line is too memory intensive
gc() ## freeing unused memory 
plot(WF_dist[[1]]) ## plotting the first layer of the stack to make sure it worked

rm(dist_stack) ## removing pre-cropped/pre-masked data
writeRaster(WF_dist, "./LandFire TIFs/WF_dist.tif", overwrite = TRUE) ## writing raster stack as a .tif file for easy access in the remainder of the code
rm(WF_dist);gc()


#### Adding NIFC Fire Line Data (2018-2024) ####  
temp <- list.files(path = "./NIFC Lines/",pattern="*.shp") ## creating a vector that has all the files in the working directory with .shp extensions (i.e., NIFC Fire Line Data)
temp <- temp[1:7] ## keeping only the fire line data from the years of interest that have not been processed 

for(i in 1:length(temp)) {
  path <- paste("./NIFC Lines/", temp[i], sep = "") ## specifying the relative pathway for assign
  assign(temp[i], terra::vect(path))
} ## loading in the shapefiles of interest
rm(i);rm(path) ## keeping the global enviroment clean
gc()

## assigning years to the individual shapefiles to keep track of after the rbind - too memory intensive to automate these steps
EventLine2018.shp$year <- 2018
EventLine2019.shp$year <- 2019
EventLine2020.shp$year <- 2020
EventLine2021.shp$year <- 2021
EventLine2022.shp$year <- 2022
EventLine2023.shp$year <- 2023
EventLine2024.shp$year <- 2024

stacked_FL <- rbind(EventLine2018.shp,EventLine2019.shp,EventLine2020.shp,EventLine2021.shp,EventLine2022.shp,EventLine2023.shp,EventLine2024.shp) ## rbind appears to be less memory intensive than do.call(rbind, list)
rm(list = temp);rm(temp)
gc()

## instead of cropping to western Forests, I can crop to a buffered fire polygon dataset
WF_FLs <- terra::crop(stacked_FL,Fires_Buff) ## this is can take over 12 hours in R. It is quicker to do in ArcPro
gc();rm(stacked_FL)
WF_FLs <- terra::unique(WF_FLs)
gc()

vec <- values(WF_FLs)
colnames(vec) ## Did a double check with all years, confirmed that Feature Cat is only needed column to query
table(vec$DeleteThis)
table(unique(vec$year))
table(vec$FeatureCat)
rm(vec);gc()

WF_FLs <- WF_FLs[WF_FLs$DeleteThis == "No" &
                   WF_FLs$FeatureCat == "Completed Burnout" |
                   WF_FLs$FeatureCat == "Completed Dozer Line" |
                   WF_FLs$FeatureCat == "Completed Fuel Break" | 
                   WF_FLs$FeatureCat == "Completed Hand Line" |
                   WF_FLs$FeatureCat == "Completed Line" |
                   WF_FLs$FeatureCat == "Completed Mixed Construction Line" |
                   WF_FLs$FeatureCat == "Completed Plow Line" |
                   WF_FLs$FeatureCat == "Completed Road as Line" |
                   WF_FLs$FeatureCat == "Contained Line",]
gc()
vec <- values(WF_FLs)
table(vec$FeatureCat)
rm(vec)
gc()

writeVector(WF_FLs, "./NIFC Lines/WF_FLs.shp", overwrite = TRUE)
rm(WF_FLs);gc()
rm(list=ls())

#### Extracting Treatment History for Fire Lines ####
W_Fires <- vect("./mtbs_perimeter_data/WF_Fires.shp")
W_FLs <- vect("./NIFC Lines/WF_FLs.shp") ## still the subset from 10km buffed areas
W_dist <- rast("./LandFire TIFs/WF_dist.tif")
gc()

vec <- seq(2018,2024, by = 1) ## change to reflect data range
i <- 4 ## specifying i because for loop was too memory intensive
## a bit clunky but necessary for memory

Engaged_Lines <- NA
Perimeter_Treatment <- NA # Treatment_Boundary
BurnedOver_Treatment <- NA # Inside_Treatment

# Pre-allocate list to store results - more necessary if for loop is possible
Engaged_Lines_list <- vector("list", length(vec) * 2)
Perimeter_Treatment_list <- vector("list", length(vec))
BurnedOver_Treatment_list <- vector("list", length(vec))

# for(i in seq_along(vec)){ ## if you are using a computer with > 64 GB of RAM then this for loop will probably work
year_i <- vec[i]

# Filter for current year of loop (or object i if done manually)
W_Fires_year <- W_Fires[W_Fires$Ig_Date == year_i, ] ## getting the year of interest, too much data to do it all at once
W_FLs_year <- W_FLs[W_FLs$year == year_i, ]

# Buffer operations to get EH and EF areas
W_Fires_add60 <- buffer(W_Fires_year, 60) ## outer most area of fire perimeter
W_Fires_EF <- buffer(W_Fires_year, -60) ## this is the EF area
W_Fires_EH <- erase(W_Fires_add60, W_Fires_EF) ## creates a ring w/ 60 m on either end of the fire perimter

# Extract treatment history for fire perimeter
unique_names <- unique(W_Fires_EH$Incid_Name)
W_Fires_EH_sf <- tidyterra::as_sf(W_Fires_EH) ## need to make into sf objects first
W_Fires_EH_sf <- sf::as_Spatial(W_Fires_EH_sf$geometry) ## replace w/ sf_object if given trouble
tmp_list <- exact_extract(W_dist, W_Fires_EH_sf, include_cell = TRUE, progress = TRUE)
tmp <- do.call(rbind, lapply(seq_along(tmp_list), function(j) {
  df <- tmp_list[[j]]
  df$ID <- j
  return(df)
})) # This combines the extracted list into a single dataframe, with the ID of each cell j
tmp$fire.names <- unique_names[tmp$ID] ## matching the fire names based on the ID 
tmp$year <- year_i ## adding the year
Perimeter_Treatment_list[[i]] <- tmp ## more necessary when using for loop to store multiple extracted years. For now it is legacy code that works
gc()

# Extract treatment history for burned over area
unique_names <- unique(W_Fires_EF$Incid_Name) ## pulling out unique incident names
W_Fires_EF_sf<- tidyterra::as_sf(W_Fires_EF) ## need to make into sf objects for exact_extract
ID_match <- sf::st_is_empty(W_Fires_EF_sf$geometry) ## noting which geometeries are empty to avoid with fire name matching
W_Fires_EF_sf <- W_Fires_EF_sf[!sf::st_is_empty(W_Fires_EF_sf$geometry),] ## removing empty geometeries (possible because of the way buffering was done)
W_Fires_EF_sf <- sf::as_Spatial(W_Fires_EF_sf$geometry)
tmp_list <- exact_extract(W_dist, W_Fires_EF_sf, include_cell = TRUE, progress = TRUE) ## using exact_extract for computational efficiency
tmp <- do.call(rbind, lapply(seq_along(tmp_list), function(j) {
  df <- tmp_list[[j]]
  df$ID <- j
  return(df)
})) ## do.call that compresses a list of each extracted fire into a dataframe
tmp$fire.names <- unique_names[ID_match == FALSE][tmp$ID] ## need to include ID match because some empty geometeries were removed
tmp$year <- year_i ## adding year
BurnedOver_Treatment_list[[i]] <- tmp ##more necessary when using for loop to store multiple extracted years. For now it is legacy code that works
gc()

## Determining which of the FLs were engaged
# Intersect FLs with engaged held areas or engaged failed areas
terraOptions(progress = 1)
W_FLs_EH <- terra::intersect(W_FLs_year, W_Fires_EH)
W_FLs_EF <- terra::intersect(W_FLs_year, W_Fires_EF)
terraOptions(progress = 0)

# make lines into small polygons, needed for exact_extract
W_FLs_EH <- buffer(W_FLs_EH, 1)
W_FLs_EF <- buffer(W_FLs_EF, 1)

# Extract with xy coordinates for fire lines
# exactextractr returns coverage fraction by default, use include_xy for coordinates
W_FLs_EH <- tidyterra::as_sf(W_FLs_EH) ## need to make into sf objects first
W_FLs_EH <- sf::as_Spatial(W_FLs_EH$geometry)
extracted_FLs_EH_list <- exact_extract(W_dist, W_FLs_EH, include_xy = TRUE, progress = TRUE)

## repeat for EF
W_FLs_EF <- tidyterra::as_sf(W_FLs_EF) ## need to make into sf objects first
W_FLs_EF <- sf::as_Spatial(W_FLs_EF$geometry)
extracted_FLs_EF_list <- exact_extract(W_dist, W_FLs_EF, include_xy = TRUE, progress = TRUE)

# Combine into dataframes
extracted_FLs_EH <- do.call(rbind, extracted_FLs_EH_list)
extracted_FLs_EF <- do.call(rbind, extracted_FLs_EF_list)

# Get unique values
EF <- unique(extracted_FLs_EF)
EH <- unique(extracted_FLs_EH)

EF$Stat <- "EF"
EF$year <- year_i
EH$Stat <- "EH"
EH$year <- year_i

Engaged_Lines_list[[2*i - 1]] <- EF
Engaged_Lines_list[[2*i]] <- EH

print(year_i)

gc()

## remove excess layers
rm(EF);rm(EH);rm(extracted_FLs_EF);rm(extracted_FLs_EH);rm(extracted_FLs_EF_list);rm(extracted_FLs_EH_list)
rm(tmp);rm(tmp_list);rm(W_dist);rm(W_Fires);rm(W_Fires_add60);rm(W_Fires_EF);rm(W_Fires_EF_sf)
rm(W_Fires_EH);rm(W_Fires_EH_sf);rm(W_FLs);rm(W_FLs_EF);rm(W_FLs_EH);rm(W_FLs_year)
rm(i);rm(ID_match);rm(unique_names);rm(vec)
gc()

Perimeter_Treatment <- do.call(rbind, Perimeter_Treatment_list)
rm(Perimeter_Treatment_list);gc()
BurnedOver_Treatment <- do.call(rbind, BurnedOver_Treatment_list)
rm(BurnedOver_Treatment_list);gc()
Engaged_Lines <- do.call(rbind, Engaged_Lines_list)
Engaged_Lines$ID <- c(1:nrow(Engaged_Lines))
# rm(Engaged_Lines_list);gc() # can remove after this is sorted!
W_Fires_year <- buffer(W_Fires_year, 60) # buffering to extract fire information from extracted lines
Engaged_Lines_sp <- vect(Engaged_Lines, geom = c("x","y"), crs = crs(W_Fires_year))
Engaged_Lines_sp <- terra::intersect(Engaged_Lines_sp,W_Fires_year)
Engaged_Lines_sp <- values(Engaged_Lines_sp) ## converting values back to df
Engaged_Lines_sp$x <- Engaged_Lines$x[match(Engaged_Lines_sp$ID, Engaged_Lines$ID)] ## getting the XY information for those that have fire information 
Engaged_Lines_sp$y <- Engaged_Lines$y[match(Engaged_Lines_sp$ID, Engaged_Lines$ID)]

## LandCover years '15 and '16 need to be converted to integers prior to saving
Engaged_Lines_sp$LC15_Dist <- as.integer(Engaged_Lines_sp$LC15_Dist) 
Engaged_Lines_sp$LC16_Dist <- as.integer(Engaged_Lines_sp$LC16_Dist)

rm(Engaged_Lines);rm(W_Fires_year);gc()

table(Perimeter_Treatment$year)
length(unique(Perimeter_Treatment$fire.names))

table(BurnedOver_Treatment$year)
length(unique(BurnedOver_Treatment$fire.names))

table(Engaged_Lines_sp$year)
table(Engaged_Lines_sp$Stat)
length(unique(Engaged_Lines_sp$Incid_Name)) ## fewer fires have fire lines

write.csv(Engaged_Lines_sp, paste0("Engaged_Lines", year_i, ".csv"))
gc()
write.csv(Perimeter_Treatment, paste0("Perimeter_Treatment", year_i, ".csv"))
gc()
write.csv(BurnedOver_Treatment, paste0("BurnedOver_Treatment", year_i, ".csv"))
gc()
rm(list = ls())


#### Start Here Post-Extractions ####
#### Engaged Lines Treatment History Data Cleaning ####
D_csv <- read.csv("./LandFire csvs/LF_total_dist.csv")

EL18 <- read.csv("Engaged_Lines2018.csv")
head(EL18)
EL18$X <- NULL
EL19 <- read.csv("Engaged_Lines2019.csv")
head(EL19)
EL19$X <- NULL
EL20 <- read.csv("Engaged_Lines2020.csv")
head(EL20)
EL20$X <- NULL
EL21 <- read.csv("Engaged_Lines2021.csv")
head(EL21)
EL21$X <- NULL
EL22 <- read.csv("Engaged_Lines2022.csv")
head(EL22)
EL22$X <- NULL
EL23 <- read.csv("Engaged_Lines2023.csv")
head(EL23)
EL23$X <- NULL
EL24 <- read.csv("Engaged_Lines2024.csv")
head(EL24)
EL24$X <- NULL
gc()

head(EL18)
head(EL19)
head(EL20)
head(EL21)
head(EL22)
head(EL23)
head(EL24)

Engaged_Lines <- rbind(EL18,EL19,EL20,EL21,EL22,EL23,EL24)
rm(EL18);rm(EL19);rm(EL20);rm(EL21);rm(EL22);rm(EL23);rm(EL24)

table(D_csv$DIST_TYPE)

### DOUBLE CHECK whether 1999 is at the beginning of the sequence!

head(Engaged_Lines)
colnames(Engaged_Lines) <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","1999","cov_frac","stat","year","ID","Incid_Name","BurnAcre","Ig_Date","x","y")
head(Engaged_Lines)
colnames(Engaged_Lines)[c(26,1:25,34:35,27:29,31:32)]
Engaged_Lines <- Engaged_Lines[,c(26,1:25,34:35,27:29,31:32)]

write.csv(Engaged_Lines, "Engaged_Lines.csv")


vec <- ncol(Engaged_Lines[,c(1:26)]) ## picking out years that were extracted
## use a master csv that matches the year of dist to find the appropriate code for that year.
for(i in 1:vec){
  tmp <- D_csv[D_csv$DIST_YEAR == as.numeric(colnames(Engaged_Lines[i])),]
  Engaged_Lines[,i] <- tmp$DIST_TYPE[match(Engaged_Lines[,i], tmp$VALUE)]
}

Engaged_Lines$history <- apply(Engaged_Lines, 1, function(row) {
  year_cols <- as.numeric(colnames(Engaged_Lines)[1:26])
  valid_cols <- year_cols < as.numeric(row["year"])
  stringr::str_flatten(row[1:26][valid_cols], collapse = ", ", na.rm = TRUE)
})

table(Engaged_Lines$history) ## many many options, will need to make choices to simplify
table(Engaged_Lines$stat)
# EF      EH 
# 2223287  3663156  
nrow(Engaged_Lines[Engaged_Lines$stat == "EF",])/nrow(Engaged_Lines)
## 0.3776962 (proportion of failed lines)

length(unique(Engaged_Lines$history)) ## 2918 unique disturbance histories
Engaged_Lines$trt <- ifelse(grepl("Thinning", Engaged_Lines$history) & grepl("Prescribed", Engaged_Lines$history),
                      "Thinning and Prescribed",
                      ifelse(grepl("Thinning", Engaged_Lines$history) & !grepl("Prescribed", Engaged_Lines$history),
                             "Thinning only",
                             ifelse(grepl("Prescribed", Engaged_Lines$history) & !grepl("Thinning", Engaged_Lines$history),
                                    "Prescribed only",
                                    "Neither")))
table(Engaged_Lines$trt) ## simplified treatment history
Engaged_Lines$prop.rx <- NA
Engaged_Lines$prop.rx[grepl("Prescribed", Engaged_Lines$history)] <- stringr::str_count(Engaged_Lines$history[grepl("Prescribed", Engaged_Lines$history)], "Prescribed")
Engaged_Lines$prop.rx <- (Engaged_Lines$prop.rx/(Engaged_Lines$year-1999))
Engaged_Lines$prop.thin <- NA
Engaged_Lines$prop.thin[grepl("Thinning", Engaged_Lines$history)] <- stringr::str_count(Engaged_Lines$history[grepl("Thinning", Engaged_Lines$history)], "Thinning")
Engaged_Lines$prop.thin <- (Engaged_Lines$prop.thin/(Engaged_Lines$year-1999))

obj <- 1:26
years <- 1999:2024

Engaged_Lines$TS.rx <- apply(Engaged_Lines, 1, function(row) {
  vec <- grepl("Prescribed", row[1:26])
  ifelse(TRUE %in% vec, ifelse(as.numeric(row["year"]) >= years[obj[vec]], as.numeric(row["year"]) - years[obj[vec]], NA), NA)
})
gc()

hist(Engaged_Lines$TS.rx)

gc()
Engaged_Lines$TS.thin <- apply(Engaged_Lines, 1, function(row) {
  vec <- grepl("Thinning", row[1:26])
  ifelse(TRUE %in% vec, ifelse(as.numeric(row["year"]) >= years[obj[vec]], as.numeric(row["year"]) - years[obj[vec]], NA), NA)
})
gc()

hist(Engaged_Lines$TS.thin)

write.csv(Engaged_Lines,"Engaged_Lines_DisturbanceHistory.csv")


#### Fire Perimeter Data ####
D_csv <- read.csv("./LandFire csvs/LF_total_dist.csv")

TB18 <- read.csv("Perimeter_Treatment2018.csv")
TB19 <- read.csv("Perimeter_Treatment2019.csv")
TB20 <- read.csv("Perimeter_Treatment2020.csv")
TB21 <- read.csv("Perimeter_Treatment2021.csv")
TB22 <- read.csv("Perimeter_Treatment2022.csv")
TB23 <- read.csv("Perimeter_Treatment2023.csv")
TB24 <- read.csv("Perimeter_Treatment2024.csv")

head(TB18)
table(TB18$fire.names)
TB18$X <- NULL
head(TB19)
table(TB19$fire.names)
TB19$X <- NULL
head(TB20)
table(TB20$fire.names)
TB20$X <- NULL
head(TB21)
table(TB21$fire.names)
TB21$X <- NULL
head(TB22)
table(TB22$fire.names)
TB22$X <- NULL
head(TB23)
table(TB23$fire.names)
TB23$X <- NULL
head(TB24)
table(TB24$fire.names)
TB24$X <- NULL

gc()
trt <- rbind(TB18,TB19,TB20,TB21,TB22,TB23,TB24)
rm(TB18);rm(TB19);rm(TB20);rm(TB21);rm(TB22);rm(TB23);rm(TB24)
head(trt);gc()

colnames(trt)
trt <- trt[,c(26,1:25,30,31)]
colnames(trt) <- c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","fire.name","year")

vec <- ncol(trt[,c(1:26)]) ## need to change the number of columns
## use a master csv that matches the year of dist to find the appropriate code for that year.

for(i in 1:vec){
  tmp <- D_csv[D_csv$DIST_YEAR == as.numeric(colnames(trt[i])),]
  trt[,i] <- tmp$DIST_TYPE[match(trt[,i], tmp$VALUE)]
}
gc()

trt$history <- apply(trt, 1, function(row) {
  year_cols <- as.numeric(colnames(trt)[1:26])
  valid_cols <- year_cols < as.numeric(row["year"])
  stringr::str_flatten(row[1:26][valid_cols], collapse = ", ", na.rm = TRUE)
})

gc()

length(unique(trt$history)) ## 3367 unique disturbance histories
length(unique(trt$fire.name)) ## 945 unique fires 

trt$trt <- ifelse(grepl("Thinning", trt$history) & grepl("Prescribed", trt$history),
                            "Both",
                            ifelse(grepl("Thinning", trt$history) & !grepl("Prescribed", trt$history),
                                   "Thinning",
                                   ifelse(grepl("Prescribed", trt$history) & !grepl("Thinning", trt$history),
                                          "Prescribed",
                                          "Neither")))
gc()

table(trt$trt)
trt$thin.pa <- stringr::str_count(trt$trt, "Thinning")
trt$n.pa <- stringr::str_count(trt$trt, "Neither")
trt$rx.pa <- stringr::str_count(trt$trt, "Prescribed")
trt$b.pa <- stringr::str_count(trt$trt, "Both")
gc()
fire.perimeter <- aggregate(cbind(thin.pa, rx.pa, b.pa, n.pa) ~ fire.name + year, trt, FUN = sum) 
# apply(fire.perimeter[3:6], 2, sum) ## roughly the same but not identical
gc()
rm(trt);rm(tmp)
write.csv(fire.perimeter, "Perimeter_Treatments.csv")
rm(i);rm(vec);rm(fire.perimeter)

#### Inside Fire Data ####
D_csv <- read.csv("./LandFire csvs/LF_total_dist.csv")

IT18 <- read.csv("BurnedOver_Treatment2018.csv")
colnames(IT18)
trt <- IT18[,c(27,2:26,31,32)]
colnames(trt) <- c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","fire.name","year")
rm(IT18);gc()

vec <- ncol(trt[,c(1:26)]) ## need to change the number of columns
## use a master csv that matches the year of dist to find the appropriate code for that year.

for(i in 1:vec){
  tmp <- D_csv[D_csv$DIST_YEAR == as.numeric(colnames(trt[i])),]
  trt[,i] <- tmp$DIST_TYPE[match(trt[,i], tmp$VALUE)]
}
gc()

trt$history <- apply(trt, 1, function(row) {
  year_cols <- as.numeric(colnames(trt)[1:26])
  valid_cols <- year_cols < as.numeric(row["year"])
  stringr::str_flatten(row[1:26][valid_cols], collapse = ", ", na.rm = TRUE)
})

gc()

length(unique(trt$history))
length(unique(trt$fire.name))

trt$trt <- ifelse(grepl("Thinning", trt$history) & grepl("Prescribed", trt$history),
                  "Both",
                  ifelse(grepl("Thinning", trt$history) & !grepl("Prescribed", trt$history),
                         "Thinning",
                         ifelse(grepl("Prescribed", trt$history) & !grepl("Thinning", trt$history),
                                "Prescribed",
                                "Neither")))
gc()
table(trt$trt)
trt$thin.pa <- stringr::str_count(trt$trt, "Thinning")
trt$n.pa <- stringr::str_count(trt$trt, "Neither")
trt$rx.pa <- stringr::str_count(trt$trt, "Prescribed")
trt$b.pa <- stringr::str_count(trt$trt, "Both")
gc()
IT18_summary <- aggregate(cbind(thin.pa, rx.pa, b.pa, n.pa) ~ fire.name + year, trt, FUN = sum) 
rm(trt)
gc()

IT19 <- read.csv("BurnedOver_Treatment2019.csv")
colnames(IT19)
trt <- IT19[,c(27,2:26,31,32)]
colnames(trt) <- c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","fire.name","year")
rm(IT19);gc()

vec <- ncol(trt[,c(1:26)]) ## need to change the number of columns
## use a master csv that matches the year of dist to find the appropriate code for that year.

for(i in 1:vec){
  tmp <- D_csv[D_csv$DIST_YEAR == as.numeric(colnames(trt[i])),]
  trt[,i] <- tmp$DIST_TYPE[match(trt[,i], tmp$VALUE)]
}
gc()

trt$history <- apply(trt, 1, function(row) {
  year_cols <- as.numeric(colnames(trt)[1:26])
  valid_cols <- year_cols < as.numeric(row["year"])
  stringr::str_flatten(row[1:26][valid_cols], collapse = ", ", na.rm = TRUE)
})

gc()

length(unique(trt$history))
length(unique(trt$fire.name))

trt$trt <- ifelse(grepl("Thinning", trt$history) & grepl("Prescribed", trt$history),
                  "Both",
                  ifelse(grepl("Thinning", trt$history) & !grepl("Prescribed", trt$history),
                         "Thinning",
                         ifelse(grepl("Prescribed", trt$history) & !grepl("Thinning", trt$history),
                                "Prescribed",
                                "Neither")))
gc()
table(trt$trt)
trt$thin.pa <- stringr::str_count(trt$trt, "Thinning")
trt$n.pa <- stringr::str_count(trt$trt, "Neither")
trt$rx.pa <- stringr::str_count(trt$trt, "Prescribed")
trt$b.pa <- stringr::str_count(trt$trt, "Both")
gc()
IT19_summary <- aggregate(cbind(thin.pa, rx.pa, b.pa, n.pa) ~ fire.name + year, trt, FUN = sum) 
rm(trt)
gc()

IT20 <- read.csv("BurnedOver_Treatment2020.csv")
colnames(IT20)
trt <- IT20[,c(27,2:26,31,32)]
colnames(trt) <- c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","fire.name","year")
rm(IT20);gc()

vec <- ncol(trt[,c(1:26)]) ## need to change the number of columns
## use a master csv that matches the year of dist to find the appropriate code for that year.

for(i in 1:vec){
  tmp <- D_csv[D_csv$DIST_YEAR == as.numeric(colnames(trt[i])),]
  trt[,i] <- tmp$DIST_TYPE[match(trt[,i], tmp$VALUE)]
}
gc()

trt$history <- apply(trt, 1, function(row) {
  year_cols <- as.numeric(colnames(trt)[1:26])
  valid_cols <- year_cols < as.numeric(row["year"])
  stringr::str_flatten(row[1:26][valid_cols], collapse = ", ", na.rm = TRUE)
})

gc()

length(unique(trt$history))
length(unique(trt$fire.name))

trt$trt <- ifelse(grepl("Thinning", trt$history) & grepl("Prescribed", trt$history),
                  "Both",
                  ifelse(grepl("Thinning", trt$history) & !grepl("Prescribed", trt$history),
                         "Thinning",
                         ifelse(grepl("Prescribed", trt$history) & !grepl("Thinning", trt$history),
                                "Prescribed",
                                "Neither")))
gc()
table(trt$trt)
trt$thin.pa <- stringr::str_count(trt$trt, "Thinning")
trt$n.pa <- stringr::str_count(trt$trt, "Neither")
trt$rx.pa <- stringr::str_count(trt$trt, "Prescribed")
trt$b.pa <- stringr::str_count(trt$trt, "Both")
gc()
IT20_summary <- aggregate(cbind(thin.pa, rx.pa, b.pa, n.pa) ~ fire.name + year, trt, FUN = sum) 
rm(trt)
gc()

IT21 <- read.csv("BurnedOver_Treatment2021.csv")
colnames(IT21)
trt <- IT21[,c(27,2:26,31,32)]
colnames(trt) <- c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","fire.name","year")
rm(IT21);gc()

vec <- ncol(trt[,c(1:26)]) ## need to change the number of columns
## use a master csv that matches the year of dist to find the appropriate code for that year.

for(i in 1:vec){
  tmp <- D_csv[D_csv$DIST_YEAR == as.numeric(colnames(trt[i])),]
  trt[,i] <- tmp$DIST_TYPE[match(trt[,i], tmp$VALUE)]
}
gc()

trt$history <- apply(trt, 1, function(row) {
  year_cols <- as.numeric(colnames(trt)[1:26])
  valid_cols <- year_cols < as.numeric(row["year"])
  stringr::str_flatten(row[1:26][valid_cols], collapse = ", ", na.rm = TRUE)
})

gc()

length(unique(trt$history))
length(unique(trt$fire.name))

trt$trt <- ifelse(grepl("Thinning", trt$history) & grepl("Prescribed", trt$history),
                  "Both",
                  ifelse(grepl("Thinning", trt$history) & !grepl("Prescribed", trt$history),
                         "Thinning",
                         ifelse(grepl("Prescribed", trt$history) & !grepl("Thinning", trt$history),
                                "Prescribed",
                                "Neither")))
gc()
table(trt$trt)
trt$thin.pa <- stringr::str_count(trt$trt, "Thinning")
trt$n.pa <- stringr::str_count(trt$trt, "Neither")
trt$rx.pa <- stringr::str_count(trt$trt, "Prescribed")
trt$b.pa <- stringr::str_count(trt$trt, "Both")
gc()
IT21_summary <- aggregate(cbind(thin.pa, rx.pa, b.pa, n.pa) ~ fire.name + year, trt, FUN = sum) 
rm(trt)
gc()

IT22 <- read.csv("BurnedOver_Treatment2022.csv")
colnames(IT22)
trt <- IT22[,c(27,2:26,31,32)]
colnames(trt) <- c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","fire.name","year")
rm(IT22);gc()

vec <- ncol(trt[,c(1:26)]) ## need to change the number of columns
## use a master csv that matches the year of dist to find the appropriate code for that year.

for(i in 1:vec){
  tmp <- D_csv[D_csv$DIST_YEAR == as.numeric(colnames(trt[i])),]
  trt[,i] <- tmp$DIST_TYPE[match(trt[,i], tmp$VALUE)]
}
gc()

trt$history <- apply(trt, 1, function(row) {
  year_cols <- as.numeric(colnames(trt)[1:26])
  valid_cols <- year_cols < as.numeric(row["year"])
  stringr::str_flatten(row[1:26][valid_cols], collapse = ", ", na.rm = TRUE)
})

gc()

length(unique(trt$history))
length(unique(trt$fire.name))

trt$trt <- ifelse(grepl("Thinning", trt$history) & grepl("Prescribed", trt$history),
                  "Both",
                  ifelse(grepl("Thinning", trt$history) & !grepl("Prescribed", trt$history),
                         "Thinning",
                         ifelse(grepl("Prescribed", trt$history) & !grepl("Thinning", trt$history),
                                "Prescribed",
                                "Neither")))
gc()
table(trt$trt)
trt$thin.pa <- stringr::str_count(trt$trt, "Thinning")
trt$n.pa <- stringr::str_count(trt$trt, "Neither")
trt$rx.pa <- stringr::str_count(trt$trt, "Prescribed")
trt$b.pa <- stringr::str_count(trt$trt, "Both")
gc()
IT22_summary <- aggregate(cbind(thin.pa, rx.pa, b.pa, n.pa) ~ fire.name + year, trt, FUN = sum) 
rm(trt)
gc()

IT23 <- read.csv("BurnedOver_Treatment2023.csv")
colnames(IT23)
trt <- IT23[,c(27,2:26,31,32)]
colnames(trt) <- c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","fire.name","year")
rm(IT23);gc()

vec <- ncol(trt[,c(1:26)]) ## need to change the number of columns
## use a master csv that matches the year of dist to find the appropriate code for that year.

for(i in 1:vec){
  tmp <- D_csv[D_csv$DIST_YEAR == as.numeric(colnames(trt[i])),]
  trt[,i] <- tmp$DIST_TYPE[match(trt[,i], tmp$VALUE)]
}
gc()

trt$history <- apply(trt, 1, function(row) {
  year_cols <- as.numeric(colnames(trt)[1:26])
  valid_cols <- year_cols < as.numeric(row["year"])
  stringr::str_flatten(row[1:26][valid_cols], collapse = ", ", na.rm = TRUE)
})

gc()

length(unique(trt$history))
length(unique(trt$fire.name))

trt$trt <- ifelse(grepl("Thinning", trt$history) & grepl("Prescribed", trt$history),
                  "Both",
                  ifelse(grepl("Thinning", trt$history) & !grepl("Prescribed", trt$history),
                         "Thinning",
                         ifelse(grepl("Prescribed", trt$history) & !grepl("Thinning", trt$history),
                                "Prescribed",
                                "Neither")))
gc()
table(trt$trt)
trt$thin.pa <- stringr::str_count(trt$trt, "Thinning")
trt$n.pa <- stringr::str_count(trt$trt, "Neither")
trt$rx.pa <- stringr::str_count(trt$trt, "Prescribed")
trt$b.pa <- stringr::str_count(trt$trt, "Both")
gc()
IT23_summary <- aggregate(cbind(thin.pa, rx.pa, b.pa, n.pa) ~ fire.name + year, trt, FUN = sum) 
rm(trt)
gc()

IT24 <- read.csv("BurnedOver_Treatment2024.csv")
colnames(IT24)
trt <- IT24[,c(27,2:26,31,32)]
colnames(trt) <- c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","fire.name","year")
rm(IT24);gc()

vec <- ncol(trt[,c(1:26)]) ## need to change the number of columns
## use a master csv that matches the year of dist to find the appropriate code for that year.

for(i in 1:vec){
  tmp <- D_csv[D_csv$DIST_YEAR == as.numeric(colnames(trt[i])),]
  trt[,i] <- tmp$DIST_TYPE[match(trt[,i], tmp$VALUE)]
}
gc()

trt$history <- apply(trt, 1, function(row) {
  year_cols <- as.numeric(colnames(trt)[1:26])
  valid_cols <- year_cols < as.numeric(row["year"])
  stringr::str_flatten(row[1:26][valid_cols], collapse = ", ", na.rm = TRUE)
})

gc()

length(unique(trt$history))
length(unique(trt$fire.name))

trt$trt <- ifelse(grepl("Thinning", trt$history) & grepl("Prescribed", trt$history),
                  "Both",
                  ifelse(grepl("Thinning", trt$history) & !grepl("Prescribed", trt$history),
                         "Thinning",
                         ifelse(grepl("Prescribed", trt$history) & !grepl("Thinning", trt$history),
                                "Prescribed",
                                "Neither")))
gc()
table(trt$trt)
trt$thin.pa <- stringr::str_count(trt$trt, "Thinning")
trt$n.pa <- stringr::str_count(trt$trt, "Neither")
trt$rx.pa <- stringr::str_count(trt$trt, "Prescribed")
trt$b.pa <- stringr::str_count(trt$trt, "Both")
gc()
IT24_summary <- aggregate(cbind(thin.pa, rx.pa, b.pa, n.pa) ~ fire.name + year, trt, FUN = sum) 
rm(trt)
gc()

Inside_Treatment <- rbind(IT18_summary,IT19_summary,IT20_summary,IT21_summary,IT22_summary,IT23_summary,IT24_summary)
gc()
rm(IT18_summary);rm(IT19_summary);rm(IT20_summary);rm(IT21_summary);rm(IT22_summary);rm(IT23_summary);rm(IT24_summary)
gc()

write.csv(Inside_Treatment, "BurnedOver_Treatments.csv")


#### Fire Perimeter Effects ####
Inside <- read.csv("BurnedOver_Treatments.csv")
Perimeter <- read.csv("Perimeter_Treatments.csv")

head(Inside)
length(unique(Inside$fire.name))
length(unique(Perimeter$fire.name)) ## only one fire difference

Inside$tot <- apply(Inside[,c(4:7)],1,sum)
Inside$thin.pr <- Inside$thin.pa/Inside$tot
Inside$rx.pr <- Inside$rx.pa/Inside$tot
Inside$b.pr <- Inside$b.pa/Inside$tot
Inside$n.pr <- Inside$n.pa/Inside$tot
Inside$fire.name <- tolower(gsub("[[:punct:][:space:]]", "", Inside$fire.name))
Inside$match <- paste(Inside$fire.name, Inside$year, sep = " ")

Perimeter$tot <- apply(Perimeter[,c(4:7)],1,sum)
Perimeter$thin.pr <- Perimeter$thin.pa/Perimeter$tot
Perimeter$rx.pr <- Perimeter$rx.pa/Perimeter$tot
Perimeter$b.pr <- Perimeter$b.pa/Perimeter$tot
Perimeter$n.pr <- Perimeter$n.pa/Perimeter$tot
Perimeter$fire.name <- tolower(gsub("[[:punct:][:space:]]", "", Perimeter$fire.name))
Perimeter$match <- paste(Perimeter$fire.name, Perimeter$year, sep = " ")

Perimeter.pr <- Perimeter[match(Perimeter$match, Inside$match),c(13, 8:12)]
Inside.pr <- Inside[match(Perimeter$match, Inside$match),c(13, 8:12)]

Perimeter.pr <- Perimeter.pr[complete.cases(Perimeter.pr),] ## 997 observations
Inside.pr <- Inside.pr[complete.cases(Inside.pr),] ## 998 observations
Inside.pr <- Inside.pr[Inside.pr$name %in% Perimeter.pr$name,] ## removing the non-overlapping instance

colnames(Perimeter.pr) <- c("name","Perim.tot", "Perim.thin", "Perim.rx", "Perim.b", "Perim.n")
colnames(Inside.pr) <- c("name", "Inside.tot", "Inside.thin", "Inside.rx", "Inside.b", "Inside.n")

FireTrtHist <- cbind(Perimeter.pr,Inside.pr[,c(2:6)])
rm(Inside);rm(Perimeter);rm(Inside.pr);rm(Perimeter.pr)

FireTrtHist$year <- stringr::str_split_fixed(FireTrtHist$name, " ", n = 2)[,2]
FireTrtHist$name <- stringr::str_split_fixed(FireTrtHist$name, " ", n = 2)[,1]
FireTrtHist$tot <- FireTrtHist$Perim.tot + FireTrtHist$Inside.tot
FireTrtHist$thin.odds <- (FireTrtHist$Perim.thin - FireTrtHist$Inside.thin)/(FireTrtHist$Perim.thin + FireTrtHist$Inside.thin)
FireTrtHist$rx.odds <- (FireTrtHist$Perim.rx - FireTrtHist$Inside.rx)/(FireTrtHist$Perim.rx + FireTrtHist$Inside.rx)
FireTrtHist$b.odds <- (FireTrtHist$Perim.b - FireTrtHist$Inside.b)/(FireTrtHist$Perim.b + FireTrtHist$Inside.b)
FireTrtHist$n.odds <- (FireTrtHist$Perim.n - FireTrtHist$Inside.n)/(FireTrtHist$Perim.n + FireTrtHist$Inside.n)

FireTrtHist <- FireTrtHist[,c(1,c(12:17))]
FireTrtHist <- FireTrtHist[order(FireTrtHist$year, FireTrtHist$tot,FireTrtHist$name),]
hist(FireTrtHist$tot)
FireTrtHist$tot <- (FireTrtHist$tot * 900)/10000 ## now in ha
hist(FireTrtHist$tot[FireTrtHist$tot < 10000])

## adding ecoregion as a factor to color code things
W_Fires <- vect("./mtbs_perimeter_data/WF_Fires.shp")
W_Fires$Incid_Name <- tolower(gsub("[[:punct:][:space:]]", "", W_Fires$Incid_Name))
temp <- list.files(path = "./Geographic Subsets/Ecoregions", pattern="*.shp")

for(i in 1:length(temp)){
  path <- paste("./Geographic Subsets/Ecoregions/", temp[i], sep = "")
  assign(temp[i], terra::vect(path))
} ## loading in the shapefiles I want
rm(i);rm(path)

obj.names <- gsub(".shp", "", temp)
FireTrtHist$ecoregion <- NA

for(i in 1:length(temp)){
  X <- get(temp[i])
  # X <- terra::buffer(X, 15000) ## buffering the ecoregions, causes some problems with overlaps
  X_Fires <- crop(W_Fires,X)
  X_Fires <- values(X_Fires)
  FireTrtHist$ecoregion[FireTrtHist$name %in% X_Fires$Incid_Name] <- obj.names[i]
}
rm(list = temp)
rm(temp);rm(X);rm(X_Fires);rm(i);rm(W_Fires)

Na_Fires <- FireTrtHist[is.na(FireTrtHist$ecoregion),] ## these are smaller fires that are w/i 15km of ecoregion boundary but not within the 'core' area

FireTrtHist <- FireTrtHist[complete.cases(FireTrtHist$ecoregion),]
FireTrtHist$ecoregion <- as.factor(FireTrtHist$ecoregion)
levels(FireTrtHist$ecoregion)

smallfires <- FireTrtHist[FireTrtHist$tot < 10000,]
megafires <- FireTrtHist[FireTrtHist$tot >= 10000,]

se <- function(x, na.rm = FALSE){sd(x, na.rm = na.rm)/sqrt(length(!is.na(x)))} ## creating a function for standard error

length(unique(FireTrtHist$ecoregion))
## small fires
df <- data.frame(per.eff = c(smallfires$thin.odds,smallfires$rx.odds,smallfires$b.odds,smallfires$n.odds),
                 trt = c(rep("thin", nrow(smallfires)), rep("rx", nrow(smallfires)), rep("b", nrow(smallfires)), rep("n",nrow(smallfires))),
                 ecoregion = rep(smallfires$ecoregion, 4))

pal1 <- turbo(12, alpha = 0.2)
pal2 <- turbo(12, alpha = 1)

par(mfrow = c(2,2),oma = c(3, 0, 0, 0))

## Thinning
plot(x = c(0:14),
     ylim = c(-1.1,1.1),
     las = 1,
     main = "Thinning",
     cex.axis = 1.5,
     ylab = "",
     type = "n",
     xaxt = "n",
     xlab = "") ## ecoregion
text(x = 1:length(levels(FireTrtHist$ecoregion)),
     y = par("usr")[3] - 0.45,
     labels = levels(FireTrtHist$ecoregion),
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
abline(h = 0, lty = 2)
for(i in 1:length(levels(FireTrtHist$ecoregion))){
  points(x = jitter(rep(i, length(df$per.eff[df$trt == "thin" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]])), factor = 1.2),
         y = df$per.eff[df$trt == "thin" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]],
         col = pal1[i],
         pch = 16)
  points(x = i,
         y = mean(df$per.eff[df$trt == "thin" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE),
         col = pal2[i],
         pch = 16)
  segments(y0 = (mean(df$per.eff[df$trt == "thin" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "thin" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x0 = i, 
           y1 = (mean(df$per.eff[df$trt == "thin" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "thin" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x1 = i, 
           col = pal2[i],
           lwd = 1.5)
}
text(x = 14,
     y = par("usr")[3] - 0.45,
     labels = "total",
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
points(x = jitter(rep(14, length(df$per.eff[df$trt == "thin"])), factor = 1.2),
       y = df$per.eff[df$trt == "thin"],
       col = rgb(0,0,0, alpha = 0.2),
       pch = 16)
points(x = 14,
       y = mean(df$per.eff[df$trt == "thin"], na.rm = TRUE),
       col = "black",
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "thin"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "thin"], na.rm = TRUE)), x0 = 14, 
         y1 = (mean(df$per.eff[df$trt == "thin"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "thin"], na.rm = TRUE)), x1 = 14, 
         col = "black",
         lwd = 1.5)

## Rx Fire
plot(x = c(0:14),
     ylim = c(-1.1,1.1),
     las = 1,
     main = "Rx Fire",
     cex.axis = 1.5,
     ylab = "",
     type = "n",
     xaxt = "n",
     xlab = "") ## ecoregion
text(x = 1:length(levels(FireTrtHist$ecoregion)),
     y = par("usr")[3] - 0.45,
     labels = levels(FireTrtHist$ecoregion),
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
abline(h = 0, lty = 2)
for(i in 1:length(levels(FireTrtHist$ecoregion))){
  points(x = jitter(rep(i, length(df$per.eff[df$trt == "rx" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]])), factor = 1.2),
         y = df$per.eff[df$trt == "rx" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]],
         col = pal1[i],
         pch = 16)
  points(x = i,
         y = mean(df$per.eff[df$trt == "rx" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE),
         col = pal2[i],
         pch = 16)
  segments(y0 = (mean(df$per.eff[df$trt == "rx" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "rx" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x0 = i, 
           y1 = (mean(df$per.eff[df$trt == "rx" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "rx" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x1 = i, 
           col = pal2[i],
           lwd = 1.5)
}
text(x = 14,
     y = par("usr")[3] - 0.45,
     labels = "total",
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
points(x = jitter(rep(14, length(df$per.eff[df$trt == "rx"])), factor = 1.2),
       y = df$per.eff[df$trt == "rx"],
       col = rgb(0,0,0, alpha = 0.2),
       pch = 16)
points(x = 14,
       y = mean(df$per.eff[df$trt == "rx"], na.rm = TRUE),
       col = "black",
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "rx"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "rx"], na.rm = TRUE)), x0 = 14, 
         y1 = (mean(df$per.eff[df$trt == "rx"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "rx"], na.rm = TRUE)), x1 = 14, 
         col = "black",
         lwd = 1.5)

## Thin and Rx Fire
plot(x = c(0:14),
     ylim = c(-1.1,1.1),
     las = 1,
     main = "Thin + Rx",
     cex.axis = 1.5,
     ylab = "",
     type = "n",
     xaxt = "n",
     xlab = "") ## ecoregion
text(x = 1:length(levels(FireTrtHist$ecoregion)),
     y = par("usr")[3] - 0.45,
     labels = levels(FireTrtHist$ecoregion),
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
abline(h = 0, lty = 2)
for(i in 1:length(levels(FireTrtHist$ecoregion))){
  points(x = jitter(rep(i, length(df$per.eff[df$trt == "b" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]])), factor = 1.2),
         y = df$per.eff[df$trt == "b" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]],
         col = pal1[i],
         pch = 16)
  points(x = i,
         y = mean(df$per.eff[df$trt == "b" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE),
         col = pal2[i],
         pch = 16)
  segments(y0 = (mean(df$per.eff[df$trt == "b" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "b" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x0 = i, 
           y1 = (mean(df$per.eff[df$trt == "b" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "b" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x1 = i, 
           col = pal2[i],
           lwd = 1.5)
}
text(x = 14,
     y = par("usr")[3] - 0.45,
     labels = "total",
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
points(x = jitter(rep(14, length(df$per.eff[df$trt == "b"])), factor = 1.2),
       y = df$per.eff[df$trt == "b"],
       col = rgb(0,0,0, alpha = 0.2),
       pch = 16)
points(x = 14,
       y = mean(df$per.eff[df$trt == "b"], na.rm = TRUE),
       col = "black",
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "b"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "b"], na.rm = TRUE)), x0 = 14, 
         y1 = (mean(df$per.eff[df$trt == "b"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "b"], na.rm = TRUE)), x1 = 14, 
         col = "black",
         lwd = 1.5)

## No Treatments
plot(x = c(0:14),
     ylim = c(-1.1,1.1),
     las = 1,
     main = "No Treatments",
     cex.axis = 1.5,
     ylab = "",
     type = "n",
     xaxt = "n",
     xlab = "") ## ecoregion
text(x = 1:length(levels(FireTrtHist$ecoregion)),
     y = par("usr")[3] - 0.45,
     labels = levels(FireTrtHist$ecoregion),
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
abline(h = 0, lty = 2)
for(i in 1:length(levels(FireTrtHist$ecoregion))){
  points(x = jitter(rep(i, length(df$per.eff[df$trt == "n" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]])), factor = 1.2),
         y = df$per.eff[df$trt == "n" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]],
         col = pal1[i],
         pch = 16)
  points(x = i,
         y = mean(df$per.eff[df$trt == "n" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE),
         col = pal2[i],
         pch = 16)
  segments(y0 = (mean(df$per.eff[df$trt == "n" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "n" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x0 = i, 
           y1 = (mean(df$per.eff[df$trt == "n" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "n" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x1 = i, 
           col = pal2[i],
           lwd = 1.5)
}
text(x = 14,
     y = par("usr")[3] - 0.45,
     labels = "total",
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
points(x = jitter(rep(14, length(df$per.eff[df$trt == "n"])), factor = 1.2),
       y = df$per.eff[df$trt == "n"],
       col = rgb(0,0,0, alpha = 0.2),
       pch = 16)
points(x = 14,
       y = mean(df$per.eff[df$trt == "n"], na.rm = TRUE),
       col = "black",
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "n"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "n"], na.rm = TRUE)), x0 = 14, 
         y1 = (mean(df$per.eff[df$trt == "n"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "n"], na.rm = TRUE)), x1 = 14, 
         col = "black",
         lwd = 1.5)


## summary stats small fires
a1 <- aov(per.eff ~ trt, data = df)
summary(a1)
TukeyHSD(a1)

a2 <- aov(per.eff ~ trt + ecoregion, data = df)
summary(a2)
TukeyHSD(a2)

## west wide
aggregate(per.eff ~ trt, data = df, mean) 
aggregate(per.eff ~ trt, data = df, se) 

## broken down by ecoregion
aggregate(per.eff ~ trt + ecoregion, data = df, mean) 
aggregate(per.eff ~ trt + ecoregion, data = df, se) 


## large fires
df <- data.frame(per.eff = c(megafires$thin.odds,megafires$rx.odds,megafires$b.odds,megafires$n.odds),
                 trt = c(rep("thin", nrow(megafires)), rep("rx", nrow(megafires)), rep("b", nrow(megafires)), rep("n",nrow(megafires))),
                 ecoregion = rep(megafires$ecoregion, 4))

## Thinning
plot(x = c(0:14),
     ylim = c(-1.1,1.1),
     las = 1,
     main = "Thinning",
     cex.axis = 1.5,
     ylab = "",
     type = "n",
     xaxt = "n",
     xlab = "") ## ecoregion
text(x = 1:length(levels(FireTrtHist$ecoregion)),
     y = par("usr")[3] - 0.45,
     labels = levels(FireTrtHist$ecoregion),
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
abline(h = 0, lty = 2)
for(i in 1:length(levels(FireTrtHist$ecoregion))){
  points(x = jitter(rep(i, length(df$per.eff[df$trt == "thin" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]])), factor = 1.2),
         y = df$per.eff[df$trt == "thin" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]],
         col = pal1[i],
         pch = 16)
  points(x = i,
         y = mean(df$per.eff[df$trt == "thin" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE),
         col = pal2[i],
         pch = 16)
  segments(y0 = (mean(df$per.eff[df$trt == "thin" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "thin" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x0 = i, 
           y1 = (mean(df$per.eff[df$trt == "thin" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "thin" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x1 = i, 
           col = pal2[i],
           lwd = 1.5)
}
text(x = 14,
     y = par("usr")[3] - 0.45,
     labels = "total",
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
points(x = jitter(rep(14, length(df$per.eff[df$trt == "thin"])), factor = 1.2),
       y = df$per.eff[df$trt == "thin"],
       col = rgb(0,0,0, alpha = 0.2),
       pch = 16)
points(x = 14,
       y = mean(df$per.eff[df$trt == "thin"], na.rm = TRUE),
       col = "black",
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "thin"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "thin"], na.rm = TRUE)), x0 = 14, 
         y1 = (mean(df$per.eff[df$trt == "thin"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "thin"], na.rm = TRUE)), x1 = 14, 
         col = "black",
         lwd = 1.5)

## Rx Fire
plot(x = c(0:14),
     ylim = c(-1.1,1.1),
     las = 1,
     main = "Rx Fire",
     cex.axis = 1.5,
     ylab = "",
     type = "n",
     xaxt = "n",
     xlab = "") ## ecoregion
text(x = 1:length(levels(FireTrtHist$ecoregion)),
     y = par("usr")[3] - 0.45,
     labels = levels(FireTrtHist$ecoregion),
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
abline(h = 0, lty = 2)
for(i in 1:length(levels(FireTrtHist$ecoregion))){
  points(x = jitter(rep(i, length(df$per.eff[df$trt == "rx" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]])), factor = 1.2),
         y = df$per.eff[df$trt == "rx" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]],
         col = pal1[i],
         pch = 16)
  points(x = i,
         y = mean(df$per.eff[df$trt == "rx" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE),
         col = pal2[i],
         pch = 16)
  segments(y0 = (mean(df$per.eff[df$trt == "rx" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "rx" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x0 = i, 
           y1 = (mean(df$per.eff[df$trt == "rx" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "rx" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x1 = i, 
           col = pal2[i],
           lwd = 1.5)
}
text(x = 14,
     y = par("usr")[3] - 0.45,
     labels = "total",
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
points(x = jitter(rep(14, length(df$per.eff[df$trt == "rx"])), factor = 1.2),
       y = df$per.eff[df$trt == "rx"],
       col = rgb(0,0,0, alpha = 0.2),
       pch = 16)
points(x = 14,
       y = mean(df$per.eff[df$trt == "rx"], na.rm = TRUE),
       col = "black",
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "rx"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "rx"], na.rm = TRUE)), x0 = 14, 
         y1 = (mean(df$per.eff[df$trt == "rx"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "rx"], na.rm = TRUE)), x1 = 14, 
         col = "black",
         lwd = 1.5)

## Thin and Rx Fire
plot(x = c(0:14),
     ylim = c(-1.1,1.1),
     las = 1,
     main = "Thin + Rx",
     cex.axis = 1.5,
     ylab = "",
     type = "n",
     xaxt = "n",
     xlab = "") ## ecoregion
text(x = 1:length(levels(FireTrtHist$ecoregion)),
     y = par("usr")[3] - 0.45,
     labels = levels(FireTrtHist$ecoregion),
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
abline(h = 0, lty = 2)
for(i in 1:length(levels(FireTrtHist$ecoregion))){
  points(x = jitter(rep(i, length(df$per.eff[df$trt == "b" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]])), factor = 1.2),
         y = df$per.eff[df$trt == "b" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]],
         col = pal1[i],
         pch = 16)
  points(x = i,
         y = mean(df$per.eff[df$trt == "b" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE),
         col = pal2[i],
         pch = 16)
  segments(y0 = (mean(df$per.eff[df$trt == "b" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "b" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x0 = i, 
           y1 = (mean(df$per.eff[df$trt == "b" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "b" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x1 = i, 
           col = pal2[i],
           lwd = 1.5)
}
text(x = 14,
     y = par("usr")[3] - 0.45,
     labels = "total",
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
points(x = jitter(rep(14, length(df$per.eff[df$trt == "b"])), factor = 1.2),
       y = df$per.eff[df$trt == "b"],
       col = rgb(0,0,0, alpha = 0.2),
       pch = 16)
points(x = 14,
       y = mean(df$per.eff[df$trt == "b"], na.rm = TRUE),
       col = "black",
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "b"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "b"], na.rm = TRUE)), x0 = 14, 
         y1 = (mean(df$per.eff[df$trt == "b"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "b"], na.rm = TRUE)), x1 = 14, 
         col = "black",
         lwd = 1.5)

## No Treatments
plot(x = c(0:14),
     ylim = c(-1.1,1.1),
     las = 1,
     main = "No Treatments",
     cex.axis = 1.5,
     ylab = "",
     type = "n",
     xaxt = "n",
     xlab = "") ## ecoregion
text(x = 1:length(levels(FireTrtHist$ecoregion)),
     y = par("usr")[3] - 0.45,
     labels = levels(FireTrtHist$ecoregion),
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
abline(h = 0, lty = 2)
for(i in 1:length(levels(FireTrtHist$ecoregion))){
  points(x = jitter(rep(i, length(df$per.eff[df$trt == "n" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]])), factor = 1.2),
         y = df$per.eff[df$trt == "n" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]],
         col = pal1[i],
         pch = 16)
  points(x = i,
         y = mean(df$per.eff[df$trt == "n" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE),
         col = pal2[i],
         pch = 16)
  segments(y0 = (mean(df$per.eff[df$trt == "n" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "n" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x0 = i, 
           y1 = (mean(df$per.eff[df$trt == "n" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "n" & df$ecoregion == levels(FireTrtHist$ecoregion)[i]], na.rm = TRUE)), x1 = i, 
           col = pal2[i],
           lwd = 1.5)
}
text(x = 14,
     y = par("usr")[3] - 0.45,
     labels = "total",
     xpd = NA,
     srt = 35,      ## Rotate the labels by 35 degrees.
     cex = 1.2)
points(x = jitter(rep(14, length(df$per.eff[df$trt == "n"])), factor = 1.2),
       y = df$per.eff[df$trt == "n"],
       col = rgb(0,0,0, alpha = 0.2),
       pch = 16)
points(x = 14,
       y = mean(df$per.eff[df$trt == "n"], na.rm = TRUE),
       col = "black",
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "n"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "n"], na.rm = TRUE)), x0 = 14, 
         y1 = (mean(df$per.eff[df$trt == "n"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "n"], na.rm = TRUE)), x1 = 14, 
         col = "black",
         lwd = 1.5)


## summary stats large fires
a1 <- aov(per.eff ~ trt, data = df)
summary(a1)
TukeyHSD(a1)

a2 <- aov(per.eff ~ trt + ecoregion, data = df)
summary(a2)
TukeyHSD(a2)

## west wide
aggregate(per.eff ~ trt, data = df, mean) 
aggregate(per.eff ~ trt, data = df, se) 

## broken down by ecoregion
aggregate(per.eff ~ trt + ecoregion, data = df, mean) 
aggregate(per.eff ~ trt + ecoregion, data = df, se) 

rm(list = ls())

#### Random Forest - Western USA ####
Engaged_Lines <- read.csv("Engaged_Lines_DisturbanceHistory.csv")
colnames(Engaged_Lines)[c(31,32,33,34,36:40,28,29)]
Engaged_Lines <- Engaged_Lines[,c(31,32,33,34,36:40,28,29)]
head(Engaged_Lines)

str(Engaged_Lines)
Engaged_Lines$stat <- as.factor(Engaged_Lines$stat)
Engaged_Lines$year <- as.factor(Engaged_Lines$year)
Engaged_Lines$trt <- as.factor(Engaged_Lines$trt)

Engaged_Lines$prop.rx[is.na(Engaged_Lines$prop.rx)] <- 0
Engaged_Lines$prop.thin[is.na(Engaged_Lines$prop.thin)] <- 0
gc()

max(Engaged_Lines$TS.rx,na.rm = TRUE)
max(Engaged_Lines$TS.thin,na.rm = TRUE)
Engaged_Lines$TS.rx[is.na(Engaged_Lines$TS.rx)] <- 30 ## trying to get the max year just outside bounds
Engaged_Lines$TS.thin[is.na(Engaged_Lines$TS.thin)] <- 30

backup_dat <- Engaged_Lines
Engaged_Lines <- Engaged_Lines[,c(1,2,5:11)] ## removing incid name and burn acre (seeing if this runs)

## Random Forest - western spatial scale
n <- 100 # number of iterations
num_eigenvectors <- 30 # number of eigenvectors

prop.rx.x <- matrix(data = NA, nrow = n, ncol = 51)
prop.rx.y <- matrix(data = NA, nrow = n, ncol = 51)

prop.thin.x <- matrix(data = NA, nrow = n, ncol = 51)
prop.thin.y <- matrix(data = NA, nrow = n, ncol = 51)

TS.rx.x <- matrix(data = NA, nrow = n, ncol = 51)
TS.rx.y <- matrix(data = NA, nrow = n, ncol = 51)

TS.thin.x <- matrix(data = NA, nrow = n, ncol = 51)
TS.thin.y <- matrix(data = NA, nrow = n, ncol = 51)

rf1.res <- matrix(data = NA, nrow = n, ncol = 10000)
rf2.res <- matrix(data = NA, nrow = n, ncol = 10000)

y_hats1 <- matrix(data = NA, nrow = n, ncol = 10000)
y_hats1.diff <- NA
y_hats2 <- matrix(data = NA, nrow = n, ncol = 10000)
y_hats2.diff <- NA

varImp1.summary <- matrix(data = NA, nrow = 31, ncol = n)
varImp1.names <- matrix(data = NA, nrow = 31, ncol = n)
varImp2.summary <- matrix(data = NA, nrow = 5, ncol = n)
varImp2.names <- matrix(data = NA, nrow = 5, ncol = n)

balance1 <- NA
balance2 <- NA
error1 <- matrix(data = NA, nrow = n, ncol = 500) ## ncol = ntree
error2 <- matrix(data = NA, nrow = n, ncol = 500) ## ncol = ntree
r2 <- matrix(data = NA, nrow = n, ncol = 500)

AUC.val1 <- NA
AUC.val2 <- NA

r <- rast("./LandFire TIFs/WF_dist.tif")
blank <- rast(ext(r), resolution=100, vals=NA) ## gonna expand this
crs(blank) <- crs(r)

trt1 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[1], ]
trt2 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[2], ]
trt3 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[3], ]
trt4 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[4], ]

#### May need to thin years ####
(table(Engaged_Lines$year)/nrow(Engaged_Lines))*100 ## not balanced
## talk with Sarah on this, I think it might be okay

par(mfrow = c(1,1))
## for loop for the random forest and summary data
for(i in 1:n){
  set.seed(i)
  trt1_sample <- trt1[sample(nrow(trt1), 5000, replace = TRUE), ]
  set.seed(i)
  trt2_sample <- trt2[sample(nrow(trt2), 5000, replace = TRUE), ]
  set.seed(i)
  trt3_sample <- trt3[sample(nrow(trt3), 5000, replace = TRUE), ]
  set.seed(i)
  trt4_sample <- trt4[sample(nrow(trt4), 5000, replace = TRUE), ]
  dat_sub <- rbind(trt1_sample,trt2_sample,trt3_sample,trt4_sample)
  table(dat_sub$stat)
  level1 <- dat_sub[dat_sub$stat == levels(dat_sub$stat)[1], ]
  level2 <- dat_sub[dat_sub$stat == levels(dat_sub$stat)[2], ]
  set.seed(i)
  EF_sample <- level1[sample(nrow(level1), 5000, replace = TRUE), ]
  set.seed(i)
  EH_sample <- level2[sample(nrow(level2), 5000, replace = TRUE), ]
  dat_sub <- rbind(EF_sample, EH_sample)
  table(dat_sub$stat)
  table(dat_sub$trt)
  dat.sp <- vect(dat_sub, geom = c("x","y"))
  dat.cell <- extract(blank, dat.sp, cell = TRUE)
  dat_sub$cell <- dat.cell$cell
  dat_sub <- dat_sub %>% group_by(cell) %>% sample_n(size=1) # sample one point per 100 x 100 m cell
  dat_sub <- vect(dat_sub, geom = c("x","y"), crs = crs(blank))
  dat_sub <- project(dat_sub, "EPSG:4326")
  dmat <- as.matrix(dist(cbind(geom(dat_sub)[,4], geom(dat_sub)[,3]))) ## turning the coordinates of each plot into a distance matrix
  dmat <- dmat *111139 ## degrees to meters (approximately)
  eigen_res <- RSpectra::eigs_sym(as.matrix(dmat), k = num_eigenvectors)
  dat_sub <- as.data.frame(dat_sub)
  dat_sub <- cbind(dat_sub, eigen_res$vectors)
  dat_sub$cell <- NULL
  colnames(dat_sub)[8:(7+num_eigenvectors)] <- paste("vec",colnames(dat_sub)[8:(7+num_eigenvectors)],sep = "")
  dmat <- as.matrix(dist(cbind(dat_sub$y, dat_sub$x))) #
  set.seed(i)
  vec <- order(dmat[sample(1:nrow(dat_sub),1),]) ## getting rows in order of distance to random point generated
  vec <- vec[c(1:(0.75*nrow(dat_sub)))]
  
  dat_sub1 <- dat_sub[,c(1,2,8:37)]
  dat_sub2 <- dat_sub[,c(1,3:7)]
  
  training_set <- dat_sub1[vec,]
  balance1[i] <- 1-length(which(training_set$stat == "EF"))/length(training_set$stat)
  testing_set <- dat_sub1[-vec,]
  set.seed(i)
  train_index <- createDataPartition(y = dat_sub1$stat, p = 0.75, list = FALSE)
  training_set <- dat_sub1[train_index,]
  dat_sub2 <- dat_sub2[-vec,]
  
  set.seed(i)
  rf1 <- randomForest(stat~.,
                     data = training_set,
                     ntree = 500,
                     maxnodes = 75,
                     maximize = TRUE,
                     trControl = train_control,
                     importance = TRUE,
                     keep.forest = TRUE,
                     keep.inbag = TRUE) ## making the rf object
  y_hats1[i,1:nrow(testing_set)] <- predict(object = rf1, newdata = testing_set[, -1], type = "prob")[,2]
  y_hats1.diff[i] <- mean(as.numeric(y_hats1[i,1:nrow(testing_set)]) - (as.numeric(testing_set$stat)-1))
  varImp1.summary[,i] <- rf1$importance[,3] ## Mean decrease accuracy
  varImp1.names[,i] <- rownames(rf1$importance)
  rf1.res[i,c(1:length(testing_set$stat))] <- predict(object = rf1, newdata = testing_set[, -1], type = "prob")[,2] - (as.numeric(testing_set$stat)-1)
  error1[i,] <- rf1$err.rate[,1] ## out of bag error
  rf.roc <- suppressMessages(roc(training_set$stat, rf1$votes[,2]))
  AUC.val1[i] <- as.numeric(auc(rf.roc))
  
  ## second model predicting the residuals
  res4pred <- rf1.res[i,!is.na(rf1.res[i,])]
  length(res4pred)
  
  dat_sub2$obs <- as.numeric(dat_sub2$stat)-1
  dat_sub2$pred <- as.numeric(predict(object = rf1, newdata = testing_set[, -1], type = "prob")[,2])
  dat_sub2$stat <- as.numeric(res4pred)
  dat_sub2$obs <- NULL
  dat_sub2$pred <- NULL
  
  ## balancing dat_sub2 - requires upsampling
  type1 <- dat_sub2[round(dat_sub2$stat, 0) == -1, ]
  correct <- dat_sub2[round(dat_sub2$stat, 0) == 0, ]
  type2 <- dat_sub2[round(dat_sub2$stat, 0) == 1, ]
  
  set.seed(i)
  type1 <- type1[sample(nrow(type1), 1000, replace = TRUE), ]
  set.seed(i)
  correct <- correct[sample(nrow(correct), 1000, replace = TRUE), ]
  set.seed(i)
  type2 <- type2[sample(nrow(type2), 1000, replace = TRUE), ]
  dat_sub2 <- rbind(type1,correct,type2)
  
  set.seed(i)
  vec <- order(dmat[sample(1:nrow(dat_sub2),1),]) ## getting rows in order of distance to random point generated
  vec <- vec[c(1:(0.75*nrow(dat_sub2)))]
  
  training_set <- dat_sub2[vec,]
  balance2[i] <- mean(dat_sub2$stat)
  testing_set <- dat_sub2[-vec,]
  set.seed(i)
  train_index <- createDataPartition(y = dat_sub2$stat, p = 0.75, list = FALSE)
  training_set <- dat_sub2[train_index,]
  
  set.seed(i)
  rf2 <- randomForest(stat~.,
                      data = training_set,
                      ntree = 500,
                      maxnodes = 75,
                      maximize = TRUE,
                      trControl = train_control,
                      importance = TRUE,
                      keep.forest = TRUE,
                      keep.inbag = TRUE) ## making the rf object
  y_hats2[i,1:nrow(testing_set)] <- predict(object = rf2, newdata = testing_set[, -1])
  y_hats2.diff[i] <- mean(as.numeric(y_hats2[i,1:nrow(testing_set)]) - as.numeric(testing_set$stat))
  varImp2.summary[,i] <- rf2$importance[,1]
  varImp2.names[,i] <- rownames(rf2$importance)
  rf2.res[i,c(1:length(rf2$predicted))] <- as.numeric(rf2$predicted) - as.numeric(training_set$stat)
  error2[i,] <- rf2$mse
  training_set$bin.out <- round(training_set$stat,0)
  rf.roc <- suppressMessages(  multiclass.roc(training_set$bin.out, rf2$predicted))
  AUC.val2[i] <- as.numeric(auc(rf.roc))
  r2[i,] <- rf2$rsq
  
  training_set <- as.data.frame(training_set)
  prop.rx <- partialPlot(rf2, training_set, x.var = prop.rx)
  prop.thin <- partialPlot(rf2, training_set, x.var = prop.thin)
  TS.rx <- partialPlot(rf2, training_set, x.var = TS.rx)
  TS.thin <- partialPlot(rf2, training_set, x.var = TS.thin)
  
  prop.rx.x[i,1:length(prop.rx$x)] <- prop.rx$x
  prop.rx.y[i,1:length(prop.rx$y)] <- prop.rx$y
  prop.thin.x[i,1:length(prop.thin$x)] <- prop.thin$x
  prop.thin.y[i,1:length(prop.thin$y)] <- prop.thin$y
  TS.rx.x[i,1:length(TS.rx$x)] <- TS.rx$x
  TS.rx.y[i,1:length(TS.rx$y)] <- TS.rx$y
  TS.thin.x[i,1:length(TS.thin$x)] <- TS.thin$x
  TS.thin.y[i,1:length(TS.thin$y)] <- TS.thin$y
  gc()
  
  progress <- i/n*100
  if (progress %% 5 == 0) {
    print(paste(progress, "% done", sep = ""))
  }
}

## pred vs obs plot
par(mfrow = c(1,1), oma = c(0,0,0,0))
# y_hats1.diff <- y_hats1.diff*100 ## converting to %
max(y_hats1.diff);min(y_hats1.diff)
plot(x = 1:length(y_hats1.diff), y = y_hats1.diff,
     pch = 16,
     xlab = "model run",
     ylim = c(min(y_hats1.diff)-0.1,max(y_hats1.diff)+0.1),
     las = 1,
     main = "Space + Year",
     ylab = "Average Predicted - Observed",
     cex = 1) ## Difference in Predicted Probability vs.Observed Class
round(mean(y_hats1.diff), digits = 3)
abline(h = mean(y_hats1.diff), col="firebrick4", lty = 2)
# text("topright", "Average difference = 6.84%") 

# y_hats2.diff <- y_hats2.diff*100 ## converting to %
max(y_hats2.diff);min(y_hats2.diff)
plot(x = 1:length(y_hats2.diff), y = y_hats2.diff,
     pch = 16,
     xlab = "model run",
     ylim = c(min(y_hats2.diff)-0.1,max(y_hats2.diff)+0.1),
     las = 1,
     main = "Treatments",
     ylab = "Average Predicted - Observed",
     cex = 1) ## predicted probability of residual - observed probability of residual (from rf1)
round(mean(y_hats2.diff), digits = 3)
abline(h = mean(y_hats2.diff), col="firebrick4", lty = 2)
# text(x = 30, y = 50, "Average difference = 12.4%") 

mean(balance1);min(balance1);max(balance1) ## balance of line status
# [1] 0.4918152
# [1] 0.4734645
# [1] 0.5159817

mean(balance2);min(balance2);max(balance2) ## average residual error from rf1 (per model run)
# [1] 0.02118591
# [1] -0.01630067
# [1] 0.05184733

error.mean <- apply(error1,2,mean)
min(error1);max(error1)
plot(error.mean, type = "n",
     ylim = c(0,max(error1)+0.05),
     las = 1,
     xlab = "Tree",
     main = "Space + Year",
     ylab = "OOB Error")
for(i in 1:n){
  lines(error1[i,], col = rgb(0,0,0,alpha = 0.25))
}
lines(error.mean, type = "l", col = "firebrick", lty = 2, lwd= 2)
mean(error.mean)*100 # 31.64696
# text(x = 300, y = 0.18, "Average Error = __%")

error.mean <- apply(error2,2,mean)
min(error2);max(error2)
plot(error.mean, type = "n",
     ylim = c(0,max(error1)+0.05),
     las = 1,
     xlab = "Tree",
     main = "Treatments",
     ylab = "Mean Square Error")
for(i in 1:n){
  lines(error2[i,], col = rgb(0,0,0,alpha = 0.25))
}
lines(error.mean, type = "l", col = "firebrick", lty = 2, lwd= 2)
mean(error.mean)*100 # 18.74919
# text(x = 300, y = 0.18, "Average Error = __%")

par(mfrow = c(1,1))
plot(x = c(1,2),
     y = c(0,1),
     las = 1,
     xaxt = "n",
     xlab = "",
     ylab = "AUC",
     type = "n")
axis(1, at = c(1.2,1.8), line = 1, tick = F, labels = c("Space + Year", "Treatments"), cex.axis = 1.5)
points(x = c(1.2,1.8),
       y = c(mean(AUC.val1), mean(AUC.val2)),
       pch = 16)
segments(x0 = 1.2, y0 = max(AUC.val1), x1 = 1.2, y1 = min(AUC.val1))
segments(x0 = 1.8, y0 = max(AUC.val2), x1 = 1.8, y1 = min(AUC.val2))
abline(h = 0.5, lty = 2)

mean(AUC.val1);min(AUC.val1);max(AUC.val1)
# [1] 0.7681421
# [1] 0.7543344
# [1] 0.7820763

mean(AUC.val2);min(AUC.val2);max(AUC.val2)
# [1] 0.7059042
# [1] 0.6331383
# [1] 0.7967686

r2.mean <- apply(r2,1,mean)
mean(r2.mean);min(r2.mean);max(r2.mean)
hist(r2)
## between 11 - 40% of additional variance explained
# [1] 0.204494
# [1] 0.1107893
# [1] 0.4120808

## VarImp Plot 1
varImp.plotting1 <- data.frame(name = c(varImp1.names[c(1),1],"spatial"),
                              mean = c(mean(varImp1.summary[c(1),]),mean(varImp1.summary[c(2:31),])),
                              min = c(min(varImp1.summary[c(1),]),min(varImp1.summary[c(2:31),])),
                              max = c(max(varImp1.summary[c(1),]),max(varImp1.summary[c(2:31),])))
varImp.plotting1 <- varImp.plotting1[order(varImp.plotting1$mean, decreasing = FALSE),]

min(varImp.plotting1$min)
max(varImp.plotting1$max)
par(mfrow = c(1,2), oma = c(0,3,0,0))
plot(varImp.plotting1$mean,
     ylim = c(0,3),
     xlim = c(0,max(varImp.plotting1$max)), ## max of varImp.plotting$max + a few
     las = 1,
     type = "n",
     ylab = "",
     yaxt = "n",
     xlab = "Mean Decrease Accuracy")
axis(2, at = c(1:2), labels = varImp.plotting1$name, cex.axis = 1, las = 2)
points(x = varImp.plotting1$mean,y = 1:2, col = "black", cex = 1, pch = 16)
segments(x0 = varImp.plotting1$min, y0 = 1:2, x1 = varImp.plotting1$max, y1 = 1:2, col = "black", lwd = 1.5)
# abline(v = 10, lty = 2)

## VarImp Plot 2
varImp.plotting2 <- data.frame(name = c(varImp2.names[c(1:5),1]),
                              mean = c(apply(varImp2.summary[c(1:5),],1,mean)),
                              min = c(apply(varImp2.summary[c(1:5),],1,min)),
                              max = c(apply(varImp2.summary[c(1:5),],1,max)))
varImp.plotting2 <- varImp.plotting2[order(varImp.plotting2$mean, decreasing = FALSE),]

min(varImp.plotting2$min)
max(varImp.plotting2$max)
# par(mfrow = c(1,1), oma = c(0,3,0,0))
plot(varImp.plotting2$mean,
     ylim = c(0,6),
     xlim = c(0,max(varImp.plotting2$max)), ## max of varImp.plotting$max + a few
     las = 1,
     type = "n",
     ylab = "",
     yaxt = "n",
     xlab = "Mean Decrease Accuracy")
axis(2, at = c(1:5), labels = varImp.plotting2$name, cex.axis = 1, las = 2)
points(x = varImp.plotting2$mean,y = 1:5, col = "black", cex = 1, pch = 16)
segments(x0 = varImp.plotting2$min, y0 = 1:5, x1 = varImp.plotting2$max, y1 = 1:5, col = "black", lwd = 1.5)
# abline(v = 10, lty = 2)

## Partial Dependence Plots
# FD <- Engaged_Lines
# FD$LineInt <- as.integer(FD$LineStat)-1
# FD$LineInt[FD$LineInt == 0] <- -0.25
# FD$LineInt[FD$LineInt == 1] <- 1.25

prop.rx.y <- 1-(1/(1+exp(-prop.rx.y)))*2
prop.thin.y <- 1-(1/(1+exp(-prop.thin.y)))*2
TS.rx.y <- 1-(1/(1+exp(-TS.rx.y)))*2
TS.thin.y <- 1-(1/(1+exp(-TS.thin.y)))*2
gc()

par(mfrow = c(2,2))

plot(prop.rx.x[1,], prop.rx.y[1,],
     type = "l",
     ylim = c(-1.25,1.25),
     col = rgb(0,0,0,0.25),
     main = "",
     yaxt = "n",
     cex.axis = 1.5,
     cex.lab = 1.5,
     las = 1,
     xlab = "Proportion Rx Fire",
     ylab = "")
axis(2, at = c(-1,0,1), line = 1, las = 1,tick = T, labels = c("Type 1", "Correct", "Type 2"), cex.axis = 1.5)
for(i in 2:n)(
  lines(prop.rx.x[i,], prop.rx.y[i,], col = rgb(0,0,0,0.25))
)
prop.rx.x.mean <- apply(prop.rx.x,2,mean, na.rm = T)
prop.rx.y.mean <- apply(prop.rx.y,2,mean, na.rm = T)
lo <- loess(prop.rx.y.mean~prop.rx.x.mean)
lines(y = predict(lo), x = prop.rx.x.mean[1:length(predict(lo))], col = "red", lwd = 2)

plot(prop.thin.x[1,], prop.thin.y[1,],
     type = "l",
     ylim = c(-1.25,1.25),
     col = rgb(0,0,0,0.25),
     main = "",
     yaxt = "n",
     cex.axis = 1.5,
     cex.lab = 1.5,
     las = 1,
     xlab = "Proportion Thinning",
     ylab = "")
axis(2, at = c(-1,0,1), line = 1, las = 1,tick = T, labels = c("Type 1", "Correct", "Type 2"), cex.axis = 1.5)
for(i in 2:n)(
  lines(prop.thin.x[i,], prop.thin.y[i,], col = rgb(0,0,0,0.25))
)
prop.thin.x.mean <- apply(prop.thin.x,2,mean, na.rm = T)
prop.thin.y.mean <- apply(prop.thin.y,2,mean, na.rm = T)
lo <- loess(prop.thin.y.mean~prop.thin.x.mean)
lines(y = predict(lo), x = prop.thin.x.mean[1:length(predict(lo))], col = "red", lwd = 2)

plot(TS.rx.x[1,], TS.rx.y[1,],
     type = "l",
     ylim = c(-1.25,1.25),
     col = rgb(0,0,0,0.25),
     main = "",
     yaxt = "n",
     cex.axis = 1.5,
     cex.lab = 1.5,
     las = 1,
     xlab = "Time Since Rx Fire",
     ylab = "")
axis(2, at = c(-1,0,1), line = 1, las = 1,tick = T, labels = c("Type 1", "Correct", "Type 2"), cex.axis = 1.5)
for(i in 2:n)(
  lines(TS.rx.x[i,], TS.rx.y[i,], col = rgb(0,0,0,0.25))
)
TS.rx.x.mean <- apply(TS.rx.x,2,mean, na.rm = T)
TS.rx.y.mean <- apply(TS.rx.y,2,mean, na.rm = T)
lo <- loess(TS.rx.y.mean~TS.rx.x.mean)
lines(y = predict(lo), x = TS.rx.x.mean[1:length(predict(lo))], col = "red", lwd = 2)

plot(TS.thin.x[1,], TS.thin.y[1,],
     type = "l",
     ylim = c(-1.25,1.25),
     col = rgb(0,0,0,0.25),
     main = "",
     yaxt = "n",
      cex.axis = 1.5,
     cex.lab = 1.5,
     las = 1,
     xlab = "Time Since Thinning",
     ylab = "")
axis(2, at = c(-1,0,1), line = 1, las = 1,tick = T, labels = c("Type 1", "Correct", "Type 2"), cex.axis = 1.5)
for(i in 2:n)(
  lines(TS.thin.x[i,], TS.thin.y[i,], col = rgb(0,0,0,0.25))
)
TS.thin.x.mean <- apply(TS.thin.x,2,mean, na.rm = T)
TS.thin.y.mean <- apply(TS.thin.y,2,mean, na.rm = T)
lo <- loess(TS.thin.y.mean~TS.thin.x.mean)
lines(y = predict(lo), x = TS.thin.x.mean[1:length(predict(lo))], col = "red", lwd = 2)
par(mfrow = c(1,1))
hist(round(training_set$stat, 0),
     main = "Example Training Data Western USA",
     las = 1,
     xlab = "Error Category")
hist(training_set$stat,
     main = "Example Training Data Western USA",
     las = 1,
     xlab = "Error Category")


#### splitting engaged lines data into ecoregion scales ####
## determine which ecoregions I need
temp <- list.files(path = "./Geographic Subsets/Ecoregions", pattern="*.shp") ## creating a vector that has all the files in the working directory with .xlsx extensions

for(i in 1:length(temp)){
  path <- paste("./Geographic Subsets/Ecoregions/", temp[i], sep = "")
  assign(temp[i], terra::vect(path))
} ## loading in the shapefiles I want
rm(i);rm(path)

plot(BlueMnts.shp)
Engaged_Lines <- read.csv("Engaged_Lines_DisturbanceHistory.csv")

EL <- Engaged_Lines
Engaged_Lines <- vect(EL, geom = c("x","y"), crs = crs(SouthernRockies.shp))
gc()

obj.names <- gsub(".shp", "", temp)
# Loop through each object
for (i in 1:length(temp)) {
  # Get the object from the global environment
  X <- get(temp[i])
  
  # Your original code with X
  X_Lines <- crop(Engaged_Lines, X)
  X_Lines_df <- as.data.frame(X_Lines, geom = c("XY"))
  
  # Create filename using the object name
  
  filename <- paste0("Engaged_Lines_", obj.names[i], ".csv")
  write.csv(X_Lines_df, paste0("./Geographic Subsets/Ecoregions/",filename))
  
  # Optional: print progress
  cat("Processed:", obj.names[i], "\n")
}
## no fires in the Puget Lowlands (not too strange)
gc()
rm(list = ls())
gc()

#### Random Forests - Ecoregion Scales ####
## NEED TO UPDATE DATA READ IN
sr_Lines_df <- read.csv("D:/Outside Boundary/Engaged_Lines_sr.csv")
table(sr_Lines_df$stat)
(table(sr_Lines_df$stat)/nrow(sr_Lines_df))*100
(table(sr_Lines_df$year)/nrow(sr_Lines_df))*100
(table(sr_Lines_df$trt)/nrow(sr_Lines_df))*100
hist(sr_Lines_df$prop.rx[sr_Lines_df$prop.rx > 0])
hist(sr_Lines_df$prop.thin[sr_Lines_df$prop.thin > 0])
hist(sr_Lines_df$TS.rx[sr_Lines_df$TS.rx < 30])
hist(sr_Lines_df$TS.thin[sr_Lines_df$TS.thin < 30])
par(mfrow = c(1,1))

prop.rx.x <- matrix(data = NA, nrow = n, ncol = 51)
prop.rx.y <- matrix(data = NA, nrow = n, ncol = 51)

prop.thin.x <- matrix(data = NA, nrow = n, ncol = 51)
prop.thin.y <- matrix(data = NA, nrow = n, ncol = 51)

TS.rx.x <- matrix(data = NA, nrow = n, ncol = 51)
TS.rx.y <- matrix(data = NA, nrow = n, ncol = 51)

TS.thin.x <- matrix(data = NA, nrow = n, ncol = 51)
TS.thin.y <- matrix(data = NA, nrow = n, ncol = 51)

rf1.res <- matrix(data = NA, nrow = n, ncol = 10000)
rf2.res <- matrix(data = NA, nrow = n, ncol = 10000)

y_hats1 <- matrix(data = NA, nrow = n, ncol = 10000)
y_hats1.diff <- NA
y_hats2 <- matrix(data = NA, nrow = n, ncol = 10000)
y_hats2.diff <- NA

varImp1.summary <- matrix(data = NA, nrow = 31, ncol = n)
varImp1.names <- matrix(data = NA, nrow = 31, ncol = n)
varImp2.summary <- matrix(data = NA, nrow = 5, ncol = n)
varImp2.names <- matrix(data = NA, nrow = 5, ncol = n)

balance1 <- NA
balance2 <- NA
error1 <- matrix(data = NA, nrow = n, ncol = 500) ## ncol = ntree
error2 <- matrix(data = NA, nrow = n, ncol = 500) ## ncol = ntree
r2_SR <- matrix(data = NA, nrow = n, ncol = 500)

AUC.val1_SR <- NA
AUC.val2_SR <- NA

r <- rast("./LandFire TIFs/WF_dist.tif")
blank <- rast(ext(r), resolution=100, vals=NA) ## gonna expand this
crs(blank) <- crs(r)

str(sr_Lines_df)
sr_Lines_df$X <- NULL
sr_Lines_df$stat <- as.factor(sr_Lines_df$stat)
sr_Lines_df$year <- as.factor(sr_Lines_df$year)
sr_Lines_df$trt <- as.factor(sr_Lines_df$trt)
sr_Lines_df$x <- as.numeric(sr_Lines_df$x)
sr_Lines_df$y <- as.numeric(sr_Lines_df$y)

trt1 <- sr_Lines_df[sr_Lines_df$trt == levels(sr_Lines_df$trt)[1], ]
trt2 <- sr_Lines_df[sr_Lines_df$trt == levels(sr_Lines_df$trt)[2], ]
trt3 <- sr_Lines_df[sr_Lines_df$trt == levels(sr_Lines_df$trt)[3], ]
trt4 <- sr_Lines_df[sr_Lines_df$trt == levels(sr_Lines_df$trt)[4], ]

## for loop for the random forest and summary data
for(i in 1:n){
  set.seed(i)
  trt1_sample <- trt1[sample(nrow(trt1), 5000, replace = TRUE), ]
  set.seed(i)
  trt2_sample <- trt2[sample(nrow(trt2), 5000, replace = TRUE), ]
  set.seed(i)
  trt3_sample <- trt3[sample(nrow(trt3), 5000, replace = TRUE), ]
  set.seed(i)
  trt4_sample <- trt4[sample(nrow(trt4), 5000, replace = TRUE), ]
  dat_sub <- rbind(trt1_sample,trt2_sample,trt3_sample,trt4_sample)
  table(dat_sub$stat)
  level1 <- dat_sub[dat_sub$stat == levels(dat_sub$stat)[1], ]
  level2 <- dat_sub[dat_sub$stat == levels(dat_sub$stat)[2], ]
  set.seed(i)
  EF_sample <- level1[sample(nrow(level1), 5000, replace = TRUE), ]
  set.seed(i)
  EH_sample <- level2[sample(nrow(level2), 5000, replace = TRUE), ]
  dat_sub <- rbind(EF_sample, EH_sample)
  table(dat_sub$stat)
  table(dat_sub$trt)
  dat.sp <- vect(dat_sub, geom = c("x","y"))
  dat.cell <- extract(blank, dat.sp, cell = TRUE)
  dat_sub$cell <- dat.cell$cell
  dat_sub <- dat_sub %>% group_by(cell) %>% sample_n(size=1) # sample one point per 100 x 100 m cell
  dat_sub <- vect(dat_sub, geom = c("x","y"), crs = crs(blank))
  dat_sub <- project(dat_sub, "EPSG:4326")
  dmat <- as.matrix(dist(cbind(geom(dat_sub)[,4], geom(dat_sub)[,3]))) ## turning the coordinates of each plot into a distance matrix
  dmat <- dmat *111139 ## degrees to meters (approximately)
  eigen_res <- RSpectra::eigs_sym(as.matrix(dmat), k = num_eigenvectors)
  dat_sub <- as.data.frame(dat_sub)
  dat_sub <- cbind(dat_sub, eigen_res$vectors)
  dat_sub$cell <- NULL
  colnames(dat_sub)[8:(7+num_eigenvectors)] <- paste("vec",colnames(dat_sub)[8:(7+num_eigenvectors)],sep = "")
  dmat <- as.matrix(dist(cbind(dat_sub$y, dat_sub$x))) #
  set.seed(i)
  vec <- order(dmat[sample(1:nrow(dat_sub),1),]) ## getting rows in order of distance to random point generated
  vec <- vec[c(1:(0.75*nrow(dat_sub)))]
  
  dat_sub1 <- dat_sub[,c(1,2,8:37)]
  dat_sub2 <- dat_sub[,c(1,3:7)]
  
  training_set <- dat_sub1[vec,]
  balance1[i] <- 1-length(which(training_set$stat == "EF"))/length(training_set$stat)
  testing_set <- dat_sub1[-vec,]
  set.seed(i)
  train_index <- createDataPartition(y = dat_sub1$stat, p = 0.75, list = FALSE)
  training_set <- dat_sub1[train_index,]
  dat_sub2 <- dat_sub2[-vec,]
  
  set.seed(i)
  rf1 <- randomForest(stat~.,
                      data = training_set,
                      ntree = 500,
                      maxnodes = 75,
                      maximize = TRUE,
                      trControl = train_control,
                      importance = TRUE,
                      keep.forest = TRUE,
                      keep.inbag = TRUE) ## making the rf object
  y_hats1[i,1:nrow(testing_set)] <- predict(object = rf1, newdata = testing_set[, -1], type = "prob")[,2]
  y_hats1.diff[i] <- mean(as.numeric(y_hats1[i,1:nrow(testing_set)]) - (as.numeric(testing_set$stat)-1))
  varImp1.summary[,i] <- rf1$importance[,3] ## Mean decrease accuracy
  varImp1.names[,i] <- rownames(rf1$importance)
  rf1.res[i,c(1:length(testing_set$stat))] <- predict(object = rf1, newdata = testing_set[, -1], type = "prob")[,2] - (as.numeric(testing_set$stat)-1)
  error1[i,] <- rf1$err.rate[,1] ## out of bag error
  rf.roc <- suppressMessages(roc(training_set$stat, rf1$votes[,2]))
  AUC.val1[i] <- as.numeric(auc(rf.roc))
  
  ## second model predicting the residuals
  res4pred <- rf1.res[i,!is.na(rf1.res[i,])]
  length(res4pred)
  
  dat_sub2$obs <- as.numeric(dat_sub2$stat)-1
  dat_sub2$pred <- as.numeric(predict(object = rf1, newdata = testing_set[, -1], type = "prob")[,2])
  dat_sub2$stat <- as.numeric(res4pred)
  dat_sub2$obs <- NULL
  dat_sub2$pred <- NULL
  
  ## balancing dat_sub2 - requires upsampling
  type1 <- dat_sub2[round(dat_sub2$stat, 0) == -1, ]
  correct <- dat_sub2[round(dat_sub2$stat, 0) == 0, ]
  type2 <- dat_sub2[round(dat_sub2$stat, 0) == 1, ]
  
  set.seed(i)
  type1 <- type1[sample(nrow(type1), 1000, replace = TRUE), ]
  set.seed(i)
  correct <- correct[sample(nrow(correct), 1000, replace = TRUE), ]
  set.seed(i)
  type2 <- type2[sample(nrow(type2), 1000, replace = TRUE), ]
  dat_sub2 <- rbind(type1,correct,type2)
  
  set.seed(i)
  vec <- order(dmat[sample(1:nrow(dat_sub2),1),]) ## getting rows in order of distance to random point generated
  vec <- vec[c(1:(0.75*nrow(dat_sub2)))]
  
  training_set <- dat_sub2[vec,]
  balance2[i] <- mean(dat_sub2$stat)
  testing_set <- dat_sub2[-vec,]
  set.seed(i)
  train_index <- createDataPartition(y = dat_sub2$stat, p = 0.75, list = FALSE)
  training_set <- dat_sub2[train_index,]
  
  set.seed(i)
  rf2 <- randomForest(stat~.,
                      data = training_set,
                      ntree = 500,
                      maxnodes = 75,
                      maximize = TRUE,
                      trControl = train_control,
                      importance = TRUE,
                      keep.forest = TRUE,
                      keep.inbag = TRUE) ## making the rf object
  y_hats2[i,1:nrow(testing_set)] <- predict(object = rf2, newdata = testing_set[, -1])
  y_hats2.diff[i] <- mean(as.numeric(y_hats2[i,1:nrow(testing_set)]) - as.numeric(testing_set$stat))
  varImp2.summary[,i] <- rf2$importance[,1]
  varImp2.names[,i] <- rownames(rf2$importance)
  rf2.res[i,c(1:length(rf2$predicted))] <- as.numeric(rf2$predicted) - as.numeric(training_set$stat)
  error2[i,] <- rf2$mse
  training_set$bin.out <- round(training_set$stat,0)
  rf.roc <- suppressMessages(  multiclass.roc(training_set$bin.out, rf2$predicted))
  AUC.val2[i] <- as.numeric(auc(rf.roc))
  r2[i,] <- rf2$rsq
  
  training_set <- as.data.frame(training_set)
  prop.rx <- partialPlot(rf2, training_set, x.var = prop.rx)
  prop.thin <- partialPlot(rf2, training_set, x.var = prop.thin)
  TS.rx <- partialPlot(rf2, training_set, x.var = TS.rx)
  TS.thin <- partialPlot(rf2, training_set, x.var = TS.thin)
  
  prop.rx.x[i,1:length(prop.rx$x)] <- prop.rx$x
  prop.rx.y[i,1:length(prop.rx$y)] <- prop.rx$y
  prop.thin.x[i,1:length(prop.thin$x)] <- prop.thin$x
  prop.thin.y[i,1:length(prop.thin$y)] <- prop.thin$y
  TS.rx.x[i,1:length(TS.rx$x)] <- TS.rx$x
  TS.rx.y[i,1:length(TS.rx$y)] <- TS.rx$y
  TS.thin.x[i,1:length(TS.thin$x)] <- TS.thin$x
  TS.thin.y[i,1:length(TS.thin$y)] <- TS.thin$y
  gc()
  
  progress <- i/n*100
  if (progress %% 5 == 0) {
    print(paste(progress, "% done", sep = ""))
  }
}

## pred vs obs plot
par(mfrow = c(1,2))
# y_hats1.diff <- y_hats1.diff*100 ## converting to %
max(y_hats1.diff);min(y_hats1.diff)
plot(x = 1:length(y_hats1.diff), y = y_hats1.diff,
     pch = 16,
     xlab = "model run",
     ylim = c(min(y_hats1.diff)-0.1,max(y_hats1.diff)+0.1),
     las = 1,
     main = "Space + Year",
     ylab = "Average Predicted - Observed",
     cex = 1) ## Difference in Predicted Probability vs.Observed Class
round(mean(y_hats1.diff), digits = 3)
abline(h = mean(y_hats1.diff), col="firebrick4", lty = 2)
# text("topright", "Average difference = 6.84%") 

# y_hats2.diff <- y_hats2.diff*100 ## converting to %
max(y_hats2.diff);min(y_hats2.diff)
plot(x = 1:length(y_hats2.diff), y = y_hats2.diff,
     pch = 16,
     xlab = "model run",
     ylim = c(min(y_hats1.diff)-0.1,max(y_hats1.diff)+0.1),
     las = 1,
     main = "Treatments",
     ylab = "Average Predicted - Observed",
     cex = 1) ## predicted probability of residual - observed probability of residual (from rf1)
round(mean(y_hats2.diff), digits = 3)
abline(h = mean(y_hats2.diff), col="firebrick4", lty = 2)
# text(x = 30, y = 50, "Average difference = 12.4%") 

mean(balance1);min(balance1);max(balance1) ## balance of line status
# [1] 0.6070943
# [1] 0.5699896
# [1] 0.6353383

mean(balance2);min(balance2);max(balance2) ## average residual error from rf1 (per model run)
# [1] 0.06036143
# [1] 0.01682586
# [1] 0.1121711

error.mean <- apply(error1,2,mean)
min(error1);max(error1)
plot(error.mean, type = "n",
     ylim = c(0,max(error1)+0.05),
     las = 1,
     xlab = "Tree",
     main = "Space + Year",
     ylab = "OOB Error")
for(i in 1:n){
  lines(error1[i,], col = rgb(0,0,0,alpha = 0.25))
}
lines(error.mean, type = "l", col = "firebrick", lty = 2, lwd= 2)
mean(error.mean)*100 # 14.94094
# text(x = 300, y = 0.18, "Average Error = __%")

error.mean <- apply(error2,2,mean)
min(error2);max(error2)
plot(error.mean, type = "n",
     ylim = c(0,max(error1)+0.05),
     las = 1,
     xlab = "Tree",
     main = "Treatments",
     ylab = "Mean Square Error")
for(i in 1:n){
  lines(error2[i,], col = rgb(0,0,0,alpha = 0.25))
}
lines(error.mean, type = "l", col = "firebrick", lty = 2, lwd= 2)
mean(error.mean)*100 # 7.515766
# text(x = 300, y = 0.18, "Average Error = __%")

par(mfrow = c(1,1))
plot(x = c(1,2),
     y = c(0,1),
     las = 1,
     xaxt = "n",
     xlab = "",
     ylab = "AUC",
     type = "n")
axis(1, at = c(1.2,1.8), line = 1, tick = F, labels = c("Space + Year", "Treatments"), cex.axis = 1.5)
points(x = c(1.2,1.8),
       y = c(mean(AUC.val1_SR), mean(AUC.val2_SR)),
       pch = 16)
segments(x0 = 1.2, y0 = max(AUC.val1_SR), x1 = 1.2, y1 = min(AUC.val1_SR))
segments(x0 = 1.8, y0 = max(AUC.val2_SR), x1 = 1.8, y1 = min(AUC.val2_SR))
abline(h = 0.5, lty = 2)

mean(AUC.val1_SR);min(AUC.val1_SR);max(AUC.val1_SR)
# [1] 0.9270696
# [1] 0.9181488
# [1] 0.9393754

mean(AUC.val2_SR);min(AUC.val2_SR);max(AUC.val2_SR)
# [1] 0.7257837
# [1] 0.5655083
# [1] 0.8820921

r2.mean <- apply(r2_SR,1,mean)
mean(r2.mean);min(r2.mean);max(r2.mean)
## between -0.1 - 7% of additional variance explained (average 2%)
# [1] 0.01774606
# [1] -0.001408677
# [1] 0.06644681

## VarImp Plot 1
varImp.plotting1 <- data.frame(name = c(varImp1.names[c(1),1],"spatial"),
                               mean = c(mean(varImp1.summary[c(1),]),mean(varImp1.summary[c(2:31),])),
                               min = c(min(varImp1.summary[c(1),]),min(varImp1.summary[c(2:31),])),
                               max = c(max(varImp1.summary[c(1),]),max(varImp1.summary[c(2:31),])))
varImp.plotting1 <- varImp.plotting1[order(varImp.plotting1$mean, decreasing = FALSE),]

min(varImp.plotting1$min)
max(varImp.plotting1$max)
par(mfrow = c(1,2), oma = c(0,3,0,0))
plot(varImp.plotting1$mean,
     ylim = c(0,3),
     xlim = c(0,max(varImp.plotting1$max)), ## max of varImp.plotting$max + a few
     las = 1,
     type = "n",
     ylab = "",
     yaxt = "n",
     xlab = "Mean Decrease Accuracy")
axis(2, at = c(1:2), labels = varImp.plotting1$name, cex.axis = 1, las = 2)
points(x = varImp.plotting1$mean,y = 1:2, col = "black", cex = 1, pch = 16)
segments(x0 = varImp.plotting1$min, y0 = 1:2, x1 = varImp.plotting1$max, y1 = 1:2, col = "black", lwd = 1.5)
# abline(v = 10, lty = 2)

## VarImp Plot 2
varImp.plotting2 <- data.frame(name = c(varImp2.names[c(1:5),1]),
                               mean = c(apply(varImp2.summary[c(1:5),],1,mean)),
                               min = c(apply(varImp2.summary[c(1:5),],1,min)),
                               max = c(apply(varImp2.summary[c(1:5),],1,max)))
varImp.plotting2 <- varImp.plotting2[order(varImp.plotting2$mean, decreasing = FALSE),]

min(varImp.plotting2$min)
max(varImp.plotting2$max)
# par(mfrow = c(1,1), oma = c(0,3,0,0))
plot(varImp.plotting2$mean,
     ylim = c(0,6),
     xlim = c(0,max(varImp.plotting1$max)), ## max of varImp.plotting$max + a few
     las = 1,
     type = "n",
     ylab = "",
     yaxt = "n",
     xlab = "Mean Decrease Accuracy")
axis(2, at = c(1:5), labels = varImp.plotting2$name, cex.axis = 1, las = 2)
points(x = varImp.plotting2$mean,y = 1:5, col = "black", cex = 1, pch = 16)
segments(x0 = varImp.plotting2$min, y0 = 1:5, x1 = varImp.plotting2$max, y1 = 1:5, col = "black", lwd = 1.5)
# abline(v = 10, lty = 2)

## Partial Dependence Plots
# FD <- Engaged_Lines
# FD$LineInt <- as.integer(FD$LineStat)-1
# FD$LineInt[FD$LineInt == 0] <- -0.25
# FD$LineInt[FD$LineInt == 1] <- 1.25

prop.rx.y <- 1-(1/(1+exp(-prop.rx.y)))*2
prop.thin.y <- 1-(1/(1+exp(-prop.thin.y)))*2
TS.rx.y <- 1-(1/(1+exp(-TS.rx.y)))*2
TS.thin.y <- 1-(1/(1+exp(-TS.thin.y)))*2
gc()

par(mfrow = c(2,2))

plot(prop.rx.x[1,], prop.rx.y[1,],
     type = "l",
     ylim = c(-1.25,1.25),
     col = rgb(0,0,0,0.25),
     main = "",
     yaxt = "n",
     cex.axis = 1.5,
     cex.lab = 1.5,
     las = 1,
     xlab = "Proportion Rx Fire",
     ylab = "")
axis(2, at = c(-1,0,1), line = 1, las = 1,tick = T, labels = c("Type 1", "Correct", "Type 2"), cex.axis = 1.5)
for(i in 2:n)(
  lines(prop.rx.x[i,], prop.rx.y[i,], col = rgb(0,0,0,0.25))
)
prop.rx.x.mean <- apply(prop.rx.x,2,mean, na.rm = T)
prop.rx.y.mean <- apply(prop.rx.y,2,mean, na.rm = T)
lo <- loess(prop.rx.y.mean~prop.rx.x.mean)
lines(y = predict(lo), x = prop.rx.x.mean[1:length(predict(lo))], col = "red", lwd = 2)

plot(prop.thin.x[1,], prop.thin.y[1,],
     type = "l",
     ylim = c(-1.25,1.25),
     col = rgb(0,0,0,0.25),
     main = "",
     yaxt = "n",
     cex.axis = 1.5,
     cex.lab = 1.5,
     las = 1,
     xlab = "Proportion Thinning",
     ylab = "")
axis(2, at = c(-1,0,1), line = 1, las = 1,tick = T, labels = c("Type 1", "Correct", "Type 2"), cex.axis = 1.5)
for(i in 2:n)(
  lines(prop.thin.x[i,], prop.thin.y[i,], col = rgb(0,0,0,0.25))
)
prop.thin.x.mean <- apply(prop.thin.x,2,mean, na.rm = T)
prop.thin.y.mean <- apply(prop.thin.y,2,mean, na.rm = T)
lo <- loess(prop.thin.y.mean~prop.thin.x.mean)
lines(y = predict(lo), x = prop.thin.x.mean[1:length(predict(lo))], col = "red", lwd = 2)

plot(TS.rx.x[1,], TS.rx.y[1,],
     type = "l",
     ylim = c(-1.25,1.25),
     col = rgb(0,0,0,0.25),
     main = "",
     yaxt = "n",
     cex.axis = 1.5,
     cex.lab = 1.5,
     las = 1,
     xlab = "Time Since Rx Fire",
     ylab = "")
axis(2, at = c(-1,0,1), line = 1, las = 1,tick = T, labels = c("Type 1", "Correct", "Type 2"), cex.axis = 1.5)
for(i in 2:n)(
  lines(TS.rx.x[i,], TS.rx.y[i,], col = rgb(0,0,0,0.25))
)
TS.rx.x.mean <- apply(TS.rx.x,2,mean, na.rm = T)
TS.rx.y.mean <- apply(TS.rx.y,2,mean, na.rm = T)
lo <- loess(TS.rx.y.mean~TS.rx.x.mean)
lines(y = predict(lo), x = TS.rx.x.mean[1:length(predict(lo))], col = "red", lwd = 2)

plot(TS.thin.x[1,], TS.thin.y[1,],
     type = "l",
     ylim = c(-1.25,1.25),
     col = rgb(0,0,0,0.25),
     main = "",
     yaxt = "n",
     cex.axis = 1.5,
     cex.lab = 1.5,
     las = 1,
     xlab = "Time Since Thinning",
     ylab = "")
axis(2, at = c(-1,0,1), line = 1, las = 1,tick = T, labels = c("Type 1", "Correct", "Type 2"), cex.axis = 1.5)
for(i in 2:n)(
  lines(TS.thin.x[i,], TS.thin.y[i,], col = rgb(0,0,0,0.25))
)
TS.thin.x.mean <- apply(TS.thin.x,2,mean, na.rm = T)
TS.thin.y.mean <- apply(TS.thin.y,2,mean, na.rm = T)
lo <- loess(TS.thin.y.mean~TS.thin.x.mean)
lines(y = predict(lo), x = TS.thin.x.mean[1:length(predict(lo))], col = "red", lwd = 2)
par(mfrow = c(1,1))
hist(round(training_set$stat, 0),
     main = "Example Training Data Southern Rockies",
     las = 1,
     xlab = "Error Category")

####  AUC Plot Across Scales ####
par(mfrow = c(1,1))
plot(x = c(1,2),
     y = c(0,1),
     las = 1,
     xaxt = "n",
     xlab = "",
     ylab = "AUC",
     type = "n")
axis(1, at = c(1.2,1.8), line = 1, tick = F, labels = c("Space + Year", "Treatments"), cex.axis = 1.5)
points(x = c(1.1,1.7),
       y = c(mean(AUC.val1), mean(AUC.val2)),
       pch = 16,
       col = "goldenrod")
points(x = c(1.2,1.8),
       y = c(mean(AUC.val1_SR), mean(AUC.val2_SR)),
       pch = 16,
       col = "navy")
points(x = c(1.3,1.9),
       y = c(mean(AUC.val1_CP), mean(AUC.val2_CP)),
       pch = 16,
       col = "magenta3")

segments(x0 = 1.1, y0 = max(AUC.val1), x1 = 1.1, y1 = min(AUC.val1), col = "goldenrod")
segments(x0 = 1.7, y0 = max(AUC.val2), x1 = 1.7, y1 = min(AUC.val2), col = "goldenrod")
segments(x0 = 1.2, y0 = max(AUC.val1_SR), x1 = 1.2, y1 = min(AUC.val1_SR), col = "navy")
segments(x0 = 1.8, y0 = max(AUC.val2_SR), x1 = 1.8, y1 = min(AUC.val2_SR), col = "navy")
segments(x0 = 1.3, y0 = max(AUC.val1_CP), x1 = 1.3, y1 = min(AUC.val1_CP), col = "magenta3")
segments(x0 = 1.9, y0 = max(AUC.val2_CP), x1 = 1.9, y1 = min(AUC.val2_CP), col = "magenta3")
abline(h = 0.5, lty = 2)
legend("bottomright", legend = c("western USA", "Southern Rockies", "Cameron Pass"),
       col = c("goldenrod","navy","magenta3"), pch = 16, ncol = 1, bty = "n")


r2.mean1 <- apply(r2,1,mean)
r2.mean2 <- apply(r2_SR,1,mean)
r2.mean3 <- apply(r2_CP,1,mean)


## R2
par(mfrow = c(1,1))
plot(x = c(1,2),
     y = c(-1,1),
     las = 1,
     xaxt = "n",
     xlab = "",
     ylab = "R2",
     type = "n")
abline(h = 0, lty = 2)
points(x = c(1.4),
       y = mean(r2.mean1),
       pch = 16,
       col = "goldenrod")
points(x = c(1.5),
       y = mean(r2.mean2),
       pch = 16,
       col = "navy")
points(x = c(1.6),
       y = mean(r2.mean3),
       pch = 16,
       col = "magenta3")

segments(x0 = 1.4, y0 = max(r2.mean1), x1 = 1.4, y1 = min(r2.mean1), col = "goldenrod")
segments(x0 = 1.5, y0 = max(r2.mean2), x1 = 1.5, y1 = min(r2.mean2), col = "navy")
segments(x0 = 1.6, y0 = max(r2.mean3), x1 = 1.6, y1 = min(r2.mean3), col = "magenta3")
abline(h = 0, lty = 2)
legend("bottomright", legend = c("western USA", "Southern Rockies", "Cameron Pass"),
       col = c("goldenrod","navy","magenta3"), pch = 16, ncol = 1, bty = "n")

#### Chi Sq. Test ####
Engaged_Lines <- read.csv("Engaged_Lines_DisturbanceHistory.csv")
colnames(Engaged_Lines)[c(31:34,36:40,28,29)]
Engaged_Lines <- Engaged_Lines[,c(31:34,36:40,28,29)]
head(Engaged_Lines)

## all fires
table(Engaged_Lines$trt)
table(Engaged_Lines$stat[Engaged_Lines$trt == "Thinning only"])
table(Engaged_Lines$stat[Engaged_Lines$trt == "Thinning and Prescribed"])
table(Engaged_Lines$stat[Engaged_Lines$trt == "Prescribed only"])
table(Engaged_Lines$stat[Engaged_Lines$trt == "Neither"])

mat <- matrix(c(nrow(Engaged_Lines[Engaged_Lines$stat == "EH" & Engaged_Lines$trt == "Thinning only",]),
                nrow(Engaged_Lines[Engaged_Lines$stat == "EF" & Engaged_Lines$trt == "Thinning only",]),
                nrow(Engaged_Lines[Engaged_Lines$stat == "EH" & Engaged_Lines$trt == "Prescribed only",]),
                nrow(Engaged_Lines[Engaged_Lines$stat == "EF" & Engaged_Lines$trt == "Prescribed only",]),
                nrow(Engaged_Lines[Engaged_Lines$stat == "EH" & Engaged_Lines$trt == "Thinning and Prescribed",]),
                nrow(Engaged_Lines[Engaged_Lines$stat == "EF" & Engaged_Lines$trt == "Thinning and Prescribed",]),
                nrow(Engaged_Lines[Engaged_Lines$stat == "EH" & Engaged_Lines$trt == "Neither",]),
                nrow(Engaged_Lines[Engaged_Lines$stat == "EF" & Engaged_Lines$trt == "Neither",])),
              nrow = 2, byrow = F)

colnames(mat) <- c("Thin", "Burn", "ThinXBurn","No Trt")
rownames(mat) <- c("EH", "EF")

mosaicplot(mat,
           main = "",
           xlab = "Line Status",
           ylab = "Treatment")

chisq.test(mat)

m2 <- mat[c(1,2),c(4,1)]
mosaicplot(m2,
           main = "",
           xlab = "Line Status",
           ylab = "Treatment")

chisq.test(m2)
fisher.test(m2) ## looks like thinned lines more likely to fail

m3 <- mat[c(1,2),c(4,2)]
mosaicplot(m3,
           main = "",
           xlab = "Line Status",
           ylab = "Treatment")

chisq.test(m3)
fisher.test(m3) ## looks like burned lines more likely to fail

m4 <- mat[c(1,2),c(4,3)]
mosaicplot(m4,
           main = "",
           xlab = "Line Status",
           ylab = "Treatment")

chisq.test(m4)
fisher.test(m4) ## looks like thinned x burned lines less likely to fail
1/0.8139909 ## 20% increase in lines holding in TxB

## splitting into small and larger fires
hist(Engaged_Lines$BurnAcre)
smallfires <- Engaged_Lines[Engaged_Lines$BurnAcre < 10000,]
smallfires <- smallfires[complete.cases(smallfires$stat),]
largefires <- Engaged_Lines[Engaged_Lines$BurnAcre >= 10000,]
largefires <- largefires[complete.cases(largefires$stat),]
gc()

## small fires
mat <- matrix(c(nrow(smallfires[smallfires$stat == "EH" & smallfires$trt == "Thinning only",]),
                nrow(smallfires[smallfires$stat == "EF" & smallfires$trt == "Thinning only",]),
                nrow(smallfires[smallfires$stat == "EH" & smallfires$trt == "Prescribed only",]),
                nrow(smallfires[smallfires$stat == "EF" & smallfires$trt == "Prescribed only",]),
                nrow(smallfires[smallfires$stat == "EH" & smallfires$trt == "Thinning and Prescribed",]),
                nrow(smallfires[smallfires$stat == "EF" & smallfires$trt == "Thinning and Prescribed",]),
                nrow(smallfires[smallfires$stat == "EH" & smallfires$trt == "Neither",]),
                nrow(smallfires[smallfires$stat == "EF" & smallfires$trt == "Neither",])),
              nrow = 2, byrow = F)

colnames(mat) <- c("Thin", "Burn", "ThinXBurn","No Trt")
rownames(mat) <- c("EH", "EF")

mosaicplot(mat,
           main = "",
           xlab = "Line Status",
           ylab = "Treatment")

chisq.test(mat)

m2 <- mat[c(1,2),c(4,1)]
mosaicplot(m2,
           main = "",
           xlab = "Line Status",
           ylab = "Treatment")

chisq.test(m2)
fisher.test(m2) ## looks like thinned lines more likely to fail

m3 <- mat[c(1,2),c(4,2)]
mosaicplot(m3,
           main = "",
           xlab = "Line Status",
           ylab = "Treatment")

chisq.test(m3)
fisher.test(m3) ## looks like burned lines more likely to fail

m4 <- mat[c(1,2),c(4,3)]
mosaicplot(m4,
           main = "",
           xlab = "Line Status",
           ylab = "Treatment")

chisq.test(m4)
fisher.test(m4) ## looks like thinned x burned lines less likely to fail

## large fires
mat <- matrix(c(nrow(largefires[largefires$stat == "EH" & largefires$trt == "Thinning only",]),
                nrow(largefires[largefires$stat == "EF" & largefires$trt == "Thinning only",]),
                nrow(largefires[largefires$stat == "EH" & largefires$trt == "Prescribed only",]),
                nrow(largefires[largefires$stat == "EF" & largefires$trt == "Prescribed only",]),
                nrow(largefires[largefires$stat == "EH" & largefires$trt == "Thinning and Prescribed",]),
                nrow(largefires[largefires$stat == "EF" & largefires$trt == "Thinning and Prescribed",]),
                nrow(largefires[largefires$stat == "EH" & largefires$trt == "Neither",]),
                nrow(largefires[largefires$stat == "EF" & largefires$trt == "Neither",])),
              nrow = 2, byrow = F)

colnames(mat) <- c("Thin", "Burn", "ThinXBurn","No Trt")
rownames(mat) <- c("EH", "EF")

mosaicplot(mat,
           main = "",
           xlab = "Line Status",
           ylab = "Treatment")

chisq.test(mat)

m2 <- mat[c(1,2),c(4,1)]
mosaicplot(m2,
           main = "",
           xlab = "Line Status",
           ylab = "Treatment")

chisq.test(m2)
fisher.test(m2) ## looks like thinned lines more likely to fail

m3 <- mat[c(1,2),c(4,2)]
mosaicplot(m3,
           main = "",
           xlab = "Line Status",
           ylab = "Treatment")

chisq.test(m3)
fisher.test(m3) ## looks like burned lines more likely to fail

m4 <- mat[c(1,2),c(4,3)]
mosaicplot(m4,
           main = "",
           xlab = "Line Status",
           ylab = "Treatment")

chisq.test(m4)
fisher.test(m4) ## looks like thinned x burned lines less likely to fail

#### Chi sq. by Ecoregions ####
## adding ecoregion
W_Fires <- vect("./mtbs_perimeter_data/WF_Fires.shp")
W_Fires$Incid_Name <- tolower(gsub("[[:punct:][:space:]]", "", W_Fires$Incid_Name))
temp <- list.files(path = "./Geographic Subsets/Ecoregions", pattern="*.shp")

for(i in 1:length(temp)){
  path <- paste("./Geographic Subsets/Ecoregions/", temp[i], sep = "")
  assign(temp[i], terra::vect(path))
} ## loading in the shapefiles I want
rm(i);rm(path)

obj.names <- gsub(".shp", "", temp)
Engaged_Lines$ecoregion <- NA

for(i in 1:length(temp)){
  X <- get(temp[i])
  # X <- terra::buffer(X, 15000) ## buffering the ecoregions, causes some problems with overlaps
  X_Fires <- crop(W_Fires,X)
  X_Fires <- values(X_Fires)
  Engaged_Lines$ecoregion[Engaged_Lines$Incid_Name %in% X_Fires$Incid_Name] <- obj.names[i]
}
rm(list = temp)
rm(temp);rm(X);rm(X_Fires);rm(i);rm(W_Fires)

table(Engaged_Lines$ecoregion)
table(is.na(Engaged_Lines$ecoregion))
Engaged_Lines <- Engaged_Lines[complete.cases(Engaged_Lines$ecoregion),]

## splitting into small and larger fires
hist(Engaged_Lines$BurnAcre)
smallfires <- Engaged_Lines[Engaged_Lines$BurnAcre < 10000,]
smallfires <- smallfires[complete.cases(smallfires$stat),]
largefires <- Engaged_Lines[Engaged_Lines$BurnAcre >= 10000,]
largefires <- largefires[complete.cases(largefires$stat),]
gc()

## small fires
vec <- unique(Engaged_Lines$ecoregion)
trts <- unique(Engaged_Lines$trt)

est.mat.s <-matrix(nrow = length(vec),
                 ncol = length(trts))
rownames(est.mat.s) <- vec
colnames(est.mat.s) <- trts

lwr.mat.s <-matrix(nrow = length(vec),
                 ncol = length(trts))
rownames(lwr.mat.s) <- vec
colnames(lwr.mat.s) <- trts

upr.mat.s <-matrix(nrow = length(vec),
                 ncol = length(trts))
rownames(upr.mat.s) <- vec
colnames(upr.mat.s) <- trts

p.mat.s <-matrix(nrow = length(vec),
                 ncol = length(trts))
rownames(p.mat.s) <- vec
colnames(p.mat.s) <- trts

for(i in 1:length(vec)){
  mat <- matrix(c(nrow(smallfires[smallfires$stat == "EH" & smallfires$trt == "Neither" & smallfires$ecoregion == vec[i],]),
                  nrow(smallfires[smallfires$stat == "EF" & smallfires$trt == "Neither" & smallfires$ecoregion == vec[i],]),
                  nrow(smallfires[smallfires$stat == "EH" & smallfires$trt == "Prescribed only" & smallfires$ecoregion == vec[i],]),
                  nrow(smallfires[smallfires$stat == "EF" & smallfires$trt == "Prescribed only" & smallfires$ecoregion == vec[i],]),
                  nrow(smallfires[smallfires$stat == "EH" & smallfires$trt == "Thinning only" & smallfires$ecoregion == vec[i],]),
                  nrow(smallfires[smallfires$stat == "EF" & smallfires$trt == "Thinning only" & smallfires$ecoregion == vec[i],]),
                  nrow(smallfires[smallfires$stat == "EH" & smallfires$trt == "Thinning and Prescribed" & smallfires$ecoregion == vec[i],]),
                  nrow(smallfires[smallfires$stat == "EF" & smallfires$trt == "Thinning and Prescribed" & smallfires$ecoregion == vec[i],])),
                nrow = 2, byrow = F)
  colnames(mat) <- trts
  rownames(mat) <- c("EH", "EF")
  for(j in 1:length(trts)){
    m2 <- mat[c(1,2),c(j,1)]
    est.mat.s[i,j] <- fisher.test(m2)$estimate
    lwr.mat.s[i,j] <- fisher.test(m2)$conf.int[1]
    upr.mat.s[i,j] <- fisher.test(m2)$conf.int[2]
    p.mat.s[i,j] <- fisher.test(m2)$p.value
  }
}


## large fires
est.mat.l <-matrix(nrow = length(vec),
                   ncol = length(trts))
rownames(est.mat.l) <- vec
colnames(est.mat.l) <- trts

lwr.mat.l <-matrix(nrow = length(vec),
                   ncol = length(trts))
rownames(lwr.mat.l) <- vec
colnames(lwr.mat.l) <- trts

upr.mat.l <-matrix(nrow = length(vec),
                   ncol = length(trts))
rownames(upr.mat.l) <- vec
colnames(upr.mat.l) <- trts
p.mat.l <-matrix(nrow = length(vec),
                 ncol = length(trts))
rownames(p.mat.l) <- vec
colnames(p.mat.l) <- trts

for(i in 1:length(vec)){
  mat <- matrix(c(nrow(largefires[largefires$stat == "EH" & largefires$trt == "Neither" & largefires$ecoregion == vec[i],]),
                  nrow(largefires[largefires$stat == "EF" & largefires$trt == "Neither" & largefires$ecoregion == vec[i],]),
                  nrow(largefires[largefires$stat == "EH" & largefires$trt == "Prescribed only" & largefires$ecoregion == vec[i],]),
                  nrow(largefires[largefires$stat == "EF" & largefires$trt == "Prescribed only" & largefires$ecoregion == vec[i],]),
                  nrow(largefires[largefires$stat == "EH" & largefires$trt == "Thinning only" & largefires$ecoregion == vec[i],]),
                  nrow(largefires[largefires$stat == "EF" & largefires$trt == "Thinning only" & largefires$ecoregion == vec[i],]),
                  nrow(largefires[largefires$stat == "EH" & largefires$trt == "Thinning and Prescribed" & largefires$ecoregion == vec[i],]),
                  nrow(largefires[largefires$stat == "EF" & largefires$trt == "Thinning and Prescribed" & largefires$ecoregion == vec[i],])),
                nrow = 2, byrow = F)
  colnames(mat) <- trts
  rownames(mat) <- c("EH", "EF")
  for(j in 1:length(trts)){
    m2 <- mat[c(1,2),c(j,1)]
    est.mat.l[i,j] <- fisher.test(m2)$estimate
    lwr.mat.l[i,j] <- fisher.test(m2)$conf.int[1]
    upr.mat.l[i,j] <- fisher.test(m2)$conf.int[2]
    p.mat.l[i,j] <- fisher.test(m2)$p.value
  }
  
}

# upr.mat.s[upr.mat.s == Inf] <- NA
# upr.mat.s[upr.mat.s == 0] <- NA
# est.mat.s[est.mat.s == Inf] <- NA
# est.mat.s[est.mat.s == 0] <- NA
# lwr.mat.s[lwr.mat.s == Inf] <- NA
# lwr.mat.s[lwr.mat.s == 0] <- NA

max(upr.mat.s[which(p.mat.s[,j] < 0.05),j])+0.5
min(lwr.mat.s[which(p.mat.s[,j] < 0.05),j])-0.5

## plotting small fires first
par(oma = c(0,3,0,0))

## Thinning
j <- 3
plot(x = c(min(lwr.mat.s[which(p.mat.s[,j] < 0.05),j])-0.5:max(upr.mat.s[which(p.mat.s[,j] < 0.05),j])+0.5),
     ylim = c(0,12),
     xlim = c(min(lwr.mat.s[which(p.mat.s[,j] < 0.05),j])-0.5,max(upr.mat.s[which(p.mat.s[,j] < 0.05),j])+0.5),
     las = 1,
     main = trts[j],
     cex.axis = 1.5,
     xlab = "",
     type = "n",
     yaxt = "n",
     ylab = "") ## ecoregion
text(y = 1:length(vec),
     x = -4.5,
     labels = vec,
     xpd = NA,
     srt = 0,      ## Rotate the labels by 35 degrees.
     cex = 1)
abline(v = 1, lty = 2)
for(i in 1:length(vec)){
  points(x = ifelse(p.mat.s[i,j] < 0.05,est.mat.s[i,j],NA),
         y = i,
         col = "black",
         cex = 0.5,
         pch = 16)
  segments(y0 = i, x0 = ifelse(p.mat.s[i,j] < 0.05,lwr.mat.s[i,j],NA), 
           y1 = i, x1 = ifelse(p.mat.s[i,j] < 0.05,upr.mat.s[i,j],NA), 
           col = "black",
           lwd = 1.5)
}

## Rx Fire
j <- 2
plot(x = c(min(lwr.mat.s[which(p.mat.s[,j] < 0.05),j])-0.5:max(upr.mat.s[which(p.mat.s[,j] < 0.05),j])+0.5),
     ylim = c(0,12),
     xlim = c(min(lwr.mat.s[which(p.mat.s[,j] < 0.05),j])-0.5,max(upr.mat.s[which(p.mat.s[,j] < 0.05),j])+0.5),
     las = 1,
     main = trts[j],
     cex.axis = 1.5,
     xlab = "",
     type = "n",
     yaxt = "n",
     ylab = "") ## ecoregion
text(y = 1:length(vec),
     x = -1.5,
     labels = vec,
     xpd = NA,
     srt = 0,      ## Rotate the labels by 35 degrees.
     cex = 1)
abline(v = 1, lty = 2)
for(i in 1:length(vec)){
  points(x = ifelse(p.mat.s[i,j] < 0.05,est.mat.s[i,j],NA),
         y = i,
         col = "black",
         cex = 0.5,
         pch = 16)
  segments(y0 = i, x0 = ifelse(p.mat.s[i,j] < 0.05,lwr.mat.s[i,j],NA), 
           y1 = i, x1 = ifelse(p.mat.s[i,j] < 0.05,upr.mat.s[i,j],NA), 
           col = "black",
           lwd = 1.5)
}

## Thinning + Rx
j <- 4
plot(x = c(min(lwr.mat.s[which(p.mat.s[,j] < 0.05),j])-0.5:max(upr.mat.s[which(p.mat.s[,j] < 0.05),j])+0.5),
     ylim = c(0,12),
     xlim = c(min(lwr.mat.s[which(p.mat.s[,j] < 0.05),j])-0.5,max(upr.mat.s[which(p.mat.s[,j] < 0.05),j])+0.5),
     las = 1,
     main = trts[j],
     cex.axis = 1.5,
     xlab = "",
     type = "n",
     yaxt = "n",
     ylab = "") ## ecoregion
text(y = 1:length(vec),
     x = -2,
     labels = vec,
     xpd = NA,
     srt = 0,      ## Rotate the labels by 35 degrees.
     cex = 1)
abline(v = 1, lty = 2)
for(i in 1:length(vec)){
  points(x = ifelse(p.mat.s[i,j] < 0.05,est.mat.s[i,j],NA),
         y = i,
         col = "black",
         cex = 0.5,
         pch = 16)
  segments(y0 = i, x0 = ifelse(p.mat.s[i,j] < 0.05,lwr.mat.s[i,j],NA), 
           y1 = i, x1 = ifelse(p.mat.s[i,j] < 0.05,upr.mat.s[i,j],NA), 
           col = "black",
           lwd = 1.5)
}

## large fires
upr.mat.l[upr.mat.l == Inf] <- 100 ## making infinite valus very high

## Thinning
j <- 3
plot(x = c(min(lwr.mat.l[which(p.mat.l[,j] < 0.05),j])-0.5:max(upr.mat.l[which(p.mat.l[,j] < 0.05),j])+0.5),
     ylim = c(0,12),
     xlim = c(min(lwr.mat.l[which(p.mat.l[,j] < 0.05),j])-0.5,max(upr.mat.l[which(p.mat.l[,j] < 0.05),j])+0.5),
     las = 1,
     main = trts[j],
     cex.axis = 1.5,
     xlab = "",
     type = "n",
     yaxt = "n",
     ylab = "") ## ecoregion
text(y = 1:length(vec),
     x = -25,
     labels = vec,
     xpd = NA,
     srt = 0,      ## Rotate the labels by 35 degrees.
     cex = 1)
abline(v = 1, lty = 2)
for(i in 1:length(vec)){
  points(x = ifelse(p.mat.l[i,j] < 0.05,est.mat.l[i,j],NA),
         y = i,
         col = "black",
         cex = 0.5,
         pch = 16)
  segments(y0 = i, x0 = ifelse(p.mat.l[i,j] < 0.05,lwr.mat.l[i,j],NA), 
           y1 = i, x1 = ifelse(p.mat.l[i,j] < 0.05,upr.mat.l[i,j],NA), 
           col = "black",
           lwd = 1.5)
}

## Rx
j <- 2
plot(x = c(min(lwr.mat.l[which(p.mat.l[,j] < 0.05),j])-0.5:max(upr.mat.l[which(p.mat.l[,j] < 0.05),j])+0.5),
     ylim = c(0,12),
     xlim = c(min(lwr.mat.l[which(p.mat.l[,j] < 0.05),j])-0.5,max(upr.mat.l[which(p.mat.l[,j] < 0.05),j])+0.5),
     las = 1,
     main = trts[j],
     cex.axis = 1.5,
     xlab = "",
     type = "n",
     yaxt = "n",
     ylab = "") ## ecoregion
text(y = 1:length(vec),
     x = -200,
     labels = vec,
     xpd = NA,
     srt = 0,      ## Rotate the labels by 35 degrees.
     cex = 1)
abline(v = 1, lty = 2)
for(i in 1:length(vec)){
  points(x = ifelse(p.mat.l[i,j] < 0.05,est.mat.l[i,j],NA),
         y = i,
         col = "black",
         cex = 0.5,
         pch = 16)
  segments(y0 = i, x0 = ifelse(p.mat.l[i,j] < 0.05,lwr.mat.l[i,j],NA), 
           y1 = i, x1 = ifelse(p.mat.l[i,j] < 0.05,upr.mat.l[i,j],NA), 
           col = "black",
           lwd = 1.5)
}

## Both
j <- 4
plot(x = c(min(lwr.mat.l[which(p.mat.l[,j] < 0.05),j])-0.5:max(upr.mat.l[which(p.mat.l[,j] < 0.05),j])+0.5),
     ylim = c(0,12),
     xlim = c(min(lwr.mat.l[which(p.mat.l[,j] < 0.05),j])-0.5,max(upr.mat.l[which(p.mat.l[,j] < 0.05),j])+0.5),
     las = 1,
     main = trts[j],
     cex.axis = 1.5,
     xlab = "",
     type = "n",
     yaxt = "n",
     ylab = "") ## ecoregion
text(y = 1:length(vec),
     x = -50,
     labels = vec,
     xpd = NA,
     srt = 0,      ## Rotate the labels by 35 degrees.
     cex = 1)
abline(v = 1, lty = 2)
for(i in 1:length(vec)){
  points(x = ifelse(p.mat.l[i,j] < 0.05,est.mat.l[i,j],NA),
         y = i,
         col = "black",
         cex = 0.5,
         pch = 16)
  segments(y0 = i, x0 = ifelse(p.mat.l[i,j] < 0.05,lwr.mat.l[i,j],NA), 
           y1 = i, x1 = ifelse(p.mat.l[i,j] < 0.05,upr.mat.l[i,j],NA), 
           col = "black",
           lwd = 1.5)
}

