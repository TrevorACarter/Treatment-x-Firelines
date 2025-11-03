library(terra)
library(exactextractr)
WesternStates <- vect("D:/Aspen Firelines/SouthernRockyBoundary_10kmBuff.shp") ## keeping the name the same for posterity sake

#### Adding Raster files from LandFire (1999-2024) ####
setwd("D:/Outside Boundary/LandFire TIFs")
temp <- list.files(pattern="*.tif") ## creating a vector that has all the files in the working directory with .xlsx extensions
temp <- temp[-27]

for(i in 1:length(temp)) assign(temp[i], terra::rast(temp[i])) ## loading in the shapefiles I want

## stacking the rasters
dist_stack <- c(LF18_Dist.tif,LF19_Dist.tif,LF20_Dist.tif,LF21_Dist.tif,LF22_Dist.tif,LF23_Dist.tif)
rm(LF99_Dist.tif)
rm(LF00_Dist.tif);rm(LF01_Dist.tif);rm(LF02_Dist.tif);rm(LF03_Dist.tif);rm(LF04_Dist.tif);rm(LF05_Dist.tif);rm(LF06_Dist.tif);rm(LF07_Dist.tif);rm(LF08_Dist.tif);rm(LF09_Dist.tif)
rm(LF10_Dist.tif);rm(LF11_Dist.tif);rm(LF12_Dist.tif);rm(LF13_Dist.tif);rm(LF14_Dist.tif);rm(LF15_Dist.tif);rm(LF16_Dist.tif);rm(LF17_Dist.tif);rm(LF18_Dist.tif);rm(LF19_Dist.tif)
rm(LF20_Dist.tif);rm(LF21_Dist.tif);rm(LF22_Dist.tif);rm(LF23_Dist.tif);rm(LF24_Dist.tif)

dist_crop <- crop(dist_stack, WesternStates)
gc()
W_dist <- mask(dist_crop, WesternStates)
gc()
plot(W_dist[[2]])

rm(dist_crop);rm(dist_stack)
# writeRaster(W_dist, "W_dist.tif")


#### Adding NIFC Fire Line Data (2018-2024) ####  
setwd("D:/Outside Boundary/NIFC Lines")
temp <- list.files(pattern="*.shp") ## creating a vector that has all the files in the working directory with .xlsx extensions
temp <- temp[-8]

for(i in 1:length(temp)) assign(temp[i], terra::vect(temp[i])) ## loading in the shapefiles I want

EventLine2022.shp$year <- 2022
EventLine2023.shp$year <- 2023

stacked_FL <- rbind(EventLine2022.shp,EventLine2023.shp)
rm(EventLine2018.shp);rm(EventLine2019.shp);rm(EventLine2020.shp);rm(EventLine2021.shp);rm(EventLine2022.shp);rm(EventLine2023.shp);rm(EventLine2024.shp)
gc()

W_FLs <- crop(stacked_FL,WesternStates)
W_FLs <- terra::unique(W_FLs)
gc()

vec <- values(W_FLs)
colnames(vec) ## Did a double check with all years, confirmed that Feature Cat is only needed column to query
table(vec$FeatureCat)

W_FLs <- W_FLs[W_FLs$FeatureCat == "Completed Burnout" |
                 W_FLs$FeatureCat == "Completed Dozer Line" |
                 W_FLs$FeatureCat == "Completed Fuel Break" | 
                 W_FLs$FeatureCat == "Completed Hand Line" |
                 W_FLs$FeatureCat == "Completed Line" |
                 W_FLs$FeatureCat == "Completed Mixed Construction Line" |
                 W_FLs$FeatureCat == "Completed Plow Line" |
                 W_FLs$FeatureCat == "Completed Road as Line" |
                 W_FLs$FeatureCat == "Contained Line",]
gc()
vec <- values(W_FLs)
table(vec$FeatureCat)
rm(stacked_FL);rm(vec)
gc()

# writeVector(W_FLs, "W_FLs.shp")

#### Adding Fire Polygon Data from NIFC (2018-2024) ####
setwd("D:/Outside Boundary/NIFC Polygons")
temp <- list.files(pattern="*.shp") ## creating a vector that has all the files in the working directory with .xlsx extensions
temp <- temp[-8]

for(i in 1:length(temp)) assign(temp[i], terra::vect(temp[i])) ## loading in the shapefiles I want

EventPolygon2022.shp$year <- 2022
length(unique(EventPolygon2022.shp$IncidentNa)) ## 33027
EventPolygon2022.shp$IncidentNa <- tolower(gsub("[[:punct:][:space:]]", "", EventPolygon2022.shp$IncidentNa))
EventPolygon2022.shp <- EventPolygon2022.shp[!grepl("rx",EventPolygon2022.shp$IncidentNa, ignore.case = TRUE),]
table(grepl("rx",EventPolygon2022.shp$IncidentNa, ignore.case = TRUE))
EventPolygon2022.shp <- EventPolygon2022.shp[!grepl("pileburn",EventPolygon2022.shp$IncidentNa, ignore.case = TRUE),]
table(grepl("pileburn",EventPolygon2022.shp$IncidentNa, ignore.case = TRUE))
EventPolygon2022.shp <- EventPolygon2022.shp[order(EventPolygon2022.shp$GISAcres, decreasing = TRUE),] ## sorting by size
EventPolygon2022.shp <- EventPolygon2022.shp[!duplicated(EventPolygon2022.shp$IncidentNa),] ## keeping the largest fire per each name (there are errors in here)
length(unique(EventPolygon2022.shp$IncidentNa)) ## 30007
gc()

EventPolygon2023.shp$year <- 2023
length(unique(EventPolygon2023.shp$IncidentNa)) ## 40903
EventPolygon2023.shp$IncidentNa <- tolower(gsub("[[:punct:][:space:]]", "", EventPolygon2023.shp$IncidentNa))
EventPolygon2023.shp <- EventPolygon2023.shp[!grepl("rx",EventPolygon2023.shp$IncidentNa, ignore.case = TRUE),]
table(grepl("rx",EventPolygon2023.shp$IncidentNa, ignore.case = TRUE))
EventPolygon2023.shp <- EventPolygon2023.shp[!grepl("pileburn",EventPolygon2023.shp$IncidentNa, ignore.case = TRUE),]
table(grepl("pileburn",EventPolygon2023.shp$IncidentNa, ignore.case = TRUE))
EventPolygon2023.shp <- EventPolygon2023.shp[order(EventPolygon2023.shp$GISAcres, decreasing = TRUE),] ## sorting by size
EventPolygon2023.shp <- EventPolygon2023.shp[!duplicated(EventPolygon2023.shp$IncidentNa),] ## keeping the largest fire per each name (there are errors in here)
length(unique(EventPolygon2023.shp$IncidentNa)) ## 33955
gc()

FirePoly <- rbind(EventPolygon2022.shp,EventPolygon2023.shp)
rm(EventPolygon2018.shp);rm(EventPolygon2019.shp);rm(EventPolygon2020.shp);rm(EventPolygon2021.shp);rm(EventPolygon2022.shp);rm(EventPolygon2023.shp);rm(EventPolygon2024.shp)
gc()

table(terra::is.valid(FirePoly)) ## checking the validity of geometery
## have several thousand invalid topologies
FirePoly <- terra::makeValid(FirePoly)
table(terra::is.valid(FirePoly)) ## checking the validity of geometery
## validated
gc()

W_Fires <- crop(FirePoly,WesternStates)
gc()
W_Fires <- terra::unique(W_Fires)
gc()

vec <- values(W_Fires)
colnames(vec)

# writeVector(W_Fires,"W_Fires.shp")

rm(FirePoly)
gc()


#### Extracting Treatment History for Fire Lines ####
vec <- c(2022,2023) ## change to reflect data range
Engaged_Lines <- NA
Treatment_Boundary <- NA
Inside_Treatment <- NA

# Pre-allocate list to store results
Treatment_Boundary_list <- vector("list", length(vec))
Inside_Treatment_list <- vector("list", length(vec))
Engaged_Lines_list <- vector("list", length(vec) * 2)

# Pre-filter data once
W_Fires_filtered <- W_Fires[W_Fires$year %in% vec, ]
W_FLs_filtered <- W_FLs[W_FLs$year %in% vec, ]

for(i in seq_along(vec)){
  year_i <- vec[i]
  
  # Filter for current year
  W_Fires_year <- W_Fires_filtered[W_Fires_filtered$year == year_i, ]
  W_FLs_year <- W_FLs_filtered[W_FLs_filtered$year == year_i, ]
  
  # Buffer operations
  W_Fires_add60 <- buffer(W_Fires_year, 60) 
  W_Fires_minus60 <- buffer(W_Fires_year, -60)
  W_Fires_EH <- erase(W_Fires_add60, W_Fires_minus60)
  
  # Extract using exactextractr - returns a list of dataframes
  unique_names <- unique(W_Fires_EH$IncidentNa)
  W_Fires_EH <- tidyterra::as_sf(W_Fires_EH) ## need to make into sf objects first
  W_Fires_EH <- sf::as_Spatial(W_Fires_EH$geometry)
  tmp_list <- exact_extract(W_dist, W_Fires_EH, include_cell = TRUE, progress = FALSE)
  
  # Combine list into single dataframe with ID column
  tmp <- do.call(rbind, lapply(seq_along(tmp_list), function(j) {
    df <- tmp_list[[j]]
    df$ID <- j
    return(df)
  }))
  
  # Match fire names
  tmp$fire.names <- unique_names[tmp$ID]
  tmp$year <- year_i
  
  Treatment_Boundary_list[[i]] <- tmp
  
  # Extract using exactextractr - returns a list of dataframes
  unique_names <- unique(W_Fires_minus60$IncidentNa)
  W_Fires_minus60 <- tidyterra::as_sf(W_Fires_minus60) ## need to make into sf objects first
  ID_match <- sf::st_is_empty(W_Fires_minus60$geometry)
  W_Fires_minus60 <- W_Fires_minus60[!sf::st_is_empty(W_Fires_minus60$geometry),]
  W_Fires_minus60 <- sf::as_Spatial(W_Fires_minus60$geometry)
  tmp_list <- exact_extract(W_dist, W_Fires_minus60, include_cell = TRUE, progress = FALSE)
  
  # Combine list into single dataframe with ID column
  tmp <- do.call(rbind, lapply(seq_along(tmp_list), function(j) {
    df <- tmp_list[[j]]
    df$ID <- j
    return(df)
  }))
  
  # Match fire names
  tmp$fire.names <- unique_names[ID_match == FALSE][tmp$ID]
  tmp$year <- year_i
  
  Inside_Treatment_list[[i]] <- tmp
  
  # Intersect operations
  W_Fires_minus60 <- vect(W_Fires_minus60)
  W_Fires_EH <- vect(W_Fires_EH)
  W_FLs_EH <- intersect(W_FLs_year, W_Fires_EH)
  W_FLs_EF <- intersect(W_FLs_year, W_Fires_minus60)
  
  # make lines into small polygons
  W_FLs_EH <- buffer(W_FLs_EH, 1) 
  W_FLs_EF <- buffer(W_FLs_EF, 1) 
  
  # Extract with xy coordinates for fire lines
  # exactextractr returns coverage fraction by default, use include_xy for coordinates
  W_FLs_EH <- tidyterra::as_sf(W_FLs_EH) ## need to make into sf objects first
  W_FLs_EH <- sf::as_Spatial(W_FLs_EH$geometry)
  extracted_FLs_EH_list <- exact_extract(W_dist, W_FLs_EH, include_xy = TRUE, progress = FALSE)
  
  W_FLs_EF <- tidyterra::as_sf(W_FLs_EF) ## need to make into sf objects first
  W_FLs_EF <- sf::as_Spatial(W_FLs_EF$geometry)
  extracted_FLs_EF_list <- exact_extract(W_dist, W_FLs_EF, include_xy = TRUE, progress = FALSE)
  
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
}

Treatment_Boundary <- do.call(rbind, Treatment_Boundary_list)
Inside_Treatment <- do.call(rbind, Inside_Treatment_list)
Engaged_Lines <- do.call(rbind, Engaged_Lines_list)

table(Treatment_Boundary$year)
length(unique(Treatment_Boundary$fire.names))

table(Inside_Treatment$year)
length(unique(Inside_Treatment$fire.names))

table(Engaged_Lines$year)


rm(EF);rm(EH);rm(extracted_FLs_EF);rm(extracted_FLs_EH);rm(W_dist);rm(W_Fires)
rm(W_Fires_add60);rm(W_Fires_EH);rm(W_Fires_minus60);rm(W_FLs);rm(W_FLs_EF);rm(W_FLs_EH)
rm(WesternStates);rm(i);rm(temp);rm(vec);rm(tmp);rm(unique_names);rm(year_i)
rm(Engaged_Lines_list);rm(extracted_FLs_EF_list);rm(extracted_FLs_EH_list);rm(tmp_list)
rm(Inside_Treatment_list);rm(Treatment_Boundary_list);rm(W_Fires_filtered);rm(W_Fires_year)
rm(W_FLs_filtered);rm(W_FLs_year);rm(ID_match);rm(unique_ID)
gc()

#### Finalizing Treatment History Data ####
D_csv <- read.csv("D:/Outside Boundary/LandFire csvs/LF_total_dist.csv")

table(D_csv$DIST_TYPE)
setwd("D:/Outside Boundary")
# write.csv(Engaged_Lines, "Engaged_Lines09162025.csv")

colnames(Engaged_Lines) <- c("2018","2019","2020","2021","2022","2023","x","y","coverage.fraction", "stat","year")

vec <- ncol(Engaged_Lines[,c(1:6)]) ## need to change the number of columns
## use a master csv that matches the year of dist to find the appropriate code for that year.
for(i in 1:vec){
  tmp <- D_csv[D_csv$DIST_YEAR == as.numeric(colnames(Engaged_Lines[i])),]
  Engaged_Lines[,i+1] <- tmp$DIST_TYPE[match(Engaged_Lines[,i], tmp$VALUE)]
}

Engaged_Lines$history <- NA

## need to adjust the number of columns
Engaged_Lines$history <- apply(Engaged_Lines, 1, function(row) {
    year <- as.numeric(row["year"])
    cols_to_use <- 2:27 ## if different number of years, need to adjust the number of columns
    col_years <- as.numeric(colnames(Engaged_Lines)[cols_to_use])
    valid_cols <- cols_to_use[col_years < year]
    stringr::str_flatten(row[valid_cols], collapse = ", ", na.rm = TRUE)
  })
table(Engaged_Lines$history)
write.csv(Engaged_Lines,"Engaged_Lines_DisturbanceHistory09162025_SR22_23.csv")
# FireChecking <- Engaged_Lines[grep("Fire",Engaged_Lines$history),]

## can do the same workflow for the other two
unique(Inside_Treatment$ID)
unique(Treatment_Boundary$ID)
