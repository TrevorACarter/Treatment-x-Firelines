library(terra)
# WesternStates <- vect("D:/Outside Boundary/Census State Boundaries/WesternStates.shp")
WesternForests <- vect("D:/Outside Boundary/US EPA Ecoregions/WesternForestEcoregions.shp")

#### Adding Raster files from LandFire (1999-2024) ####
setwd("D:/Outside Boundary/LandFire TIFs")
temp <- list.files(pattern="*.tif") ## creating a vector that has all the files in the working directory with .xlsx extensions
temp <- temp[1:26]

for(i in 1:length(temp)) assign(temp[i], terra::rast(temp[i])) ## loading in the shapefiles I want

## stacking the rasters
dist_stack <- c(LF99_Dist.tif,
                LF00_Dist.tif,LF01_Dist.tif,LF02_Dist.tif,LF03_Dist.tif,LF04_Dist.tif,LF05_Dist.tif,LF06_Dist.tif,LF07_Dist.tif,LF08_Dist.tif,LF09_Dist.tif,
                LF10_Dist.tif,LF11_Dist.tif,LF12_Dist.tif,LF13_Dist.tif,LF14_Dist.tif,LF15_Dist.tif,LF16_Dist.tif,LF17_Dist.tif,LF18_Dist.tif,LF19_Dist.tif,
                LF20_Dist.tif,LF21_Dist.tif,LF22_Dist.tif,LF23_Dist.tif,LF24_Dist.tif)
rm(LF99_Dist.tif)
rm(LF00_Dist.tif);rm(LF01_Dist.tif);rm(LF02_Dist.tif);rm(LF03_Dist.tif);rm(LF04_Dist.tif);rm(LF05_Dist.tif);rm(LF06_Dist.tif);rm(LF07_Dist.tif);rm(LF08_Dist.tif);rm(LF09_Dist.tif)
rm(LF10_Dist.tif);rm(LF11_Dist.tif);rm(LF12_Dist.tif);rm(LF13_Dist.tif);rm(LF14_Dist.tif);rm(LF15_Dist.tif);rm(LF16_Dist.tif);rm(LF17_Dist.tif);rm(LF18_Dist.tif);rm(LF19_Dist.tif)
rm(LF20_Dist.tif);rm(LF21_Dist.tif);rm(LF22_Dist.tif);rm(LF23_Dist.tif);rm(LF24_Dist.tif)

dist_crop <- crop(dist_stack, WesternForests)
gc()
WF_dist <- mask(dist_crop, WesternForests)
gc()
plot(WF_dist[[1]])

rm(dist_crop);rm(dist_stack)
getwd()
writeRaster(WF_dist, "WF_dist.tif", overwrite = TRUE)


#### Adding NIFC Fire Line Data (2018-2024) ####  
setwd("D:/Outside Boundary/NIFC Lines")
temp <- list.files(pattern="*.shp") ## creating a vector that has all the files in the working directory with .xlsx extensions
temp <- temp[1:7]

for(i in 1:length(temp)) assign(temp[i], terra::vect(temp[i])) ## loading in the shapefiles I want

EventLine2018.shp$year <- 2018
EventLine2019.shp$year <- 2019
EventLine2020.shp$year <- 2020
EventLine2021.shp$year <- 2021
EventLine2022.shp$year <- 2022
EventLine2023.shp$year <- 2023
EventLine2024.shp$year <- 2024

stacked_FL <- rbind(EventLine2018.shp,EventLine2019.shp,EventLine2020.shp,EventLine2021.shp,EventLine2022.shp,EventLine2023.shp,EventLine2024.shp)
rm(EventLine2018.shp);rm(EventLine2019.shp);rm(EventLine2020.shp);rm(EventLine2021.shp);rm(EventLine2022.shp);rm(EventLine2023.shp);rm(EventLine2024.shp)
gc()

# W_FLs <- crop(stacked_FL,WesternStates)
WF_FLs <- crop(stacked_FL,WesternForests)
gc();rm(stacked_FL);rm(WesternForests)
WF_FLs <- terra::unique(WF_FLs)
gc()

vec <- values(WF_FLs)
colnames(vec) ## Did a double check with all years, confirmed that Feature Cat is only needed column to query
table(unique(vec$year))
table(vec$FeatureCat)
rm(vec);gc()

WF_FLs <- WF_FLs[WF_FLs$FeatureCat == "Completed Burnout" |
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

getwd()
writeVector(WF_FLs, "WF_FLs.shp", overwrite = TRUE)

#### Adding Fire Polygon Data from NIFC (2018-2024) ####
setwd("D:/Outside Boundary/NIFC Polygons")
temp <- list.files(pattern="*.shp") ## creating a vector that has all the files in the working directory with .xlsx extensions
temp ## if W_FLs.shp is here remove
temp <- temp[1:7]

for(i in 1:length(temp)) assign(temp[i], terra::vect(temp[i])) ## loading in the shapefiles I want

EventPolygon2018.shp$year <- 2018
length(unique(EventPolygon2018.shp$IncidentNa)) ## 20198
EventPolygon2018.shp$IncidentNa <- tolower(gsub("[[:punct:][:space:]]", "", EventPolygon2018.shp$IncidentNa))
EventPolygon2018.shp <- EventPolygon2018.shp[!grepl("rx",EventPolygon2018.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2018.shp <- EventPolygon2018.shp[!grepl("pileburn",EventPolygon2018.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2018.shp <- EventPolygon2018.shp[!grepl("falsealarm",EventPolygon2018.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2018.shp <- EventPolygon2018.shp[!grepl("baer",EventPolygon2018.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2018.shp <- EventPolygon2018.shp[!grepl("test",EventPolygon2018.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2018.shp <- EventPolygon2018.shp[!grepl("delete",EventPolygon2018.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2018.shp <- EventPolygon2018.shp[order(EventPolygon2018.shp$GISAcres, decreasing = TRUE),] ## sorting by size
EventPolygon2018.shp <- EventPolygon2018.shp[!duplicated(EventPolygon2018.shp$IncidentNa),] ## keeping the largest fire per each name (there are errors in here)
length(unique(EventPolygon2018.shp$IncidentNa)) ## 18146
EventPolygon2018.shp <- EventPolygon2018.shp[!EventPolygon2018.shp$GISAcres < 1,]
length(unique(EventPolygon2018.shp$IncidentNa)) ## 6777
gc()

EventPolygon2019.shp$year <- 2019
length(unique(EventPolygon2019.shp$IncidentNa)) ## 23722
EventPolygon2019.shp$IncidentNa <- tolower(gsub("[[:punct:][:space:]]", "", EventPolygon2019.shp$IncidentNa))
EventPolygon2019.shp <- EventPolygon2019.shp[!grepl("rx",EventPolygon2019.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2019.shp <- EventPolygon2019.shp[!grepl("pileburn",EventPolygon2019.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2019.shp <- EventPolygon2019.shp[!grepl("falsealarm",EventPolygon2019.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2019.shp <- EventPolygon2019.shp[!grepl("baer",EventPolygon2019.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2019.shp <- EventPolygon2019.shp[!grepl("test",EventPolygon2019.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2019.shp <- EventPolygon2019.shp[!grepl("delete",EventPolygon2019.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2019.shp <- EventPolygon2019.shp[order(EventPolygon2019.shp$GISAcres, decreasing = TRUE),] ## sorting by size
EventPolygon2019.shp <- EventPolygon2019.shp[!duplicated(EventPolygon2019.shp$IncidentNa),] ## keeping the largest fire per each name (there are errors in here)
length(unique(EventPolygon2019.shp$IncidentNa)) ## 20717
EventPolygon2019.shp <- EventPolygon2019.shp[!EventPolygon2019.shp$GISAcres < 1,]
length(unique(EventPolygon2019.shp$IncidentNa)) ## 7342
gc()

EventPolygon2020.shp$year <- 2020
length(unique(EventPolygon2020.shp$IncidentNa)) ## 40386
EventPolygon2020.shp$IncidentNa <- tolower(gsub("[[:punct:][:space:]]", "", EventPolygon2020.shp$IncidentNa))
EventPolygon2020.shp <- EventPolygon2020.shp[!grepl("rx",EventPolygon2020.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2020.shp <- EventPolygon2020.shp[!grepl("pileburn",EventPolygon2020.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2020.shp <- EventPolygon2020.shp[!grepl("falsealarm",EventPolygon2020.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2020.shp <- EventPolygon2020.shp[!grepl("baer",EventPolygon2020.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2020.shp <- EventPolygon2020.shp[!grepl("test",EventPolygon2020.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2020.shp <- EventPolygon2020.shp[!grepl("delete",EventPolygon2020.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2020.shp <- EventPolygon2020.shp[order(EventPolygon2020.shp$GISAcres, decreasing = TRUE),] ## sorting by size
EventPolygon2020.shp <- EventPolygon2020.shp[!duplicated(EventPolygon2020.shp$IncidentNa),] ## keeping the largest fire per each name (there are errors in here)
length(unique(EventPolygon2020.shp$IncidentNa)) ## 35882
EventPolygon2020.shp <- EventPolygon2020.shp[!EventPolygon2020.shp$GISAcres < 1,]
length(unique(EventPolygon2020.shp$IncidentNa)) ## 14459
gc()

EventPolygon2021.shp$year <- 2021
length(unique(EventPolygon2021.shp$IncidentNa)) ## 41647
EventPolygon2021.shp <- EventPolygon2021.shp[EventPolygon2021.shp$IncidentNa != "InTerNaTiOnAl fAlLs WaTeR tOwEr \xed\xa0\xbd\xed\xb7\xbc",] ## this name is causing alot of trouble, so I removed it
EventPolygon2021.shp <- EventPolygon2021.shp[EventPolygon2021.shp$IncidentNa != "fire \xed\xa0\xbd\xed\xb4\xa5 \xed\xa0\xbd\xed\xb3\x9b \xed\xa0\xbd\xed\xb1\xa8‍\xed\xa0\xbd\xed\xba\x92",] ## this name is causing alot of trouble, so I removed it
EventPolygon2021.shp$IncidentNa <- tolower(gsub("[[:punct:][:space:]]", "", EventPolygon2021.shp$IncidentNa))
EventPolygon2021.shp <- EventPolygon2021.shp[!grepl("rx",EventPolygon2021.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2021.shp <- EventPolygon2021.shp[!grepl("pileburn",EventPolygon2021.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2021.shp <- EventPolygon2021.shp[!grepl("falsealarm",EventPolygon2021.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2021.shp <- EventPolygon2021.shp[!grepl("baer",EventPolygon2021.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2021.shp <- EventPolygon2021.shp[!grepl("test",EventPolygon2021.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2021.shp <- EventPolygon2021.shp[!grepl("delete",EventPolygon2021.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2021.shp <- EventPolygon2021.shp[order(EventPolygon2021.shp$GISAcres, decreasing = TRUE),] ## sorting by size
EventPolygon2021.shp <- EventPolygon2021.shp[!duplicated(EventPolygon2021.shp$IncidentNa),] ## keeping the largest fire per each name (there are errors in here)
length(unique(EventPolygon2021.shp$IncidentNa)) ## 37317
EventPolygon2021.shp <- EventPolygon2021.shp[!EventPolygon2021.shp$GISAcres < 1,]
length(unique(EventPolygon2021.shp$IncidentNa)) ## 15803
gc()

EventPolygon2022.shp$year <- 2022
length(unique(EventPolygon2022.shp$IncidentNa)) ## 33027
EventPolygon2022.shp$IncidentNa <- tolower(gsub("[[:punct:][:space:]]", "", EventPolygon2022.shp$IncidentNa))
EventPolygon2022.shp <- EventPolygon2022.shp[!grepl("rx",EventPolygon2022.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2022.shp <- EventPolygon2022.shp[!grepl("pileburn",EventPolygon2022.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2022.shp <- EventPolygon2022.shp[!grepl("falsealarm",EventPolygon2022.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2022.shp <- EventPolygon2022.shp[!grepl("baer",EventPolygon2022.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2022.shp <- EventPolygon2022.shp[!grepl("test",EventPolygon2022.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2022.shp <- EventPolygon2022.shp[!grepl("delete",EventPolygon2022.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2022.shp <- EventPolygon2022.shp[order(EventPolygon2022.shp$GISAcres, decreasing = TRUE),] ## sorting by size
EventPolygon2022.shp <- EventPolygon2022.shp[!duplicated(EventPolygon2022.shp$IncidentNa),] ## keeping the largest fire per each name (there are errors in here)
length(unique(EventPolygon2022.shp$IncidentNa)) ## 29943
EventPolygon2022.shp <- EventPolygon2022.shp[!EventPolygon2022.shp$GISAcres < 1,]
length(unique(EventPolygon2022.shp$IncidentNa)) ## 10175
gc()

EventPolygon2023.shp$year <- 2023
length(unique(EventPolygon2023.shp$IncidentNa)) ## 40903
EventPolygon2023.shp$IncidentNa <- tolower(gsub("[[:punct:][:space:]]", "", EventPolygon2023.shp$IncidentNa))
EventPolygon2023.shp <- EventPolygon2023.shp[!grepl("rx",EventPolygon2023.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2023.shp <- EventPolygon2023.shp[!grepl("pileburn",EventPolygon2023.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2023.shp <- EventPolygon2023.shp[!grepl("falsealarm",EventPolygon2023.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2023.shp <- EventPolygon2023.shp[!grepl("baer",EventPolygon2023.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2023.shp <- EventPolygon2023.shp[!grepl("test",EventPolygon2023.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2023.shp <- EventPolygon2023.shp[!grepl("delete",EventPolygon2023.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2023.shp <- EventPolygon2023.shp[order(EventPolygon2023.shp$GISAcres, decreasing = TRUE),] ## sorting by size
EventPolygon2023.shp <- EventPolygon2023.shp[!duplicated(EventPolygon2023.shp$IncidentNa),] ## keeping the largest fire per each name (there are errors in here)
length(unique(EventPolygon2023.shp$IncidentNa)) ## 33869
EventPolygon2023.shp <- EventPolygon2023.shp[!EventPolygon2023.shp$GISAcres < 1,]
length(unique(EventPolygon2023.shp$IncidentNa)) ## 10836
gc()

EventPolygon2024.shp$year <- 2024
length(unique(EventPolygon2024.shp$IncidentNa)) ## 42394
EventPolygon2024.shp$IncidentNa <- tolower(gsub("[[:punct:][:space:]]", "", EventPolygon2024.shp$IncidentNa))
EventPolygon2024.shp <- EventPolygon2024.shp[!grepl("rx",EventPolygon2024.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2024.shp <- EventPolygon2024.shp[!grepl("pileburn",EventPolygon2024.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2024.shp <- EventPolygon2024.shp[!grepl("falsealarm",EventPolygon2024.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2024.shp <- EventPolygon2024.shp[!grepl("baer",EventPolygon2024.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2024.shp <- EventPolygon2024.shp[!grepl("test",EventPolygon2024.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2024.shp <- EventPolygon2024.shp[!grepl("delete",EventPolygon2024.shp$IncidentNa, ignore.case = TRUE),]
EventPolygon2024.shp <- EventPolygon2024.shp[order(EventPolygon2024.shp$GISAcres, decreasing = TRUE),] ## sorting by size
EventPolygon2024.shp <- EventPolygon2024.shp[!duplicated(EventPolygon2024.shp$IncidentNa),] ## keeping the largest fire per each name (there are errors in here)
length(unique(EventPolygon2024.shp$IncidentNa)) ## 33643
EventPolygon2024.shp <- EventPolygon2024.shp[!EventPolygon2024.shp$GISAcres < 1,]
length(unique(EventPolygon2024.shp$IncidentNa)) ## 11368
gc()

EventPolygon2018.shp <- EventPolygon2018.shp[,c(colnames(values(EventPolygon2018.shp)) == "IncidentNa" | 
                                                  colnames(values(EventPolygon2018.shp)) == "GISAcres" |
                                                  colnames(values(EventPolygon2018.shp)) == "year")]
gc()
EventPolygon2019.shp <- EventPolygon2019.shp[,c(colnames(values(EventPolygon2019.shp)) == "IncidentNa" | 
                                                     colnames(values(EventPolygon2019.shp)) == "GISAcres" |
                                                     colnames(values(EventPolygon2019.shp)) == "year")]
gc()
EventPolygon2020.shp <- EventPolygon2020.shp[,c(colnames(values(EventPolygon2020.shp)) == "IncidentNa" | 
                                                  colnames(values(EventPolygon2020.shp)) == "GISAcres" |
                                                  colnames(values(EventPolygon2020.shp)) == "year")]
gc()
EventPolygon2021.shp <- EventPolygon2021.shp[,c(colnames(values(EventPolygon2021.shp)) == "IncidentNa" | 
                                                  colnames(values(EventPolygon2021.shp)) == "GISAcres" |
                                                  colnames(values(EventPolygon2021.shp)) == "year")]
gc()
EventPolygon2022.shp <- EventPolygon2022.shp[,c(colnames(values(EventPolygon2022.shp)) == "IncidentNa" | 
                                                  colnames(values(EventPolygon2022.shp)) == "GISAcres" |
                                                  colnames(values(EventPolygon2022.shp)) == "year")]
gc()
EventPolygon2023.shp <- EventPolygon2023.shp[,c(colnames(values(EventPolygon2023.shp)) == "IncidentNa" | 
                                                  colnames(values(EventPolygon2023.shp)) == "GISAcres" |
                                                  colnames(values(EventPolygon2023.shp)) == "year")]
gc()
EventPolygon2024.shp <- EventPolygon2024.shp[,c(colnames(values(EventPolygon2024.shp)) == "IncidentNa" | 
                                                  colnames(values(EventPolygon2024.shp)) == "GISAcres" |
                                                  colnames(values(EventPolygon2024.shp)) == "year")]
gc()
colnames(values(EventPolygon2018.shp))
colnames(values(EventPolygon2019.shp))
colnames(values(EventPolygon2020.shp))
colnames(values(EventPolygon2021.shp))
colnames(values(EventPolygon2022.shp))
colnames(values(EventPolygon2023.shp))
colnames(values(EventPolygon2024.shp))

head(values(EventPolygon2021.shp))
gc()

FirePoly <- rbind(EventPolygon2018.shp,EventPolygon2019.shp,EventPolygon2020.shp,EventPolygon2021.shp,EventPolygon2022.shp,EventPolygon2023.shp,EventPolygon2024.shp)
rm(EventPolygon2018.shp);rm(EventPolygon2019.shp);rm(EventPolygon2020.shp);rm(EventPolygon2021.shp);rm(EventPolygon2022.shp);rm(EventPolygon2023.shp);rm(EventPolygon2024.shp)
gc() ## fire poly has 2021 as of now


table(terra::is.valid(FirePoly)) ## checking the validity of geometery
## have several thousand invalid topologies
FirePoly <- terra::makeValid(FirePoly)
table(terra::is.valid(FirePoly)) ## checking the validity of geometery
## validated
gc()

# W_Fires <- crop(FirePoly,WesternStates)
WF_Fires <- crop(FirePoly,WesternForests)
gc();rm(FirePoly);rm(WesternForests)
WF_Fires <- terra::unique(WF_Fires)
gc()

vec <- values(WF_Fires) ## wow, down to < 40k fires
colnames(vec)
table(unique(vec$year))
table(is.na(vec$GISAcres)) 

plot(WF_Fires)

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
table(WF_Fires$IncidentNa)
plot(WF_Fires)
gc()

getwd()
# writeVector(WF_Fires,"WF_Fires.shp", overwrite = TRUE)

gc()

#### Mid-way point for extracting the disturbance history for fires and fire lines ####
library(sf)
library(terra)
library(exactextractr)
W_Fires <- vect("D:/Outside Boundary/NIFC Polygons/WF_Fires.shp")
# W_FLs <- vect("D:/Outside Boundary/NIFC Lines/WF_FLs.shp")
W_dist <- rast("D:/Outside Boundary/LandFire TIFs/WF_dist.tif")
gc()


#### Extracting Treatment History for Fire Lines ####
vec <- seq(2018,2024, by = 1) ## change to reflect data range
i <- 4
# Engaged_Lines <- NA
Treatment_Boundary <- NA
Inside_Treatment <- NA

# Pre-allocate list to store results
Treatment_Boundary_list <- vector("list", length(vec))
Inside_Treatment_list <- vector("list", length(vec))
# Engaged_Lines_list <- vector("list", length(vec) * 2)

# Pre-filter data once
W_Fires_filtered <- W_Fires[W_Fires$year %in% vec, ]
# W_FLs_filtered <- W_FLs[W_FLs$year %in% vec, ]

# for(i in seq_along(vec)){
year_i <- vec[i]

# Filter for current year
W_Fires_year <- W_Fires_filtered[W_Fires_filtered$year == year_i, ]
# W_FLs_year <- W_FLs_filtered[W_FLs_filtered$year == year_i, ]

# Buffer operations
W_Fires_add60 <- buffer(W_Fires_year, 60) 
W_Fires_minus60 <- buffer(W_Fires_year, -60)
W_Fires_EH <- erase(W_Fires_add60, W_Fires_minus60)

# Extract using exactextractr - returns a list of dataframes
unique_names <- unique(W_Fires_EH$IncidentNa)
geom_df <- geom(W_Fires_EH) ## Needed for the 2021 oddness
attr_df <- as.data.frame(W_Fires_EH)
sf_object <- create_polygon_sf(geom_df, attr_df, crs = crs(W_Fires_EH))
# W_Fires_EH <- tidyterra::as_sf(W_Fires_EH) ## need to make into sf objects first
W_Fires_EH <- sf::as_Spatial(sf_object$geometry) ## replace w/ sf_object if given trouble
tmp_list <- exact_extract(W_dist, W_Fires_EH, include_cell = TRUE, progress = TRUE)

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
gc()

# Extract using exactextractr - returns a list of dataframes
unique_names <- unique(W_Fires_minus60$IncidentNa)
W_Fires_minus60 <- tidyterra::as_sf(W_Fires_minus60) ## need to make into sf objects first
ID_match <- sf::st_is_empty(W_Fires_minus60$geometry)
W_Fires_minus60 <- W_Fires_minus60[!sf::st_is_empty(W_Fires_minus60$geometry),]
W_Fires_minus60 <- sf::as_Spatial(W_Fires_minus60$geometry)
tmp_list <- exact_extract(W_dist, W_Fires_minus60, include_cell = TRUE, progress = TRUE)

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
gc()

# # Intersect operations
# W_Fires_minus60 <- vect(W_Fires_minus60)
# W_Fires_EH <- vect(W_Fires_EH)
# W_FLs_EH <- intersect(W_FLs_year, W_Fires_EH)
# W_FLs_EF <- intersect(W_FLs_year, W_Fires_minus60)
# 
# # make lines into small polygons
# W_FLs_EH <- buffer(W_FLs_EH, 1)
# W_FLs_EF <- buffer(W_FLs_EF, 1)
# 
# # Extract with xy coordinates for fire lines
# # exactextractr returns coverage fraction by default, use include_xy for coordinates
# W_FLs_EH <- tidyterra::as_sf(W_FLs_EH) ## need to make into sf objects first
# W_FLs_EH <- sf::as_Spatial(W_FLs_EH$geometry)
# extracted_FLs_EH_list <- exact_extract(W_dist, W_FLs_EH, include_xy = TRUE, progress = TRUE)
# # extracted_FLs_EH_list <- terra::extract(W_dist, W_FLs_EH, xy = TRUE)
# 
# W_FLs_EF <- tidyterra::as_sf(W_FLs_EF) ## need to make into sf objects first
# W_FLs_EF <- sf::as_Spatial(W_FLs_EF$geometry)
# extracted_FLs_EF_list <- exact_extract(W_dist, W_FLs_EF, include_xy = TRUE, progress = TRUE)
# # extracted_FLs_EF_list <- terra::extract(W_dist, W_FLs_EF, xy = TRUE)
# 
# # Combine into dataframes
# extracted_FLs_EH <- do.call(rbind, extracted_FLs_EH_list)
# extracted_FLs_EF <- do.call(rbind, extracted_FLs_EF_list)
# 
# # Get unique values
# EF <- unique(extracted_FLs_EF)
# EH <- unique(extracted_FLs_EH)
# 
# EF$Stat <- "EF"
# EF$year <- year_i
# EH$Stat <- "EH"
# EH$year <- year_i
# 
# Engaged_Lines_list[[2*i - 1]] <- EF
# Engaged_Lines_list[[2*i]] <- EH

print(year_i)

gc()

Treatment_Boundary <- do.call(rbind, Treatment_Boundary_list);gc()
Inside_Treatment <- do.call(rbind, Inside_Treatment_list);gc()
# Engaged_Lines <- do.call(rbind, Engaged_Lines_list);gc()

table(Treatment_Boundary$year)
length(unique(Treatment_Boundary$fire.names))

table(Inside_Treatment$year)
length(unique(Inside_Treatment$fire.names))

# table(Engaged_Lines$year)
# table(Engaged_Lines$Stat)

rm(EF);rm(EH);rm(extracted_FLs_EF);rm(extracted_FLs_EH);rm(W_dist);rm(W_Fires)
rm(W_Fires_add60);rm(W_Fires_EH);rm(W_Fires_minus60);rm(W_FLs);rm(W_FLs_EF);rm(W_FLs_EH)
rm(i);rm(vec);rm(tmp);rm(unique_names);rm(year_i)
rm(Engaged_Lines_list);rm(extracted_FLs_EF_list);rm(extracted_FLs_EH_list);rm(tmp_list)
rm(Inside_Treatment_list);rm(Treatment_Boundary_list);rm(W_Fires_filtered);rm(W_Fires_year)
rm(W_FLs_filtered);rm(W_FLs_year);rm(ID_match);rm(unique_ID)
gc()

setwd("D:/Outside Boundary")
# write.csv(Engaged_Lines, "Engaged_Lines2021.csv")
# gc()
write.csv(Treatment_Boundary, "Treatment_Boundary2021.csv")
gc()
write.csv(Inside_Treatment, "Inside_Treatment2021.csv")
gc()


#### Engaged Lines Treatment History Data Cleaning ####
D_csv <- read.csv("D:/Outside Boundary/LandFire csvs/LF_total_dist.csv")
setwd("D:/Outside Boundary")

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

colnames(Engaged_Lines) <- c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","x","y", "cov_frac","stat","year")
head(Engaged_Lines)

write.csv(Engaged_Lines, "Engaged_Lines.csv")


vec <- ncol(Engaged_Lines[,c(1:26)]) ## need to change the number of columns
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

table(Engaged_Lines$history)
table(Engaged_Lines$stat)
# EF      EH 
# 1712538 5254178 
nrow(Engaged_Lines[Engaged_Lines$stat == "EF",])/nrow(Engaged_Lines[Engaged_Lines$stat == "EH",])
## 0.3259 (proportion of failed lines)

length(unique(Engaged_Lines$history)) ## 2996 unique disturbance histories
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
D_csv <- read.csv("D:/Outside Boundary/LandFire csvs/LF_total_dist.csv")
setwd("D:/Outside Boundary")

TB18 <- read.csv("Treatment_Boundary2018.csv")
TB19 <- read.csv("Treatment_Boundary2019.csv")
TB20 <- read.csv("Treatment_Boundary2020.csv")
TB21 <- read.csv("Treatment_Boundary2021.csv")
TB22 <- read.csv("Treatment_Boundary2022.csv")
TB23 <- read.csv("Treatment_Boundary2023.csv")
TB24 <- read.csv("Treatment_Boundary2024.csv")

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
trt <- trt[,c(1:26,30,31)]
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

length(unique(trt$history)) ## 5952 unique disturbance histories (609 in 2018)
length(unique(trt$fire.name)) ## 3471 unique fires (400 in 2018)

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
write.csv(fire.perimeter, "Treatment_Boundary.csv")


#### Inside Fire Data ####
D_csv <- read.csv("D:/Outside Boundary/LandFire csvs/LF_total_dist.csv")
setwd("D:/Outside Boundary")

IT18 <- read.csv("Inside_Treatment2018.csv")
colnames(IT18)
trt <- IT18[,c(2:27,31,32)]
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

IT19 <- read.csv("Inside_Treatment2019.csv")
colnames(IT19)
trt <- IT19[,c(2:27,31,32)]
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

IT20 <- read.csv("Inside_Treatment2020.csv")
colnames(IT20)
trt <- IT20[,c(2:27,31,32)]
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

IT21 <- read.csv("Inside_Treatment2021.csv")
colnames(IT21)
trt <- IT21[,c(2:27,31,32)]
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

IT22 <- read.csv("Inside_Treatment2022.csv")
colnames(IT22)
trt <- IT22[,c(2:27,31,32)]
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

IT23 <- read.csv("Inside_Treatment2023.csv")
colnames(IT23)
trt <- IT23[,c(2:27,31,32)]
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

IT24 <- read.csv("Inside_Treatment2024.csv")
colnames(IT24)
trt <- IT24[,c(2:27,31,32)]
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


Inside_Treatment <- rbind(IT18_summary,IT19_summary,IT20_summary,IT21_summary,IT22_summary,IT23_summary,IT24_summary)
gc()
rm(IT18_summary);rm(IT19_summary);rm(IT20_summary);rm(IT21_summary);rm(IT22_summary);rm(IT23_summary);rm(IT24_summary)
gc()

write.csv(Inside_Treatment, "Inside_Treatment.csv")

#### Odds of treatment on a perimeter ####
setwd("D:/Outside Boundary")
IT <- read.csv("Inside_Treatment.csv")
TB <- read.csv("Treatment_Boundary.csv")

head(IT)
length(unique(IT$fire.name))
length(unique(TB$fire.name))

IT$tot <- apply(IT[,c(4:7)],1,sum)
IT$thin.pr <- IT$thin.pa/IT$tot
IT$rx.pr <- IT$rx.pa/IT$tot
IT$b.pr <- IT$b.pa/IT$tot
IT$n.pr <- IT$n.pa/IT$tot
IT$match <- paste(IT$fire.name, IT$year, sep = " ")

TB$tot <- apply(TB[,c(4:7)],1,sum)
TB$thin.pr <- TB$thin.pa/TB$tot
TB$rx.pr <- TB$rx.pa/TB$tot
TB$b.pr <- TB$b.pa/TB$tot
TB$n.pr <- TB$n.pa/TB$tot
TB$match <- paste(TB$fire.name, TB$year, sep = " ")


TB.pr <- TB[match(TB$match, IT$match),c(13, 8:12)]
IT.pr <- IT[match(TB$match, IT$match),c(13, 8:12)]

TB.pr <- TB.pr[complete.cases(TB.pr),]
IT.pr <- IT.pr[complete.cases(IT.pr),] ## 3430 observations

colnames(TB.pr) <- c("name","TB.tot", "TB.thin", "TB.rx", "TB.b", "TB.n")
colnames(IT.pr) <- c("name", "IT.tot", "IT.thin", "IT.rx", "IT.b", "IT.n")

FireTrtHist <- cbind(TB.pr,IT.pr[,c(2:6)])
rm(IT);rm(TB);rm(IT.pr);rm(TB.pr)
FireTrtHist$year <- stringr::str_split_fixed(FireTrtHist$name, " ", n = 2)[,2]
FireTrtHist$name <- stringr::str_split_fixed(FireTrtHist$name, " ", n = 2)[,1]
FireTrtHist$tot <- FireTrtHist$TB.tot + FireTrtHist$IT.tot
FireTrtHist$thin.odds <- (FireTrtHist$TB.thin - FireTrtHist$IT.thin)/(FireTrtHist$TB.thin + FireTrtHist$IT.thin)
FireTrtHist$rx.odds <- (FireTrtHist$TB.rx - FireTrtHist$IT.rx)/(FireTrtHist$TB.rx + FireTrtHist$IT.rx)
FireTrtHist$b.odds <- (FireTrtHist$TB.b - FireTrtHist$IT.b)/(FireTrtHist$TB.b + FireTrtHist$IT.b)
FireTrtHist$n.odds <- (FireTrtHist$TB.n - FireTrtHist$IT.n)/(FireTrtHist$TB.n + FireTrtHist$IT.n)

FireTrtHist <- FireTrtHist[,c(1,c(12:17))]
FireTrtHist <- FireTrtHist[order(FireTrtHist$year, FireTrtHist$tot,FireTrtHist$name),]
hist(FireTrtHist$tot)
FireTrtHist$tot <- (FireTrtHist$tot * 900)/10000 ## now in ha
hist(FireTrtHist$tot[FireTrtHist$tot < 10000])
smallfires <- FireTrtHist[FireTrtHist$tot < 10000,]
megafires <- FireTrtHist[FireTrtHist$tot >= 10000,]

par(mfrow = c(2,1))
## small fires
df <- data.frame(per.eff = c(smallfires$thin.odds,smallfires$rx.odds,smallfires$b.odds,smallfires$n.odds),
                 trt = c(rep("thin", nrow(smallfires)), rep("rx", nrow(smallfires)), rep("b", nrow(smallfires)), rep("n",nrow(smallfires))))
a1 <- aov(per.eff ~ trt, data = df)
summary(a1)
TukeyHSD(a1)
aggregate(per.eff ~ trt, data = df, mean) ## ~ 12% more likely ot be in a perimeter for small fires

se <- function(x, na.rm = FALSE){sd(x, na.rm = na.rm)/sqrt(length(!is.na(x)))} ## creating a function for standard error
plot(x = c(1:4),
     ylim = c(-1.1,1.1),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
axis(1, at = c(1:4), line = 1, tick = F, labels = c("Thin", "Rx", "Both", "Neither"), cex.axis = 1.5)
abline(h = 0, lty = 2)
points(x = jitter(rep(1, length(df$per.eff[df$trt == "thin"])), factor = 2),
       y = df$per.eff[df$trt == "thin"],
       col = rgb(0,0,0, alpha = 0.5),
       pch = 19)
points(x = jitter(rep(2, length(df$per.eff[df$trt == "rx"])), factor = 2),
       y = df$per.eff[df$trt == "rx"],
       col = rgb(0,0,0, alpha = 0.5),
       pch = 19)
points(x = jitter(rep(3, length(df$per.eff[df$trt == "b"])), factor = 2),
       y = df$per.eff[df$trt == "b"],
       col = rgb(0,0,0, alpha = 0.5),
       pch = 19)
points(x = jitter(rep(4, length(df$per.eff[df$trt == "n"])), factor = 2),
       y = df$per.eff[df$trt == "n"],
       col = rgb(0,0,0, alpha = 0.5),
       pch = 19)
points(x = 1,
       y = mean(df$per.eff[df$trt == "thin"], na.rm = TRUE),
       col = rgb(1,0,0, alpha = 1),
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "thin"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "thin"], na.rm = TRUE)), x0 = 1, 
         y1 = (mean(df$per.eff[df$trt == "thin"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "thin"], na.rm = TRUE)), x1 = 1, 
         col = rgb(1,0,0),lwd = 1.5)
points(x = 2,
       y = mean(df$per.eff[df$trt == "rx"], na.rm = TRUE),
       col = rgb(1,0,0, alpha = 1),
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "rx"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "rx"], na.rm = TRUE)), x0 = 2, 
         y1 = (mean(df$per.eff[df$trt == "rx"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "rx"], na.rm = TRUE)), x1 = 2, 
         col = rgb(1,0,0),lwd = 1.5)
points(x = 3,
       y = mean(df$per.eff[df$trt == "b"], na.rm = TRUE),
       col = rgb(1,0,0, alpha = 1),
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "b"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "b"], na.rm = TRUE)), x0 = 3, 
         y1 = (mean(df$per.eff[df$trt == "b"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "b"], na.rm = TRUE)), x1 = 3, 
         col = rgb(1,0,0),lwd = 1.5)
points(x = 4,
       y = mean(df$per.eff[df$trt == "n"], na.rm = TRUE),
       col = rgb(1,0,0, alpha = 1),
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "n"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "n"], na.rm = TRUE)), x0 = 4, 
         y1 = (mean(df$per.eff[df$trt == "n"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "n"], na.rm = TRUE)), x1 = 4, 
         col = rgb(1,0,0),lwd = 1.5)

a1 <- aov(per.eff ~ trt, data = df)
summary(a1)
TukeyHSD(a1)

## mega fires
df <- data.frame(per.eff = c(megafires$thin.odds,megafires$rx.odds,megafires$b.odds,megafires$n.odds),
                 trt = c(rep("thin", nrow(megafires)), rep("rx", nrow(megafires)), rep("b", nrow(megafires)), rep("n",nrow(megafires))))
a1 <- aov(per.eff ~ trt, data = df)
summary(a1)
TukeyHSD(a1)
aggregate(per.eff ~ trt, data = df, mean) ## ~ 40% more likely to be in the interior for large fires


se <- function(x, na.rm = FALSE){sd(x, na.rm = na.rm)/sqrt(length(!is.na(x)))} ## creating a function for standard error
plot(x = c(1:4),
     ylim = c(-1.1,1.1),
     las = 1,
     cex.axis = 1.5,
     ylab = "", ## density
     type = "n",
     xaxt = "n",
     xlab = "") ## disturbance history
axis(1, at = c(1:4), line = 1, tick = F, labels = c("Thin", "Rx", "Both", "Neither"), cex.axis = 1.5)
abline(h = 0, lty = 2)
points(x = jitter(rep(1, length(df$per.eff[df$trt == "thin"])), factor = 2),
       y = df$per.eff[df$trt == "thin"],
       col = rgb(0,0,0, alpha = 0.5),
       pch = 19)
points(x = jitter(rep(2, length(df$per.eff[df$trt == "rx"])), factor = 2),
       y = df$per.eff[df$trt == "rx"],
       col = rgb(0,0,0, alpha = 0.5),
       pch = 19)
points(x = jitter(rep(3, length(df$per.eff[df$trt == "b"])), factor = 2),
       y = df$per.eff[df$trt == "b"],
       col = rgb(0,0,0, alpha = 0.5),
       pch = 19)
points(x = jitter(rep(4, length(df$per.eff[df$trt == "n"])), factor = 2),
       y = df$per.eff[df$trt == "n"],
       col = rgb(0,0,0, alpha = 0.5),
       pch = 19)
points(x = 1,
       y = mean(df$per.eff[df$trt == "thin"], na.rm = TRUE),
       col = rgb(1,0,0, alpha = 1),
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "thin"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "thin"], na.rm = TRUE)), x0 = 1, 
         y1 = (mean(df$per.eff[df$trt == "thin"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "thin"], na.rm = TRUE)), x1 = 1, 
         col = rgb(1,0,0),lwd = 1.5)
points(x = 2,
       y = mean(df$per.eff[df$trt == "rx"], na.rm = TRUE),
       col = rgb(1,0,0, alpha = 1),
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "rx"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "rx"], na.rm = TRUE)), x0 = 2, 
         y1 = (mean(df$per.eff[df$trt == "rx"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "rx"], na.rm = TRUE)), x1 = 2, 
         col = rgb(1,0,0),lwd = 1.5)
points(x = 3,
       y = mean(df$per.eff[df$trt == "b"], na.rm = TRUE),
       col = rgb(1,0,0, alpha = 1),
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "b"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "b"], na.rm = TRUE)), x0 = 3, 
         y1 = (mean(df$per.eff[df$trt == "b"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "b"], na.rm = TRUE)), x1 = 3, 
         col = rgb(1,0,0),lwd = 1.5)
points(x = 4,
       y = mean(df$per.eff[df$trt == "n"], na.rm = TRUE),
       col = rgb(1,0,0, alpha = 1),
       pch = 16)
segments(y0 = (mean(df$per.eff[df$trt == "n"], na.rm = TRUE)-1.96*se(df$per.eff[df$trt == "n"], na.rm = TRUE)), x0 = 4, 
         y1 = (mean(df$per.eff[df$trt == "n"], na.rm = TRUE)+1.96*se(df$per.eff[df$trt == "n"], na.rm = TRUE)), x1 = 4, 
         col = rgb(1,0,0),lwd = 1.5)

a1 <- aov(per.eff ~ trt, data = df)
summary(a1)
TukeyHSD(a1)

## trying something here
test <- FireTrtHist
test$thin.odds <- round(test$thin.odds, 0)
test$rx.odds <- round(test$rx.odds, 0)
test$b.odds <- round(test$b.odds, 0)
test$n.odds <- round(test$n.odds, 0)
test$thin.odds[test$thin.odds == 0] <- NA
test$rx.odds[test$rx.odds == 0] <- NA
test$b.odds[test$b.odds == 0] <- NA
test$n.odds[test$n.odds == 0] <- NA
test$thin.odds[test$thin.odds == -1] <- 0
test$rx.odds[test$rx.odds == -1] <- 0
test$b.odds[test$b.odds == -1] <- 0
test$n.odds[test$n.odds == -1] <- 0
test$year <- as.factor(test$year)


#### Fire Lines Modelling - all vars in one model ####
library(terra)
library(dplyr)
library(caret)
library(randomForest)
library(pROC)

setwd("D:/Outside Boundary")
Engaged_Lines <- read.csv("Engaged_Lines_DisturbanceHistory.csv")
colnames(Engaged_Lines)
Engaged_Lines <- Engaged_Lines[,c(31,32,34:38,28,29)]
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


## Summary of data
table(Engaged_Lines$stat)
mean(ifelse(Engaged_Lines$stat == "EF",1,0))*100
## 25% of the lines failed across all years
table(Engaged_Lines$year)
(table(Engaged_Lines$year)/nrow(Engaged_Lines))*100
table(Engaged_Lines$trt)
(table(Engaged_Lines$trt)/nrow(Engaged_Lines))*100

hist(Engaged_Lines$prop.rx)
min(Engaged_Lines$prop.rx, na.rm = TRUE)
max(Engaged_Lines$prop.rx, na.rm = TRUE)
hist(Engaged_Lines$prop.rx[Engaged_Lines$prop.rx > 0])

hist(Engaged_Lines$prop.thin)
min(Engaged_Lines$prop.thin, na.rm = TRUE)
max(Engaged_Lines$prop.thin, na.rm = TRUE)
hist(Engaged_Lines$prop.thin[Engaged_Lines$prop.thin > 0])


hist(Engaged_Lines$TS.rx)
hist(Engaged_Lines$TS.thin)

## Random Forest - western spatial scale
n <- 20 # number of iterations

prop.rx.x <- matrix(data = NA, nrow = n, ncol = 51)
prop.rx.y <- matrix(data = NA, nrow = n, ncol = 51)

prop.thin.x <- matrix(data = NA, nrow = n, ncol = 51)
prop.thin.y <- matrix(data = NA, nrow = n, ncol = 51)

TS.rx.x <- matrix(data = NA, nrow = n, ncol = 51)
TS.rx.y <- matrix(data = NA, nrow = n, ncol = 51)

TS.thin.x <- matrix(data = NA, nrow = n, ncol = 51)
TS.thin.y <- matrix(data = NA, nrow = n, ncol = 51)

rf.res <- matrix(data = NA, nrow = n, ncol = 10000)

y_hats <- matrix(data = NA, nrow = n, ncol = 10000)
y_hats.diff <- NA
varImp.summary <- matrix(data = NA, nrow = 36, ncol = n)
varImp.names <- matrix(data = NA, nrow = 36, ncol = n)

balance <- NA
error <- matrix(data = NA, nrow = n, ncol = 500) ## ncol = ntree
AUC.val <- NA

r <- rast("LandFire TIFs/WF_dist.tif")
blank <- rast(ext(r), resolution=100, vals=NA) ## gonna expand this
crs(blank) <- crs(r)

trt1 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[1], ]
trt2 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[2], ]
trt3 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[3], ]
trt4 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[4], ]

# level1 <- Engaged_Lines[Engaged_Lines$stat == levels(Engaged_Lines$stat)[1], ]
# level2 <- Engaged_Lines[Engaged_Lines$stat == levels(Engaged_Lines$stat)[2], ]

### THERE IS A LOT OF UNTREATED, MIGHT NEED TO BALANCE THAT TOO

## for loop for the random forest and summary data
for(i in 1:n){
  set.seed(i)
  trt1_sample <- trt1[sample(nrow(trt1), 5000, replace = FALSE), ]
  set.seed(i)
  trt2_sample <- trt2[sample(nrow(trt2), 5000, replace = FALSE), ]
  set.seed(i)
  trt3_sample <- trt3[sample(nrow(trt3), 5000, replace = FALSE), ]
  set.seed(i)
  trt4_sample <- trt4[sample(nrow(trt4), 5000, replace = FALSE), ]
  dat_sub <- rbind(trt1_sample,trt2_sample,trt3_sample,trt4_sample)
  table(dat_sub$stat)
  level1 <- dat_sub[dat_sub$stat == levels(dat_sub$stat)[1], ]
  level2 <- dat_sub[dat_sub$stat == levels(dat_sub$stat)[2], ]
  set.seed(i)
  EF_sample <- level1[sample(nrow(level1), 5000, replace = FALSE), ]
  set.seed(i)
  EH_sample <- level2[sample(nrow(level2), 5000, replace = FALSE), ]
  dat_sub <- rbind(EF_sample, EH_sample)
  table(dat_sub$trt)
  dat.sp <- vect(dat_sub, geom = c("x","y"))
  dat.cell <- extract(blank, dat.sp, cell = TRUE)
  dat_sub$cell <- dat.cell$cell
  dat_sub <- dat_sub %>% group_by(cell) %>% sample_n(size=1) # sample one point per 100 x 100 m cell
  
  ###
  dat_sub <- vect(dat_sub, geom = c("x","y"), crs = crs(blank))
  dat_sub <- project(dat_sub, "EPSG:4326")
  dmat <- as.matrix(dist(cbind(geom(dat_sub)[,4], geom(dat_sub)[,3]))) ## turning the coordinates of each plot into a distance matrix
  dmat <- dmat *111139 ## degrees to meters (approximately)
  num_eigenvectors <- 30
  eigen_res <- RSpectra::eigs_sym(as.matrix(dmat), k = num_eigenvectors)
  dat_sub <- as.data.frame(dat_sub)
  dat_sub <- cbind(dat_sub, eigen_res$vectors)
  dat_sub$cell <- NULL
  colnames(dat_sub)[8:(7+num_eigenvectors)] <- paste("vec",colnames(dat_sub)[8:(7+num_eigenvectors)],sep = "")
  ###
  
  dmat <- as.matrix(dist(cbind(dat_sub$y, dat_sub$x))) #
  vec <- order(dmat[sample(1:nrow(dat_sub),1),]) ## getting rows in order of distance to random point generated
  vec <- vec[c(1:(0.75*nrow(dat_sub)))]
  training_set <- dat_sub[vec,]
  balance[i] <- 1-length(which(training_set$stat == "EF"))/length(training_set$stat)
  testing_set <- dat_sub[-vec,]
  train_index <- createDataPartition(y = dat_sub$stat, p = 0.75, list = FALSE)
  training_set <- dat_sub[train_index,]
  
  set.seed(i)
  rf <- randomForest(stat~.,
                     data = training_set,
                     ntree = 500,
                     maxnodes = 75,
                     maximize = TRUE,
                     trControl = train_control,
                     keep.forest = TRUE,
                     keep.inbag = TRUE) ## making the rf object
  y_hats[i,1:nrow(testing_set)] <- predict(object = rf, newdata = testing_set[, -1])
  y_hats.diff[i] <- mean(as.numeric(y_hats[i,1:nrow(testing_set)]) - as.numeric(testing_set$stat))
  obj <- varImp(rf)
  varImp.summary[,i] <- obj$Overall
  varImp.names[,i] <- rownames(obj)
  rf.res[i,c(1:length(rf$predicted))] <- as.integer(training_set$stat) - as.integer(rf$predicted)
  error[i,] <- rf$err.rate[,1]
  rf.roc <- suppressMessages(roc(training_set$stat, rf$votes[,2]))
  AUC.val[i] <- as.numeric(auc(rf.roc))
  
  training_set <- as.data.frame(training_set)
  prop.rx <- partialPlot(rf, training_set, x.var = prop.rx)
  prop.thin <- partialPlot(rf, training_set, x.var = prop.thin)
  TS.rx <- partialPlot(rf, training_set, x.var = TS.rx)
  TS.thin <- partialPlot(rf, training_set, x.var = TS.thin)

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
y_hats.diff <- y_hats.diff*100 ## converting to %
max(y_hats.diff);min(y_hats.diff)
plot(x = 1:length(y_hats.diff), y = y_hats.diff,
     pch = 16,
     xlab = "model run",
     ylim = c(min(y_hats.diff)-5,max(y_hats.diff)+5),
     las = 1,
     ylab = "% Difference in Predicted vs.Observed",
     cex = 1)
round(mean(y_hats.diff), digits = 3)
abline(h = mean(y_hats.diff), col="firebrick4", lty = 2)
text(x = 30, y = 50, "Average difference = 6.75%") 

mean(balance);min(balance);max(balance)
# 0.740668
# 0.7196563
# 0.7655579

error.mean <- apply(error,2,mean)
min(error);max(error)
plot(error.mean, type = "n",
     ylim = c(0,max(error)+0.05),
     xlab = "Tree",
     ylab = "Error")
for(i in 1:100){
  lines(error[i,], col = rgb(0,0,0,alpha = 0.25))
}
lines(error.mean, type = "l", col = "firebrick", lty = 2, lwd= 2)
mean(error.mean)*100
text(x = 300, y = 0.18, "Average Error = 45.46243")

mean(AUC.val);min(AUC.val);max(AUC.val)
# [1] 0.7672265
# [1] 0.7567746
# [1] 0.781692

## VarImp Plot 
varImp.plotting <- data.frame(name = c(varImp.names[c(1:6),1],"spatial"),
                              mean = c(apply(varImp.summary[c(1:6),],1,mean),mean( varImp.summary[c(7:36),])),
                              min = c(apply(varImp.summary[c(1:6),],1,min),min( varImp.summary[c(7:36),])),
                              max = c(apply(varImp.summary[c(1:6),],1,max),max( varImp.summary[c(7:36),])))
varImp.plotting <- varImp.plotting[order(varImp.plotting$mean, decreasing = FALSE),]

min(varImp.plotting$min)
max(varImp.plotting$max)
par(mfrow = c(1,1), oma = c(0,3,0,0))
plot(varImp.plotting$mean,
     ylim = c(0,8),
     xlim = c(0,max(varImp.plotting$max)+5), ## max of varImp.plotting$max + a few
     las = 1,
     type = "n",
     ylab = "",
     yaxt = "n",
     xlab = "Variable Importance")
axis(2, at = c(1:7), labels = varImp.plotting$name, cex.axis = 1, las = 2)
points(x = varImp.plotting$mean,y = 1:7, col = "black", cex = 1, pch = 16)
segments(x0 = varImp.plotting$min, y0 = 1:7, x1 = varImp.plotting$max, y1 = 1:7, col = "black", lwd = 1.5)
abline(v = 10, lty = 2)

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
     ylim = c(-0.25,1.25),
     col = rgb(0,0,0,0.25),
     main = "",
     cex.axis = 1.5,
     cex.lab = 1.5,
     las = 1,
     xlab = "Proportion Rx Fire",
     ylab = "Line Status")
for(i in 2:n)(
  lines(prop.rx.x[i,], prop.rx.y[i,], col = rgb(0,0,0,0.25))
)
abline(h = 0.5, lty = 2, lwd = 2)
# points(x = FD$X, y = jitter(FD$LineInt, factor = .25),
#        cex = 0.5,
#        col = rgb(0,0,0,0.25),
#        pch = 16)
prop.rx.x.mean <- apply(prop.rx.x,2,mean, na.rm = T)
prop.rx.y.mean <- apply(prop.rx.y,2,mean, na.rm = T)
lo <- loess(prop.rx.y.mean~prop.rx.x.mean)
lines(y = predict(lo), x = prop.rx.x.mean[1:length(predict(lo))], col = "red", lwd = 2)

plot(prop.thin.x[1,], prop.thin.y[1,],
     type = "l",
     ylim = c(-0.25,1.25),
     col = rgb(0,0,0,0.25),
     main = "",
     cex.axis = 1.5,
     cex.lab = 1.5,
     las = 1,
     xlab = "Proportion Thinning",
     ylab = "Line Status")
for(i in 2:n)(
  lines(prop.thin.x[i,], prop.thin.y[i,], col = rgb(0,0,0,0.25))
)
abline(h = 0.5, lty = 2, lwd = 2)
# points(x = FD$X, y = jitter(FD$LineInt, factor = .25),
#        cex = 0.5,
#        col = rgb(0,0,0,0.25),
#        pch = 16)
prop.thin.x.mean <- apply(prop.thin.x,2,mean, na.rm = T)
prop.thin.y.mean <- apply(prop.thin.y,2,mean, na.rm = T)
lo <- loess(prop.thin.y.mean~prop.thin.x.mean)
lines(y = predict(lo), x = prop.thin.x.mean[1:length(predict(lo))], col = "red", lwd = 2)

plot(TS.rx.x[1,], TS.rx.y[1,],
     type = "l",
     ylim = c(-0.25,1.25),
     col = rgb(0,0,0,0.25),
     main = "",
     cex.axis = 1.5,
     cex.lab = 1.5,
     las = 1,
     xlab = "Time Since Rx Fire",
     ylab = "Line Status")
for(i in 2:n)(
  lines(TS.rx.x[i,], TS.rx.y[i,], col = rgb(0,0,0,0.25))
)
abline(h = 0.5, lty = 2, lwd = 2)
# points(x = FD$X, y = jitter(FD$LineInt, factor = .25),
#        cex = 0.5,
#        col = rgb(0,0,0,0.25),
#        pch = 16)
TS.rx.x.mean <- apply(TS.rx.x,2,mean, na.rm = T)
TS.rx.y.mean <- apply(TS.rx.y,2,mean, na.rm = T)
lo <- loess(TS.rx.y.mean~TS.rx.x.mean)
lines(y = predict(lo), x = TS.rx.x.mean[1:length(predict(lo))], col = "red", lwd = 2)

plot(TS.thin.x[1,], TS.thin.y[1,],
     type = "l",
     ylim = c(-0.25,1.25),
     col = rgb(0,0,0,0.25),
     main = "",
     cex.axis = 1.5,
     cex.lab = 1.5,
     las = 1,
     xlab = "Time Since Thinning",
     ylab = "Line Status")
for(i in 2:n)(
  lines(TS.thin.x[i,], TS.thin.y[i,], col = rgb(0,0,0,0.25))
)
abline(h = 0.5, lty = 2, lwd = 2)
# points(x = FD$X, y = jitter(FD$LineInt, factor = .25),
#        cex = 0.5,
#        col = rgb(0,0,0,0.25),
#        pch = 16)
TS.thin.x.mean <- apply(TS.thin.x,2,mean, na.rm = T)
TS.thin.y.mean <- apply(TS.thin.y,2,mean, na.rm = T)
lo <- loess(TS.thin.y.mean~TS.thin.x.mean)
lines(y = predict(lo), x = TS.thin.x.mean[1:length(predict(lo))], col = "red", lwd = 2)

#### splitting rf model into multiple - western USA ####
library(terra)
library(dplyr)
library(caret)
library(randomForest)
library(pROC)

setwd("D:/Outside Boundary")
Engaged_Lines <- read.csv("Engaged_Lines_DisturbanceHistory.csv")
colnames(Engaged_Lines)
Engaged_Lines <- Engaged_Lines[,c(31,32,34:38,28,29)]
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

r <- rast("LandFire TIFs/WF_dist.tif")
blank <- rast(ext(r), resolution=100, vals=NA) ## gonna expand this
crs(blank) <- crs(r)

trt1 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[1], ]
trt2 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[2], ]
trt3 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[3], ]
trt4 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[4], ]


## for loop for the random forest and summary data
for(i in 1:n){
  set.seed(i)
  trt1_sample <- trt1[sample(nrow(trt1), 5000, replace = FALSE), ]
  set.seed(i)
  trt2_sample <- trt2[sample(nrow(trt2), 5000, replace = FALSE), ]
  set.seed(i)
  trt3_sample <- trt3[sample(nrow(trt3), 5000, replace = FALSE), ]
  set.seed(i)
  trt4_sample <- trt4[sample(nrow(trt4), 5000, replace = FALSE), ]
  dat_sub <- rbind(trt1_sample,trt2_sample,trt3_sample,trt4_sample)
  table(dat_sub$stat)
  level1 <- dat_sub[dat_sub$stat == levels(dat_sub$stat)[1], ]
  level2 <- dat_sub[dat_sub$stat == levels(dat_sub$stat)[2], ]
  set.seed(i)
  EF_sample <- level1[sample(nrow(level1), 5000, replace = FALSE), ]
  set.seed(i)
  EH_sample <- level2[sample(nrow(level2), 5000, replace = FALSE), ]
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
# [1] 0.5117817
# [1] 0.4999235
# [1] 0.5246781

mean(balance2);min(balance2);max(balance2) ## average residual error from rf1 (per model run)
# [1] 0.03831505
# [1] -0.02821458
# [1] 0.1057284

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
# [1] 0.7642294
# [1] 0.7529451
# [1] 0.7771144

mean(AUC.val2);min(AUC.val2);max(AUC.val2)
# [1] 0.6296133
# [1] 0.421915
# [1] 0.7039798

r2.mean <- apply(r2,1,mean)
mean(r2.mean);min(r2.mean);max(r2.mean)
## between 1 - 10% of additional variance explained (average 5.5%)
# [1] 0.05530371
# [1] 0.0133965
# [1] 0.09883437

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
     xlim = c(0,max(varImp.plotting2$max)), ## max of varImp.plotting$max + a few
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

#### splitting rf into multiple - southern rockies scale ####
## will need to extract the engaged lines data to southern rockies
# sr <- vect("D:/Outside Boundary/Geographic Subsets/SouthernRockyBoundary_10kmBuff.shp")
# EL <- Engaged_Lines
# Engaged_Lines <- vect(EL, geom = c("x","y"), crs = crs(sr))
# sr_Lines <- crop(Engaged_Lines,sr)
# sr_Lines_df <- as.data.frame(sr_Lines, geom = c("XY"))
# write.csv(sr_Lines_df, "Engaged_Lines_sr.csv")

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

r <- rast("LandFire TIFs/WF_dist.tif")
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
  trt1_sample <- trt1[sample(nrow(trt1), 5000, replace = FALSE), ]
  set.seed(i)
  trt2_sample <- trt2[sample(nrow(trt2), 5000, replace = FALSE), ]
  set.seed(i)
  trt3_sample <- trt3[sample(nrow(trt3), 5000, replace = TRUE), ] ## need to sample replace
  set.seed(i)
  trt4_sample <- trt4[sample(nrow(trt4), 5000, replace = FALSE), ]
  dat_sub <- rbind(trt1_sample,trt2_sample,trt3_sample,trt4_sample)
  table(dat_sub$stat)
  level1 <- dat_sub[dat_sub$stat == levels(dat_sub$stat)[1], ]
  level2 <- dat_sub[dat_sub$stat == levels(dat_sub$stat)[2], ]
  set.seed(i)
  EF_sample <- level1[sample(nrow(level1), 5000, replace = FALSE), ]
  set.seed(i)
  EH_sample <- level2[sample(nrow(level2), 5000, replace = FALSE), ]
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
  AUC.val1_SR[i] <- as.numeric(auc(rf.roc))
  
  ## second model predicting the residuals
  res4pred <- rf1.res[i,!is.na(rf1.res[i,])]
  length(res4pred)
  
  dat_sub2$obs <- as.numeric(dat_sub2$stat)-1
  dat_sub2$pred <- as.numeric(predict(object = rf1, newdata = testing_set[, -1], type = "prob")[,2])
  dat_sub2$stat <- as.numeric(res4pred)
  dat_sub2$obs <- NULL
  dat_sub2$pred <- NULL
  
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
  AUC.val2_SR[i] <- as.numeric(auc(rf.roc))
  r2_SR[i,] <- rf2$rsq
  
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

#### splitting rf into multiple - single fire scale ####
# library(terra)
# cam <- vect("D:/Outside Boundary/Geographic Subsets/CameronPeak.shp")
# EL <- Engaged_Lines
# Engaged_Lines <- vect(EL, geom = c("x","y"), crs = crs(cam))
# cam_Lines <- crop(Engaged_Lines,cam)
# cam_Lines_df <- as.data.frame(cam_Lines, geom = c("XY"))
# write.csv(cam_Lines_df, "Engaged_Lines_CameronPeak.csv")

cam_Lines_df <- read.csv("D:/Outside Boundary/Engaged_Lines_CameronPeak.csv")
table(cam_Lines_df$stat)
(table(cam_Lines_df$stat)/nrow(cam_Lines_df))*100
(table(cam_Lines_df$year)/nrow(cam_Lines_df))*100
(table(cam_Lines_df$trt)/nrow(cam_Lines_df))*100
hist(cam_Lines_df$prop.rx[cam_Lines_df$prop.rx > 0])
hist(cam_Lines_df$prop.thin[cam_Lines_df$prop.thin > 0])
hist(cam_Lines_df$TS.rx[cam_Lines_df$TS.rx < 30])
hist(cam_Lines_df$TS.thin[cam_Lines_df$TS.thin < 30])
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
r2_CP <- matrix(data = NA, nrow = n, ncol = 500)

AUC.val1_CP <- NA
AUC.val2_CP <- NA

r <- rast("LandFire TIFs/WF_dist.tif")
blank <- rast(ext(r), resolution=100, vals=NA) ## gonna expand this
crs(blank) <- crs(r)

str(cam_Lines_df)
cam_Lines_df$X <- NULL
cam_Lines_df$stat <- as.factor(cam_Lines_df$stat)
cam_Lines_df$year <- as.factor(cam_Lines_df$year)
cam_Lines_df$trt <- as.factor(cam_Lines_df$trt)
cam_Lines_df$x <- as.numeric(cam_Lines_df$x)
cam_Lines_df$y <- as.numeric(cam_Lines_df$y)

trt1 <- cam_Lines_df[cam_Lines_df$trt == levels(cam_Lines_df$trt)[1], ]
trt2 <- cam_Lines_df[cam_Lines_df$trt == levels(cam_Lines_df$trt)[2], ]
trt3 <- cam_Lines_df[cam_Lines_df$trt == levels(cam_Lines_df$trt)[3], ]
trt4 <- cam_Lines_df[cam_Lines_df$trt == levels(cam_Lines_df$trt)[4], ]


## for loop for the random forest and summary data
for(i in 1:n){
  set.seed(i)
  trt1_sample <- trt1[sample(nrow(trt1), 5000, replace = TRUE), ]
  set.seed(i)
  trt2_sample <- trt2[sample(nrow(trt2), 5000, replace = TRUE), ]
  set.seed(i)
  trt3_sample <- trt3[sample(nrow(trt3), 5000, replace = TRUE), ] ## need to sample replace
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
  # dmat <- as.matrix(dist(cbind(dat_sub$y, dat_sub$x))) #
  set.seed(i)
  v1 <- sample(which(dat_sub$stat == "EH"), size = (0.75*length(which(dat_sub$stat == "EH"))))
  set.seed(i)
  v2 <- sample(which(dat_sub$stat == "EF"), size = (0.75*length(which(dat_sub$stat == "EF"))))
  vec <- c(v1,v2)
  # vec <- order(dmat[sample(1:nrow(dat_sub),1),]) ## getting rows in order of distance to random point generated
  # test <- vec[c(1:(0.75*nrow(dat_sub)))]
  
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
  AUC.val1_CP[i] <- as.numeric(auc(rf.roc))
  
  ## second model predicting the residuals
  res4pred <- rf1.res[i,!is.na(rf1.res[i,])]
  length(res4pred)
  
  dat_sub2$obs <- as.numeric(dat_sub2$stat)-1
  dat_sub2$pred <- as.numeric(predict(object = rf1, newdata = testing_set[, -1], type = "prob")[,2])
  dat_sub2$stat <- as.numeric(res4pred)
  dat_sub2$obs <- NULL
  dat_sub2$pred <- NULL
  
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
  AUC.val2_CP[i] <- as.numeric(auc(rf.roc))
  r2_CP[i,] <- rf2$rsq
  
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
# [1] 0.7610189
# [1] 0.7474227
# [1] 0.7775446

mean(balance2);min(balance2);max(balance2) ## average residual error from rf1 (per model run)
# [1] 0.01013345
# [1] -0.0140125
# [1] 0.04282067

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
mean(error.mean)*100 # 3.82504
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
mean(error.mean)*100 # 2.47091
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
       y = c(mean(AUC.val1_CP), mean(AUC.val2_CP)),
       pch = 16)
segments(x0 = 1.2, y0 = max(AUC.val1_CP), x1 = 1.2, y1 = min(AUC.val1_CP))
segments(x0 = 1.8, y0 = max(AUC.val2_CP), x1 = 1.8, y1 = min(AUC.val2_CP))
abline(h = 0.5, lty = 2)

mean(AUC.val1_CP);min(AUC.val1_CP);max(AUC.val1_CP)
# [1] 0.9829478
# [1] 0.9728386
# [1] 0.9926972

mean(AUC.val2_CP);min(AUC.val2_CP);max(AUC.val2_CP)
# [1] 0.8980859
# [1] 0.5812789
# [1] 0.9824561

r2.mean <- apply(r2_CP,1,mean)
mean(r2.mean);min(r2.mean);max(r2.mean)
## between -06 - 5% of  variance explained (average -2%)
# [1] -0.01551323
# [1] -0.06556411
# [1] 0.05114261

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
     main = "Example Training Data Cameron Peak",
     las = 1,
     xlab = "Error Category")

#### Single AUC Plot all three scales ####
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
