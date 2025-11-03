#### Preliminary code to make sure all projections are correct
library(terra)
WesternStates <- vect("D:/Outside Boundary/Census State Boundaries/WesternStates.shp")

#### Landfire rasters ####
setwd("D:/Outside Boundary/LandFire TIFs")
temp <- list.files(pattern="*.tif") ## creating a vector that has all the files in the working directory with .xlsx extensions

for(i in 1:length(temp)) assign(temp[i], terra::rast(temp[i])) ## loading in the shapefiles I want

Sys.time()
LF23_Dist.tif <- terra::project(LF23_Dist.tif, LF24_Dist.tif);gc()
LF22_Dist.tif <- terra::project(LF22_Dist.tif, LF24_Dist.tif);gc()
LF21_Dist.tif <- terra::project(LF21_Dist.tif, LF24_Dist.tif);gc()
LF20_Dist.tif <- terra::project(LF20_Dist.tif, LF24_Dist.tif);gc()
LF19_Dist.tif <- terra::project(LF19_Dist.tif, LF24_Dist.tif);gc()
LF18_Dist.tif <- terra::project(LF18_Dist.tif, LF24_Dist.tif);gc()
LF17_Dist.tif <- terra::project(LF17_Dist.tif, LF24_Dist.tif);gc()
LF16_Dist.tif <- terra::project(LF16_Dist.tif, LF24_Dist.tif);gc()
LF15_Dist.tif <- terra::project(LF15_Dist.tif, LF24_Dist.tif);gc()
LF14_Dist.tif <- terra::project(LF14_Dist.tif, LF24_Dist.tif);gc()
LF13_Dist.tif <- terra::project(LF13_Dist.tif, LF24_Dist.tif);gc()
LF12_Dist.tif <- terra::project(LF12_Dist.tif, LF24_Dist.tif);gc()
LF11_Dist.tif <- terra::project(LF11_Dist.tif, LF24_Dist.tif);gc()
LF10_Dist.tif <- terra::project(LF10_Dist.tif, LF24_Dist.tif);gc()
LF09_Dist.tif <- terra::project(LF09_Dist.tif, LF24_Dist.tif);gc()
LF08_Dist.tif <- terra::project(LF08_Dist.tif, LF24_Dist.tif);gc()
LF07_Dist.tif <- terra::project(LF07_Dist.tif, LF24_Dist.tif);gc()
LF06_Dist.tif <- terra::project(LF06_Dist.tif, LF24_Dist.tif);gc()
LF05_Dist.tif <- terra::project(LF05_Dist.tif, LF24_Dist.tif);gc()
LF04_Dist.tif <- terra::project(LF04_Dist.tif, LF24_Dist.tif);gc()
LF03_Dist.tif <- terra::project(LF03_Dist.tif, LF24_Dist.tif);gc()
LF02_Dist.tif <- terra::project(LF02_Dist.tif, LF24_Dist.tif);gc()
LF01_Dist.tif <- terra::project(LF01_Dist.tif, LF24_Dist.tif);gc()
LF00_Dist.tif <- terra::project(LF00_Dist.tif, LF24_Dist.tif);gc()
LF99_Dist.tif <- terra::project(LF99_Dist.tif, LF24_Dist.tif);gc()

ext(LF23_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF22_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF21_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF20_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF19_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF18_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF17_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF16_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF15_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF14_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF13_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF12_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF11_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF10_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF09_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF08_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF07_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF06_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF05_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF04_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF03_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF02_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF01_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF00_Dist.tif) <- ext(LF24_Dist.tif);gc()
ext(LF99_Dist.tif) <- ext(LF24_Dist.tif);gc()

writeRaster(LF23_Dist.tif, "LF23_Dist.tif", overwrite = TRUE)
writeRaster(LF22_Dist.tif, "LF22_Dist.tif", overwrite = TRUE)
writeRaster(LF21_Dist.tif, "LF21_Dist.tif", overwrite = TRUE)
writeRaster(LF20_Dist.tif, "LF20_Dist.tif", overwrite = TRUE)
writeRaster(LF19_Dist.tif, "LF19_Dist.tif", overwrite = TRUE)
writeRaster(LF18_Dist.tif, "LF18_Dist.tif", overwrite = TRUE)
writeRaster(LF17_Dist.tif, "LF17_Dist.tif", overwrite = TRUE)
writeRaster(LF16_Dist.tif, "LF16_Dist.tif", overwrite = TRUE)
writeRaster(LF15_Dist.tif, "LF15_Dist.tif", overwrite = TRUE)
writeRaster(LF14_Dist.tif, "LF14_Dist.tif", overwrite = TRUE)
writeRaster(LF13_Dist.tif, "LF13_Dist.tif", overwrite = TRUE)
writeRaster(LF12_Dist.tif, "LF12_Dist.tif", overwrite = TRUE)
writeRaster(LF11_Dist.tif, "LF11_Dist.tif", overwrite = TRUE)
writeRaster(LF10_Dist.tif, "LF10_Dist.tif", overwrite = TRUE)
writeRaster(LF09_Dist.tif, "LF09_Dist.tif", overwrite = TRUE)
writeRaster(LF08_Dist.tif, "LF08_Dist.tif", overwrite = TRUE)
writeRaster(LF07_Dist.tif, "LF07_Dist.tif", overwrite = TRUE)
writeRaster(LF06_Dist.tif, "LF06_Dist.tif", overwrite = TRUE)
writeRaster(LF05_Dist.tif, "LF05_Dist.tif", overwrite = TRUE)
writeRaster(LF04_Dist.tif, "LF04_Dist.tif", overwrite = TRUE)
writeRaster(LF03_Dist.tif, "LF03_Dist.tif", overwrite = TRUE)
writeRaster(LF02_Dist.tif, "LF02_Dist.tif", overwrite = TRUE)
writeRaster(LF01_Dist.tif, "LF01_Dist.tif", overwrite = TRUE)
writeRaster(LF00_Dist.tif, "LF00_Dist.tif", overwrite = TRUE)
writeRaster(LF99_Dist.tif, "LF99_Dist.tif", overwrite = TRUE)

## stacking the rasters
dist_stack <- c(LF99_Dist.tif,
                LF00_Dist.tif,LF01_Dist.tif,LF02_Dist.tif,LF03_Dist.tif,LF04_Dist.tif,LF05_Dist.tif,LF06_Dist.tif,LF07_Dist.tif,LF08_Dist.tif,LF09_Dist.tif,
                LF10_Dist.tif,LF11_Dist.tif,LF12_Dist.tif,LF13_Dist.tif,LF14_Dist.tif,LF15_Dist.tif,LF16_Dist.tif,LF17_Dist.tif,LF18_Dist.tif,LF19_Dist.tif,
                LF20_Dist.tif,LF21_Dist.tif,LF22_Dist.tif,LF23_Dist.tif,LF24_Dist.tif)
rm(LF99_Dist.tif)
rm(LF00_Dist.tif);rm(LF01_Dist.tif);rm(LF02_Dist.tif);rm(LF03_Dist.tif);rm(LF04_Dist.tif);rm(LF05_Dist.tif);rm(LF06_Dist.tif);rm(LF07_Dist.tif);rm(LF08_Dist.tif);rm(LF09_Dist.tif)
rm(LF10_Dist.tif);rm(LF11_Dist.tif);rm(LF12_Dist.tif);rm(LF13_Dist.tif);rm(LF14_Dist.tif);rm(LF15_Dist.tif);rm(LF16_Dist.tif);rm(LF17_Dist.tif);rm(LF18_Dist.tif);rm(LF19_Dist.tif)
rm(LF20_Dist.tif);rm(LF21_Dist.tif);rm(LF22_Dist.tif);rm(LF23_Dist.tif);rm(LF24_Dist.tif)
Sys.time()


#### Checking the Land Fire csv files ####
setwd("D:/Outside Boundary/LandFire csvs")
temp <- list.files(pattern="*.csv") ## creating a vector that has all the files in the working directory with .xlsx extensions

for(i in 1:length(temp)) assign(temp[i], read.csv(temp[i])) ## loading in the shapefiles I want

LF24_Dist.csv$VALUE %in% LF23_Dist.csv$VALUE
LF23_Dist.csv$VALUE %in% LF23_Dist.csv$VALUE
## These CSV files are mostly slightly different
## I will make a combined CSV that keeps code, year, and disturbance type

D_csv <- data.frame(VALUE = c(LF99_Dist.csv$Value, 
                              LF00_Dist.csv$Value,LF01_Dist.csv$Value,LF02_Dist.csv$Value,LF03_Dist.csv$Value,LF04_Dist.csv$Value,LF05_Dist.csv$Value,LF06_Dist.csv$Value,LF07_Dist.csv$Value,LF08_Dist.csv$Value,LF09_Dist.csv$Value,
                              LF10_Dist.csv$Value,LF11_Dist.csv$VALUE,LF12_Dist.csv$VALUE,LF13_Dist.csv$VALUE,LF14_Dist.csv$VALUE,LF15_Dist.csv$VALUE,LF16_Dist.csv$VALUE,LF17_Dist.csv$VALUE,LF18_Dist.csv$VALUE,LF19_Dist.csv$VALUE,
                              LF20_Dist.csv$VALUE,LF21_Dist.csv$VALUE,LF22_Dist.csv$VALUE,LF23_Dist.csv$VALUE,LF24_Dist.csv$VALUE),
                    DIST_YEAR = c(LF99_Dist.csv$Year, 
                                  LF00_Dist.csv$Year,LF01_Dist.csv$Year,LF02_Dist.csv$Year,LF03_Dist.csv$Year,LF04_Dist.csv$Year,LF05_Dist.csv$Year,LF06_Dist.csv$Year,LF07_Dist.csv$Year,LF08_Dist.csv$Dist_Year,LF09_Dist.csv$Dist_Year,
                                  LF10_Dist.csv$Dist_Year,LF11_Dist.csv$DIST_YEAR,LF12_Dist.csv$DIST_YEAR,LF13_Dist.csv$DIST_YEAR,LF14_Dist.csv$DIST_YEAR,LF15_Dist.csv$DIST_YEAR,LF16_Dist.csv$DIST_YEAR,LF17_Dist.csv$DIST_YEAR,LF18_Dist.csv$DIST_YEAR,LF19_Dist.csv$DIST_YEAR,
                                  LF20_Dist.csv$DIST_YEAR,LF21_Dist.csv$DIST_YEAR,LF22_Dist.csv$DIST_YEAR,LF23_Dist.csv$DIST_YEAR,LF24_Dist.csv$FISCAL_YR),
                    DIST_TYPE = c(LF99_Dist.csv$Dist_Type, 
                                  LF00_Dist.csv$Dist_Type,LF01_Dist.csv$Dist_Type,LF02_Dist.csv$Dist_Type,LF03_Dist.csv$Dist_Type,LF04_Dist.csv$Dist_Type,LF05_Dist.csv$Dist_Type,LF06_Dist.csv$Dist_Type,LF07_Dist.csv$Dist_Type,LF08_Dist.csv$Dist_Type,LF09_Dist.csv$Dist_Type,
                                  LF10_Dist.csv$Dist_Type,LF11_Dist.csv$DIST_TYPE,LF12_Dist.csv$DIST_TYPE,LF13_Dist.csv$DIST_TYPE,LF14_Dist.csv$DIST_TYPE,LF15_Dist.csv$DIST_TYPE,LF16_Dist.csv$DIST_TYPE,LF17_Dist.csv$DIST_TYPE,LF18_Dist.csv$DIST_TYPE,LF19_Dist.csv$DIST_TYPE,
                                  LF20_Dist.csv$DIST_TYPE,LF21_Dist.csv$DIST_TYPE,LF22_Dist.csv$DIST_TYPE,LF23_Dist.csv$DIST_TYPE,LF24_Dist.csv$DIST_TYPE))

table(D_csv$VALUE) ## only a few values are present throughout all 25 observations
table(D_csv$DIST_YEAR)
table(D_csv$DIST_TYPE)

D_csv <- read.csv("D:/Outside Boundary/LandFire csvs/LF_total_dist.csv")

write.csv(D_csv,"LF_total_dist.csv")


#### Fire Lines  ####
setwd("D:/Outside Boundary/NIFC Lines")
temp <- list.files(pattern="*.shp") ## creating a vector that has all the files in the working directory with .xlsx extensions

for(i in 1:length(temp)) assign(temp[i], terra::vect(temp[i])) ## loading in the shapefiles I want

EventLine2024.shp <- terra::project(EventLine2024.shp, WesternStates)
EventLine2023.shp <- terra::project(EventLine2023.shp, WesternStates)
EventLine2022.shp <- terra::project(EventLine2022.shp, WesternStates)
EventLine2021.shp <- terra::project(EventLine2021.shp, WesternStates)
EventLine2020.shp <- terra::project(EventLine2020.shp, WesternStates) 
EventLine2019.shp <- terra::project(EventLine2019.shp, WesternStates) 
EventLine2018.shp <- terra::project(EventLine2018.shp, WesternStates) 

writeVector(EventLine2024.shp, "EventLine2024.shp", overwrite = TRUE)
writeVector(EventLine2023.shp, "EventLine2023.shp", overwrite = TRUE)
writeVector(EventLine2022.shp, "EventLine2022.shp", overwrite = TRUE)
writeVector(EventLine2021.shp, "EventLine2021.shp", overwrite = TRUE)
writeVector(EventLine2020.shp, "EventLine2020.shp", overwrite = TRUE)
writeVector(EventLine2019.shp, "EventLine2019.shp", overwrite = TRUE)
writeVector(EventLine2018.shp, "EventLine2018.shp", overwrite = TRUE)

rm(EventLine2018.shp);rm(EventLine2019.shp);rm(EventLine2020.shp);rm(EventLine2022.shp);rm(EventLine2023.shp);rm(EventLine2024.shp)
gc()


#### Fire Polygons ####
setwd("D:/Outside Boundary/NIFC Polygons")
temp <- list.files(pattern="*.shp") ## creating a vector that has all the files in the working directory with .xlsx extensions

for(i in 1:length(temp)) assign(temp[i], terra::vect(temp[i])) ## loading in the shapefiles I want

EventPolygon2024.shp <- terra::project(EventPolygon2024.shp, WesternStates)
EventPolygon2023.shp <- terra::project(EventPolygon2023.shp, WesternStates)
EventPolygon2022.shp <- terra::project(EventPolygon2022.shp, WesternStates)
EventPolygon2021.shp <- terra::project(EventPolygon2021.shp, WesternStates)
EventPolygon2020.shp <- terra::project(EventPolygon2020.shp, WesternStates) 
EventPolygon2019.shp <- terra::project(EventPolygon2019.shp, WesternStates) 
EventPolygon2018.shp <- terra::project(EventPolygon2018.shp, WesternStates) 

writeVector(EventPolygon2024.shp, "EventPolygon2024.shp", overwrite = TRUE)
writeVector(EventPolygon2023.shp, "EventPolygon2023.shp", overwrite = TRUE)
writeVector(EventPolygon2022.shp, "EventPolygon2022.shp", overwrite = TRUE)
writeVector(EventPolygon2021.shp, "EventPolygon2021.shp", overwrite = TRUE)
writeVector(EventPolygon2020.shp, "EventPolygon2020.shp", overwrite = TRUE)
writeVector(EventPolygon2019.shp, "EventPolygon2019.shp", overwrite = TRUE)
writeVector(EventPolygon2018.shp, "EventPolygon2018.shp", overwrite = TRUE)

rm(EventPolygon2018.shp);rm(EventPolygon2019.shp);rm(EventPolygon2020.shp);rm(EventPolygon2022.shp);rm(EventPolygon2023.shp);rm(EventPolygon2024.shp)
gc()


#### Western Forest Layers ####
# WesternForests <- rast("D:/Outside Boundary/Annual_NLCD_LndCov_2018/WesternUSA_Forest_2018.tif")
# WesternForests <- terra::project(WesternForests, WesternStates) 
# setwd("D:/Outside Boundary/Annual_NLCD_LndCov_2018")
# writeRaster(WesternForests, "WesternForests.tif")

WesternForests <- vect("D:/Outside Boundary/US EPA Ecoregions/WesternForestEcoregions.shp")
crs(WesternForests)
WesternForests <- terra::project(WesternForests, WesternStates)
crs(WesternForests)
crs(WesternStates)
setwd("D:/Outside Boundary/US EPA Ecoregions")
writeVector(WesternForests, "WesternForestEcoregions.shp", overwrite = TRUE)
gc()
