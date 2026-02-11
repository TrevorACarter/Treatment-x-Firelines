#### Begin - Load Dependencies and Set WD ####
library(terra)
setwd("D:/Outside Boundary")

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
# Loop through each object# Lotempop through each object
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
## fires in the Puget Lowlands (not too strange)