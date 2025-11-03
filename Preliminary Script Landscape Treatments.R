prelim <- read.csv("C:/Users/trevo/Dropbox/My PC (LAPTOP-GI7LHD15)/Documents/Carter/Research/Papers/In Progress/Landscape Treatment Effects/data/Mech_PB_WesternStates.csv")
str(prelim)
bp <- table(prelim$state)
barplot(bp)
table(prelim$twig_categ)
table(prelim$type)

TBData <- read.csv("C:/Users/trevo/Dropbox/My PC (LAPTOP-GI7LHD15)/Documents/Carter/Research/Papers/In Progress/Landscape Treatment Effects/data/Completed_Thin_Burn_WesternStates.csv")
str(TBData)
bp <- table(TBData$state)
barplot(bp,
        ylim = c(0,max(bp)+max(bp*0.1)),
        las = 1)
text(x = c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     y = (bp)+bp*0.05,
     labels = bp)
c(0.8,1.9,3.1,4.4,5.6,6.8,7.9,9.1,10.3,11.5,12.9)

table(TBData$type)
table(TBData$twig_categ)

library(terra)
shp <- vect("C:/Users/trevo/Dropbox/My PC (LAPTOP-GI7LHD15)/Documents/Carter/Research/Papers/In Progress/Landscape Treatment Effects/data/Completed_Thin_Burn_WesternStates.shp") ## reading in the shapefile to clip area of interest
gc()

plot(shp[shp$type == "Jackpot Burn",])
shp2 <- terra::intersect(shp[shp$twig_categ == "Mechanical",],
                         shp[shp$twig_categ == "Planned Ignition",])
shp3 <- terra::crop(shp[shp$twig_categ == "Planned Ignition",],
                    shp[shp$twig_categ == "Mechanical",])
shp4 <- terra::relate(shp[shp$twig_categ == "Planned Ignition",],
                      shp[shp$twig_categ == "Mechanical",],
                      relation = "intersects")

table(shp2$twig_categ)
plot(shp2)
# plot(shp)
shp2.dat <- as.data.frame(shp2)
shp3.dat <- as.data.frame(shp3)
shp4.dat <- as.data.frame(shp4)


bp <- table(shp3.dat$state)
bp
barplot(bp,
        ylim = c(0,max(bp)+max(bp*0.1)),
        las = 1)
text(x = c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     y = (bp)+bp*0.05,
     labels = bp)
c(0.8,1.9,3.1,4.4,5.6,6.8,7.9,9.1,10.3,11.5,12.9)
