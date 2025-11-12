#### splitting rf model into multiple - western USA ####
library(terra)
library(dplyr)
library(caret)
library(randomForest)
library(pROC)
library(vegan)

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

trt1 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[1], ]
trt2 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[2], ]
trt3 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[3], ]
trt4 <- Engaged_Lines[Engaged_Lines$trt == levels(Engaged_Lines$trt)[4], ]

set.seed(1)
trt1_sample <- trt1[sample(nrow(trt1), 1000, replace = TRUE), ]
set.seed(1)
trt2_sample <- trt2[sample(nrow(trt2), 1000, replace = TRUE), ]
set.seed(1)
trt3_sample <- trt3[sample(nrow(trt3), 1000, replace = TRUE), ]
set.seed(1)
trt4_sample <- trt4[sample(nrow(trt4), 1000, replace = TRUE), ]
dat_sub <- rbind(trt1_sample,trt2_sample,trt3_sample,trt4_sample)

## pre spatial thinning ordination
r <- rast("LandFire TIFs/WF_dist.tif")
d1 <- dat_sub
d1 <- vect(d1, geom = c("x","y"), crs = crs(r))
d1 <- project(d1, "EPSG:4326")
dmat <- as.matrix(dist(cbind(geom(d1)[,4], geom(d1)[,3]))) ## turning the coordinates of each plot into a distance matrix
dmat <- dmat *111139 ## degrees to meters (approximately)
eigen_res <- RSpectra::eigs_sym(as.matrix(dmat), k = 30)
d1 <- as.data.frame(d1)
d1 <- cbind(d1, eigen_res$vectors)

dat_ord <- as.matrix(d1[,c(8:37)])
PCA.res <- princomp(dat_ord, cor = TRUE)
names(PCA.res)
PCA.res$scores
eigen.res <- eigen(cor(dat_ord))
eigen.res$vectors
d1$stat <- factor(d1$stat, levels = c("EF", "EH"))
d1$trt <- ifelse(d1$trt == "Neither", "Neither",
                      ifelse(d1$trt == "Thinning only", "Thinning", 
                             ifelse(d1$trt == "Prescribed only", "Rx", "Both")))
d1$trt <- factor(d1$trt, levels = c("Neither", "Thinning", "Rx", "Both"))


levels(d1$trt)
group.colors <- c(Neither = "#f6d746", Thinning = "#e55c30", Rx = "#84206b", Both = "#140b34")
library(ggbiplot)
g1 <- ggbiplot(PCA.res, choices = c(1,2),
               obs.scale = 1,
               var.scale = 1,
               varname.size = 4,
               var.axes = TRUE,
               varname.adjust = 1.5,
               group = d1$trt) +
  scale_color_manual(name = "trt", values = group.colors)+
  scale_shape_manual(name="stat", values=c(16,17))+
  stat_ellipse(aes(colour = d1$trt), type = "norm", level = 0.95) +  # Add ellipses
  # stat_ellipse(aes(colour = d1$trt, group = interaction(d1$trt, dat_sub$stat)), 
  #              type = "norm", level = 0.95)+
  geom_point(aes(colour = d1$trt, shape = d1$stat), size = 0.5)+
  guides(color = guide_legend("Treatment"), shape = guide_legend("Line Stat"))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

plot(g1)



## spatial
r <- rast("LandFire TIFs/WF_dist.tif")
blank <- rast(ext(r), resolution=100, vals=NA) ## gonna expand this
crs(blank) <- crs(r)
dat.sp <- vect(dat_sub, geom = c("x","y"))
dat.cell <- extract(blank, dat.sp, cell = TRUE)
dat_sub$cell <- dat.cell$cell
dat_sub <- dat_sub %>% group_by(cell) %>% sample_n(size=1) # sample one point per 100 x 100 m cell
dat_sub <- vect(dat_sub, geom = c("x","y"), crs = crs(blank))
dat_sub <- project(dat_sub, "EPSG:4326")
dmat <- as.matrix(dist(cbind(geom(dat_sub)[,4], geom(dat_sub)[,3]))) ## turning the coordinates of each plot into a distance matrix
dmat <- dmat *111139 ## degrees to meters (approximately)
eigen_res <- RSpectra::eigs_sym(as.matrix(dmat), k = 30)
dat_sub <- as.data.frame(dat_sub)
dat_sub <- cbind(dat_sub, eigen_res$vectors)
dat_sub$cell <- NULL

## post to spatial thinning
dat_ord <- as.matrix(dat_sub[,c(8:37)])
PCA.res <- princomp(dat_ord, cor = TRUE)
names(PCA.res)
PCA.res$scores
eigen.res <- eigen(cor(dat_ord))
eigen.res$vectors
dat_sub$stat <- factor(dat_sub$stat, levels = c("EF", "EH"))
dat_sub$trt <- ifelse(dat_sub$trt == "Neither", "Neither",
                      ifelse(dat_sub$trt == "Thinning only", "Thinning", 
                             ifelse(dat_sub$trt == "Prescribed only", "Rx", "Both")))
dat_sub$trt <- factor(dat_sub$trt, levels = c("Neither", "Thinning", "Rx", "Both"))


levels(dat_sub$trt)
group.colors <- c(Neither = "#f6d746", Thinning = "#e55c30", Rx = "#84206b", Both = "#140b34")
library(ggbiplot)
g1 <- ggbiplot(PCA.res, choices = c(1,2),
               obs.scale = 1,
               var.scale = 1,
               varname.size = 4,
               var.axes = TRUE,
               varname.adjust = 1.5,
               group = dat_sub$trt) +
  scale_color_manual(name = "trt", values = group.colors)+
  scale_shape_manual(name="stat", values=c(16,17))+
  stat_ellipse(aes(colour = dat_sub$trt), type = "norm", level = 0.95) +  # Add ellipses
  # stat_ellipse(aes(colour = dat_sub$trt, group = interaction(dat_sub$trt, dat_sub$stat)), 
  #              type = "norm", level = 0.95)+
  geom_point(aes(colour = dat_sub$trt, shape = dat_sub$stat), size = 0.5)+
  guides(color = guide_legend("Treatment"), shape = guide_legend("Line Stat"))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

plot(g1)



g1 + geom_convexhull(aes(fill=group.colors, 
                      shape = dat_sub$trt, 
                      colour= group.colors),
                  alpha = 0.5)
