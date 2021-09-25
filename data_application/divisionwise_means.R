rm(list = ls())

mydir <- "/home/hazraa/Documents/0_arsenic_small/00_final6/MCMC/BGD_adm"
setwd(mydir)

library(raster)

#Load shapefile
shp <- shapefile("BGD_adm1.shp")

library(maps)

mydir <- "/home/hazraa/Documents/0_arsenic_small/00_final6/MCMC"
setwd(mydir)

load("clean_data3.Rdata")
load("fit.observed.GP.Rdata")

#-------------------------

div.ind <- as.numeric(sapply(map.where(database = shp, x = S.pred[ , 1], y = S.pred[ , 2]), function(xx){as.numeric(strsplit(xx, ":")[[1]][1])}))

ggplot() + geom_tile(aes(x = S.pred[ , 1], y = S.pred[ , 2], fill = div.ind))

div.names <- c("Barisal", "Chittagong", "Dhaka", "Khulna", "Rajshahi", "Rangpur", "Sylhet")

div.means.se <- t(sapply(0:6, function(div.no){
  Y <- round(apply(apply(exp(fit.observed.GP$Y.pred)[-(1:4000), which(div.ind == div.no), ], c(1, 3), mean), 2, 
                   function(x){c(mean(x), sd(x)/sqrt(1e4))}), 4)
  c(paste(Y[1, 1], " (", Y[2, 1], ")", sep = ""), 
    paste(Y[1, 2], " (", Y[2, 2], ")", sep = ""), 
    paste(Y[1, 3], " (", Y[2, 3], ")", sep = ""))}))

rownames(div.means.se) <- div.names
colnames(div.means.se) <- c("As(ug/L)", "Ba(mg/L)", "Ca(mg/L)")

div.means.se

library(xtable)

xtable(div.means.se)

# apply(exp(fit.observed.GP$Y.pred)[ , div.ind == 0, ], c()
# 
# map.where(database = shp, x = S.pred[ , 1], y = S.pred[ , 2])[1]
# 
# apply(exp(fit.observed.GP$Y.pred)[ , which(div.ind == 0), ], c(1, 3), mean)
# 
# ggplot() + geom_tile(aes(x = S.pred[ , 1], y = S.pred[ , 2], fill = Y))
# 
# sum(exp(fit.observed.GP$Y.pred)[ , , ])
# 
# sapply(unique(map.where(database = shp, x = S.pred[ , 1], y = S.pred[ , 2])), function(xx){as.numeric(strsplit(xx, ":")[[1]][1])})
# 
# as.numeric(strsplit(map.where(database = shp, x = S.pred[ , 1], y = S.pred[ , 2])[1], ":")[[1]][1])
# 
# strsplit(, ":")
# 
# strsplit(map.where(database = shp, x = S.pred[ , 1], y = S.pred[ , 2])[1], ":")
# 
# library(sf)
# 
# bd_data0 <- read_sf("BGD_adm0.shp")
# bd_data1<- read_sf("BGD_adm1.shp")
# bd_data2<- read_sf("BGD_adm2.shp")
# 
# View(bd_data0)