rm(list = ls())

mydir <- "/home/hazraa/Documents/0_arsenic_small/00_final6/MCMC"
setwd(mydir)

load("clean_data3.Rdata")

load("fit.observed.GP.Rdata")

#---------------------

posmean.predict <- apply(fit.observed.GP$Y.pred[-(1:4000), , ], c(2, 3), mean)
possd.predict <- apply(fit.observed.GP$Y.pred[-(1:4000), , ], c(2, 3), sd)

source("heatmap_generator.R")

p1 <- map.heatmap2(lat = S.pred[ , 2], lon = S.pred[ , 1], data = posmean.predict[ , 1], 
                        mainTitle = "Posterior mean log-As(ug)")
p2 <- map.heatmap2(lat = S.pred[ , 2], lon = S.pred[ , 1], data = possd.predict[ , 1], 
                   mainTitle = "Posterior SD log-As(ug)")

p3 <- map.heatmap2(lat = S.pred[ , 2], lon = S.pred[ , 1], data = posmean.predict[ , 2], 
                   mainTitle = "Posterior mean log-Ba(mg)")
p4 <- map.heatmap2(lat = S.pred[ , 2], lon = S.pred[ , 1], data = possd.predict[ , 2], 
                   mainTitle = "Posterior SD log-Ba(mg)")

p5 <- map.heatmap2(lat = S.pred[ , 2], lon = S.pred[ , 1], data = posmean.predict[ , 3], 
                   mainTitle = "Posterior mean log-Ca(mg)")
p6 <- map.heatmap2(lat = S.pred[ , 2], lon = S.pred[ , 1], data = possd.predict[ , 3], 
                   mainTitle = "Posterior SD log-Ca(mg)")

library(gridExtra)

p <- grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2)

ggsave(p, filename = "spatial_maps_predicted.pdf", height = 9, width = 6)
