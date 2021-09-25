rm(list = ls())

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/high_censoring/fix_top"
setwd(mydir)

load("crps.all.Rdata")

high.fixtop <- round(apply(t(sapply(crps.all, colMeans)), 2, function(x){c(mean(x), sd(x)/10)}), 3)

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/high_censoring/ignore"
setwd(mydir)

load("crps.all.Rdata")

high.ignore <- round(apply(t(sapply(crps.all, colMeans)), 2, function(x){c(mean(x), sd(x)/10)}), 3)

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/high_censoring/impute"
setwd(mydir)

load("crps.all.Rdata")

high.impute <- round(apply(t(sapply(crps.all, colMeans)), 2, function(x){c(mean(x), sd(x)/10)}), 3)

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/low_censoring/fix_top"
setwd(mydir)

load("crps.all.Rdata")

low.fixtop <- round(apply(t(sapply(crps.all, colMeans)), 2, function(x){c(mean(x), sd(x)/10)}), 3)

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/low_censoring/ignore"
setwd(mydir)

load("crps.all.Rdata")

low.ignore <- round(apply(t(sapply(crps.all, colMeans)), 2, function(x){c(mean(x), sd(x)/10)}), 3)

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/low_censoring/impute"
setwd(mydir)

load("crps.all.Rdata")

low.impute <- round(apply(t(sapply(crps.all, colMeans)), 2, function(x){c(mean(x), sd(x)/10)}), 3)

high.fixtop
high.ignore
high.impute
low.fixtop
low.ignore
low.impute