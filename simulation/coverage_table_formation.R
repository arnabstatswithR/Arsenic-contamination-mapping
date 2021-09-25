rm(list = ls())

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation_revision/high_censoring/fix_top"
setwd(mydir)

load("coverage.all.Rdata")

high.fixtop90 <- round(apply(sapply(1:100, function(rep.no){coverage.all[[rep.no]]$coverage90}), 1, function(x){c(mean(x), sd(x)/10)}), 3)
high.fixtop95 <- round(apply(sapply(1:100, function(rep.no){coverage.all[[rep.no]]$coverage95}), 1, function(x){c(mean(x), sd(x)/10)}), 3)

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation_revision/high_censoring/ignore"
setwd(mydir)

load("coverage.all.Rdata")

high.ignore90 <- round(apply(sapply(1:100, function(rep.no){coverage.all[[rep.no]]$coverage90}), 1, function(x){c(mean(x), sd(x)/10)}), 3)
high.ignore95 <- round(apply(sapply(1:100, function(rep.no){coverage.all[[rep.no]]$coverage95}), 1, function(x){c(mean(x), sd(x)/10)}), 3)

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation_revision/high_censoring/impute"
setwd(mydir)

load("coverage.all.Rdata")

high.impute90 <- round(apply(sapply(1:100, function(rep.no){coverage.all[[rep.no]]$coverage90}), 1, function(x){c(mean(x), sd(x)/10)}), 3)
high.impute95 <- round(apply(sapply(1:100, function(rep.no){coverage.all[[rep.no]]$coverage95}), 1, function(x){c(mean(x), sd(x)/10)}), 3)

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation_revision/low_censoring/fix_top"
setwd(mydir)

load("coverage.all.Rdata")

low.fixtop90 <- round(apply(sapply(1:100, function(rep.no){coverage.all[[rep.no]]$coverage90}), 1, function(x){c(mean(x), sd(x)/10)}), 3)
low.fixtop95 <- round(apply(sapply(1:100, function(rep.no){coverage.all[[rep.no]]$coverage95}), 1, function(x){c(mean(x), sd(x)/10)}), 3)

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation_revision/low_censoring/ignore"
setwd(mydir)

load("coverage.all.Rdata")

low.ignore90 <- round(apply(sapply(1:100, function(rep.no){coverage.all[[rep.no]]$coverage90}), 1, function(x){c(mean(x), sd(x)/10)}), 3)
low.ignore95 <- round(apply(sapply(1:100, function(rep.no){coverage.all[[rep.no]]$coverage95}), 1, function(x){c(mean(x), sd(x)/10)}), 3)

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation_revision/low_censoring/impute"
setwd(mydir)

load("coverage.all.Rdata")

low.impute90 <- round(apply(sapply(1:100, function(rep.no){coverage.all[[rep.no]]$coverage90}), 1, function(x){c(mean(x), sd(x)/10)}), 3)
low.impute95 <- round(apply(sapply(1:100, function(rep.no){coverage.all[[rep.no]]$coverage95}), 1, function(x){c(mean(x), sd(x)/10)}), 3)

high.fixtop90
high.ignore90
high.impute90
low.fixtop90
low.ignore90
low.impute90

high.fixtop95
high.ignore95
high.impute95
low.fixtop95
low.ignore95
low.impute95

coverage90.h <- t(sapply(1:2, function(var.no){
  c(paste(high.fixtop90[1, var.no], " (", high.fixtop90[2, var.no], ")", sep = ""),
    paste(high.ignore90[1, var.no], " (", high.ignore90[2, var.no], ")", sep = ""),
    paste(high.impute90[1, var.no], " (", high.impute90[2, var.no], ")", sep = ""))}))

coverage90.l <- t(sapply(1:2, function(var.no){
  c(paste(low.fixtop90[1, var.no], " (", low.fixtop90[2, var.no], ")", sep = ""),
    paste(low.ignore90[1, var.no], " (", low.ignore90[2, var.no], ")", sep = ""),
    paste(low.impute90[1, var.no], " (", low.impute90[2, var.no], ")", sep = ""))}))

coverage95.h <- t(sapply(1:2, function(var.no){
  c(paste(high.fixtop95[1, var.no], " (", high.fixtop95[2, var.no], ")", sep = ""),
    paste(high.ignore95[1, var.no], " (", high.ignore95[2, var.no], ")", sep = ""),
    paste(high.impute95[1, var.no], " (", high.impute95[2, var.no], ")", sep = ""))}))

coverage95.l <- t(sapply(1:2, function(var.no){
  c(paste(low.fixtop95[1, var.no], " (", low.fixtop95[2, var.no], ")", sep = ""),
    paste(low.ignore95[1, var.no], " (", low.ignore95[2, var.no], ")", sep = ""),
    paste(low.impute95[1, var.no], " (", low.impute95[2, var.no], ")", sep = ""))}))

rbind(cbind(coverage90.l, coverage95.l), cbind(coverage90.h, coverage95.h))

library(xtable)

xtable(rbind(cbind(coverage90.l, coverage95.l), cbind(coverage90.h, coverage95.h)))

