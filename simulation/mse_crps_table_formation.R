rm(list = ls())

#--------------------

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/high_censoring/fix_top"
setwd(mydir)

load("crps.all.Rdata")

beta.rmse.mean.se.high.fixtop <- round(t(apply(sapply(1:100, function(rep.no){c(t(crps.all[[rep.no]]$beta.rmse))}), 1, function(x){c(mean(x), sd(x)/10)})), 3)
Sigma.rmse.mean.se.high.fixtop <- round(t(apply(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$Sigma.rmse)}), 1, function(x){c(mean(x), sd(x)/10)})), 3)
phi.rmse.mean.se.high.fixtop <- round(c(mean(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$phi.rmse)})), 
                      sd(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$phi.rmse)})) / 10), 3)
r.rmse.mean.se.high.fixtop <- round(c(mean(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$r.rmse)})), 
                    sd(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$r.rmse)})) / 10), 3)
crps.mean.se.high.fixtop <- round(apply(t(sapply(1:100, function(rep.no){colMeans(crps.all[[rep.no]]$crps.vals)})), 2, function(x){c(mean(x), sd(x)/10)}), 3)

beta.rmse.mean.se.high.fixtop
Sigma.rmse.mean.se.high.fixtop
phi.rmse.mean.se.high.fixtop
r.rmse.mean.se.high.fixtop
crps.mean.se.high.fixtop
#--------------------

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/high_censoring/ignore"
setwd(mydir)

load("crps.all.Rdata")

beta.rmse.mean.se.high.ignore <- round(t(apply(sapply(1:100, function(rep.no){c(t(crps.all[[rep.no]]$beta.rmse))}), 1, function(x){c(mean(x), sd(x)/10)})), 3)
Sigma.rmse.mean.se.high.ignore <- round(t(apply(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$Sigma.rmse)}), 1, function(x){c(mean(x), sd(x)/10)})), 3)
phi.rmse.mean.se.high.ignore <- round(c(mean(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$phi.rmse)})), 
                      sd(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$phi.rmse)})) / 10), 3)
r.rmse.mean.se.high.ignore <- round(c(mean(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$r.rmse)})), 
                    sd(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$r.rmse)})) / 10), 3)
crps.mean.se.high.ignore <- round(apply(t(sapply(1:100, function(rep.no){colMeans(crps.all[[rep.no]]$crps.vals)})), 2, function(x){c(mean(x), sd(x)/10)}), 3)

beta.rmse.mean.se.high.ignore
Sigma.rmse.mean.se.high.ignore
phi.rmse.mean.se.high.ignore
r.rmse.mean.se.high.ignore
crps.mean.se.high.ignore
#--------------------

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/high_censoring/impute"
setwd(mydir)

load("crps.all.Rdata")

beta.rmse.mean.se.high.impute <- round(t(apply(sapply(1:100, function(rep.no){c(t(crps.all[[rep.no]]$beta.rmse))}), 1, function(x){c(mean(x), sd(x)/10)})), 3)
Sigma.rmse.mean.se.high.impute <- round(t(apply(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$Sigma.rmse)}), 1, function(x){c(mean(x), sd(x)/10)})), 3)
phi.rmse.mean.se.high.impute <- round(c(mean(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$phi.rmse)})), 
                      sd(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$phi.rmse)})) / 10), 3)
r.rmse.mean.se.high.impute <- round(c(mean(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$r.rmse)})), 
                    sd(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$r.rmse)})) / 10), 3)
crps.mean.se.high.impute <- round(apply(t(sapply(1:100, function(rep.no){colMeans(crps.all[[rep.no]]$crps.vals)})), 2, function(x){c(mean(x), sd(x)/10)}), 3)

beta.rmse.mean.se.high.impute
Sigma.rmse.mean.se.high.impute
phi.rmse.mean.se.high.impute
r.rmse.mean.se.high.impute
crps.mean.se.high.impute
#--------------------

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/low_censoring/fix_top"
setwd(mydir)

load("crps.all.Rdata")

beta.rmse.mean.se.low.fixtop <- round(t(apply(sapply(1:100, function(rep.no){c(t(crps.all[[rep.no]]$beta.rmse))}), 1, function(x){c(mean(x), sd(x)/10)})), 3)
Sigma.rmse.mean.se.low.fixtop <- round(t(apply(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$Sigma.rmse)}), 1, function(x){c(mean(x), sd(x)/10)})), 3)
phi.rmse.mean.se.low.fixtop <- round(c(mean(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$phi.rmse)})), 
                      sd(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$phi.rmse)})) / 10), 3)
r.rmse.mean.se.low.fixtop <- round(c(mean(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$r.rmse)})), 
                    sd(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$r.rmse)})) / 10), 3)
crps.mean.se.low.fixtop <- round(apply(t(sapply(1:100, function(rep.no){colMeans(crps.all[[rep.no]]$crps.vals)})), 2, function(x){c(mean(x), sd(x)/10)}), 3)

beta.rmse.mean.se.low.fixtop
Sigma.rmse.mean.se.low.fixtop
phi.rmse.mean.se.low.fixtop
r.rmse.mean.se.low.fixtop
crps.mean.se.low.fixtop
#--------------------

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/low_censoring/ignore"
setwd(mydir)

load("crps.all.Rdata")

beta.rmse.mean.se.low.ignore <- round(t(apply(sapply(1:100, function(rep.no){c(t(crps.all[[rep.no]]$beta.rmse))}), 1, function(x){c(mean(x), sd(x)/10)})), 3)
Sigma.rmse.mean.se.low.ignore <- round(t(apply(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$Sigma.rmse)}), 1, function(x){c(mean(x), sd(x)/10)})), 3)
phi.rmse.mean.se.low.ignore <- round(c(mean(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$phi.rmse)})), 
                      sd(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$phi.rmse)})) / 10), 3)
r.rmse.mean.se.low.ignore <- round(c(mean(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$r.rmse)})), 
                    sd(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$r.rmse)})) / 10), 3)
crps.mean.se.low.ignore <- round(apply(t(sapply(1:100, function(rep.no){colMeans(crps.all[[rep.no]]$crps.vals)})), 2, function(x){c(mean(x), sd(x)/10)}), 3)

beta.rmse.mean.se.low.ignore
Sigma.rmse.mean.se.low.ignore
phi.rmse.mean.se.low.ignore
r.rmse.mean.se.low.ignore
crps.mean.se.low.ignore
#--------------------

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/low_censoring/impute"
setwd(mydir)

load("crps.all.Rdata")

beta.rmse.mean.se.low.impute <- round(t(apply(sapply(1:100, function(rep.no){c(t(crps.all[[rep.no]]$beta.rmse))}), 1, function(x){c(mean(x), sd(x)/10)})), 3)
Sigma.rmse.mean.se.low.impute <- round(t(apply(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$Sigma.rmse)}), 1, function(x){c(mean(x), sd(x)/10)})), 3)
phi.rmse.mean.se.low.impute <- round(c(mean(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$phi.rmse)})), 
                      sd(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$phi.rmse)})) / 10), 3)
r.rmse.mean.se.low.impute <- round(c(mean(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$r.rmse)})), 
                    sd(sapply(1:100, function(rep.no){c(crps.all[[rep.no]]$r.rmse)})) / 10), 3)
crps.mean.se.low.impute <- round(apply(t(sapply(1:100, function(rep.no){colMeans(crps.all[[rep.no]]$crps.vals)})), 2, function(x){c(mean(x), sd(x)/10)}), 3)

beta.rmse.mean.se.low.impute
Sigma.rmse.mean.se.low.impute
phi.rmse.mean.se.low.impute
r.rmse.mean.se.low.impute
crps.mean.se.low.impute
#--------------------

beta.low.table <- t(sapply(1:6, function(i){
  c(paste(beta.rmse.mean.se.low.fixtop[i, 1], " (", beta.rmse.mean.se.low.fixtop[i, 2], ")", sep = ""),
    paste(beta.rmse.mean.se.low.ignore[i, 1], " (", beta.rmse.mean.se.low.ignore[i, 2], ")", sep = ""),
    paste(beta.rmse.mean.se.low.impute[i, 1], " (", beta.rmse.mean.se.low.impute[i, 2], ")", sep = ""))}))

Sigma.low.table <- t(sapply(c(1, 4, 2), function(i){
  c(paste(Sigma.rmse.mean.se.low.fixtop[i, 1], " (", Sigma.rmse.mean.se.low.fixtop[i, 2], ")", sep = ""),
    paste(Sigma.rmse.mean.se.low.ignore[i, 1], " (", Sigma.rmse.mean.se.low.ignore[i, 2], ")", sep = ""),
    paste(Sigma.rmse.mean.se.low.impute[i, 1], " (", Sigma.rmse.mean.se.low.impute[i, 2], ")", sep = ""))}))

phi.low.table <- c(paste(phi.rmse.mean.se.low.fixtop[1], " (", phi.rmse.mean.se.low.fixtop[2], ")", sep = ""),
  paste(phi.rmse.mean.se.low.ignore[1], " (", phi.rmse.mean.se.low.ignore[2], ")", sep = ""),
  paste(phi.rmse.mean.se.low.impute[1], " (", phi.rmse.mean.se.low.impute[2], ")", sep = ""))

r.low.table <- c(paste(r.rmse.mean.se.low.fixtop[1], " (", r.rmse.mean.se.low.fixtop[2], ")", sep = ""),
                   paste(r.rmse.mean.se.low.ignore[1], " (", r.rmse.mean.se.low.ignore[2], ")", sep = ""),
                   paste(r.rmse.mean.se.low.impute[1], " (", r.rmse.mean.se.low.impute[2], ")", sep = ""))

full.low.table <- rbind(beta.low.table, Sigma.low.table, phi.low.table, r.low.table)
full.low.table
#--------------------

beta.high.table <- t(sapply(1:6, function(i){
  c(paste(beta.rmse.mean.se.high.fixtop[i, 1], " (", beta.rmse.mean.se.high.fixtop[i, 2], ")", sep = ""),
    paste(beta.rmse.mean.se.high.ignore[i, 1], " (", beta.rmse.mean.se.high.ignore[i, 2], ")", sep = ""),
    paste(beta.rmse.mean.se.high.impute[i, 1], " (", beta.rmse.mean.se.high.impute[i, 2], ")", sep = ""))}))

Sigma.high.table <- t(sapply(c(1, 4, 2), function(i){
  c(paste(Sigma.rmse.mean.se.high.fixtop[i, 1], " (", Sigma.rmse.mean.se.high.fixtop[i, 2], ")", sep = ""),
    paste(Sigma.rmse.mean.se.high.ignore[i, 1], " (", Sigma.rmse.mean.se.high.ignore[i, 2], ")", sep = ""),
    paste(Sigma.rmse.mean.se.high.impute[i, 1], " (", Sigma.rmse.mean.se.high.impute[i, 2], ")", sep = ""))}))

phi.high.table <- c(paste(phi.rmse.mean.se.high.fixtop[1], " (", phi.rmse.mean.se.high.fixtop[2], ")", sep = ""),
                   paste(phi.rmse.mean.se.high.ignore[1], " (", phi.rmse.mean.se.high.ignore[2], ")", sep = ""),
                   paste(phi.rmse.mean.se.high.impute[1], " (", phi.rmse.mean.se.high.impute[2], ")", sep = ""))

r.high.table <- c(paste(r.rmse.mean.se.high.fixtop[1], " (", r.rmse.mean.se.high.fixtop[2], ")", sep = ""),
                 paste(r.rmse.mean.se.high.ignore[1], " (", r.rmse.mean.se.high.ignore[2], ")", sep = ""),
                 paste(r.rmse.mean.se.high.impute[1], " (", r.rmse.mean.se.high.impute[2], ")", sep = ""))

full.high.table <- rbind(beta.high.table, Sigma.high.table, phi.high.table, r.high.table)
full.high.table

crps.mean.se.high.fixtop
crps.mean.se.high.ignore
crps.mean.se.high.impute
crps.mean.se.low.fixtop
crps.mean.se.low.ignore
crps.mean.se.low.impute

library(xtable)

xtable(full.low.table)
xtable(full.high.table)

low.pred <- rbind(c(paste(crps.mean.se.low.fixtop[1, 1],  " (", crps.mean.se.low.fixtop[2, 1], ")", sep = ""),
                    paste(crps.mean.se.low.ignore[1, 1],  " (", crps.mean.se.low.ignore[2, 1], ")", sep = ""),
                    paste(crps.mean.se.low.impute[1, 1],  " (", crps.mean.se.low.impute[2, 1], ")", sep = "")),
                  c(paste(crps.mean.se.low.fixtop[1, 2],  " (", crps.mean.se.low.fixtop[2, 2], ")", sep = ""),
                    paste(crps.mean.se.low.ignore[1, 2],  " (", crps.mean.se.low.ignore[2, 2], ")", sep = ""),
                    paste(crps.mean.se.low.impute[1, 2],  " (", crps.mean.se.low.impute[2, 2], ")", sep = "")))

high.pred <- rbind(c(paste(crps.mean.se.high.fixtop[1, 1],  " (", crps.mean.se.high.fixtop[2, 1], ")", sep = ""),
                     paste(crps.mean.se.high.ignore[1, 1],  " (", crps.mean.se.high.ignore[2, 1], ")", sep = ""),
                     paste(crps.mean.se.high.impute[1, 1],  " (", crps.mean.se.high.impute[2, 1], ")", sep = "")),
                   c(paste(crps.mean.se.high.fixtop[1, 2],  " (", crps.mean.se.high.fixtop[2, 2], ")", sep = ""),
                     paste(crps.mean.se.high.ignore[1, 2],  " (", crps.mean.se.high.ignore[2, 2], ")", sep = ""),
                     paste(crps.mean.se.high.impute[1, 2],  " (", crps.mean.se.high.impute[2, 2], ")", sep = "")))

xtable(low.pred)
xtable(high.pred)
