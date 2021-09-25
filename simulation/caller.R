rm(list = ls())

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/high_censoring/fix_top"
setwd(mydir)

source("run_fit_mcmc.R")

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/high_censoring/ignore"
setwd(mydir)

source("run_fit_mcmc.R")

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/high_censoring/impute"
setwd(mydir)

source("run_fit_mcmc.R")

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/low_censoring/fix_top"
setwd(mydir)

source("run_fit_mcmc.R")

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/low_censoring/ignore"
setwd(mydir)

source("run_fit_mcmc.R")

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/low_censoring/impute"
setwd(mydir)

source("run_fit_mcmc.R")
