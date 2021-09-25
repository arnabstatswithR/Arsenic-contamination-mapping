rm(list = ls())

mydir <- "/home/hazraa/Documents/0_arsenic_small/00_final6/MCMC"
setwd(mydir)

load("clean_data3.Rdata")

#--------------------
# initiation

source("auxfunctions.R")
source("update_params.R")
source("mcmc.R")
#--------------------

library(fields)

fit.observed.GP <- mcmc.GP(Y, S, S.pred, X, X.pred, cutoff = log(0.5), censored.locs,
                           beta.init = NULL, Sigma.init = NULL, latent.init = NULL,
                           phi.init = 0.1 * max(rdist.earth(S, miles= F)), r.init = 0.5,
                           # priors
                           sd.beta = 100,
                           shape.Sigma = 0.01, rate.Sigma = 0.01,
                           phi.upper = 0.5 * max(rdist.earth(S, miles= F)),
                           # mcmc settings
                           iters = 14000, burn = 4000, thin = 5)

save(fit.observed.GP, file = "fit.observed.GP.Rdata")

# plot(fit.observed.GP$tau.mu, type = "l")
# plot(fit.observed.GP$tau, type = "l")
# plot(fit.observed.GP$r, type = "l")
# plot(fit.observed.GP$rho, type = "l")

#-------------
