rm(list = ls())

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/low_censoring/impute"
setwd(mydir)

load("simulated_data.Rdata")

#--------------------

library(parallel)
library(doParallel)

ncores <- 20

cl <- makeCluster(ncores)
registerDoParallel(cl)

crps.coverage.all <- parLapply(cl, 1:100, function(rep.no){
  
  library(fields)
  
  load("simulated_data.Rdata")
  
  source("auxfunctions.R")
  source("update_params.R")
  source("mcmc.R")
  
  crps_calculator <- function(pos.samples, test.val){
    pos.samples <- sort(pos.samples)
    mm <- length(pos.samples)
    inds <- 1:mm
    out <- 2 / mm^2 * sum((pos.samples - test.val) * (mm * as.numeric(test.val < pos.samples) - inds + 0.5))
    out}
  
  Y.all <- Y.list[[rep.no]]
  
  censor.level <- 0.15
  ntest <- 50
  
  set.seed(rep.no)
  test.cases <- sort(sample(1:nrow(S), ntest))
  Y.test <- Y.all[test.cases, ]
  S.test <- S[test.cases, ]
  X.test <- X[test.cases, ]
  
  train.cases <- sort(setdiff(1:nrow(S), test.cases))
  Y.train <- Y.all[train.cases, ]
  S.train <- S[train.cases, ]
  X.train <- X[train.cases, ]
  
  censor.val <- as.vector(quantile(Y.train[ , 1], probs = censor.level))
  censor.cases <- which(Y.train[ , 1] <= censor.val)
  
  iters <- 4000
  burn <- 2000
  
  fit.observed.GP <- mcmc.GP(Y.train, S.train, S.test, X.train, X.test, cutoff = censor.val, censor.cases,
                             beta.init = NULL, Sigma.init = NULL, latent.init = NULL,
                             phi.init = 0.1 * max(rdist(S)), r.init = 0.5,
                             # priors
                             sd.beta = 100,
                             shape.Sigma = 0.01, rate.Sigma = 0.01,
                             phi.upper = 0.25 * max(rdist(S)),
                             # mcmc settings
                             iters = iters, burn = burn, thin = 5)
  
  beta.rmse <- t(matrix(sqrt(rowMeans((t(fit.observed.GP$beta[-c(1:burn), ]) - c(t(B.mat)))^2)), ncol(Y.test), ncol(X.test)))
  Sigma.rmse <- sqrt(matrix(rowMeans((apply(fit.observed.GP$Sigma[-c(1:burn), , ], 1, c) - c(Sigma))^2), ncol(Y.test), ncol(Y.test)))
  phi.rmse <- sqrt(mean((fit.observed.GP$phi[-c(1:burn)] - phi)^2))
  r.rmse <- sqrt(mean((fit.observed.GP$r[-c(1:burn)] - r)^2))
  
  crps.vals <- sapply(1:ncol(Y.test), function(var.no){sapply(1:length(Y.test[ , var.no]), function(loc.no){
    crps_calculator(fit.observed.GP$Y.pred[-c(1:burn), loc.no, var.no], Y.test[loc.no, var.no])})})
  
  coverage90 <- colMeans(sapply(1:ncol(Y.test), function(var.no){sapply(1:length(Y.test[ , var.no]), function(loc.no){
    cis <- quantile(fit.observed.GP$Y.pred[-c(1:burn), loc.no, var.no], probs = c(0.05, 0.95))
    as.numeric((cis[1] < Y.test[loc.no, var.no]) * (Y.test[loc.no, var.no] < cis[2]))})}))
  
  coverage95 <- colMeans(sapply(1:ncol(Y.test), function(var.no){sapply(1:length(Y.test[ , var.no]), function(loc.no){
    cis <- quantile(fit.observed.GP$Y.pred[-c(1:burn), loc.no, var.no], probs = c(0.025, 0.975))
    as.numeric((cis[1] < Y.test[loc.no, var.no]) * (Y.test[loc.no, var.no] < cis[2]))})}))
  
  print(rep.no)
  
  list(beta.rmse = beta.rmse, Sigma.rmse = Sigma.rmse, phi.rmse = phi.rmse, r.rmse = r.rmse, 
       crps.vals = crps.vals, coverage90 = coverage90, coverage95 = coverage95)})

stopCluster(cl)

save(crps.coverage.all, file = "crps.coverage.all.Rdata")
