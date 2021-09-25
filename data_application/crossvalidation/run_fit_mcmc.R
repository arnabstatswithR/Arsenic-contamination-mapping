rm(list = ls())

mydir <- "/home/hazraa/Documents/0_arsenic_small/real_revision/impute"
setwd(mydir)

load("clean_data3.Rdata")

available.cases <- sum(!is.na(Y[ , 1]))
#--------------------

library(parallel)
library(doParallel)

ncores <- 20

cl <- makeCluster(ncores)
registerDoParallel(cl)

coverage.all <- parLapply(cl, 1:available.cases, function(rep.no){
  
  library(fields)
  
  load("clean_data3.Rdata")
  total.cases <- nrow(S)
  censored.cases <- sum(is.na(Y[ , 1]))
  avoid.cases <- which(is.na(Y[ , 1]))
  available.cases <- sum(!is.na(Y[ , 1]))
  keep.cases <- which(!is.na(Y[ , 1]))
  
  source("auxfunctions.R")
  source("update_params.R")
  source("mcmc.R")
  
  test.cases <- keep.cases[rep.no]
  Y.test <- t(as.matrix(Y[test.cases, ]))
  S.test <- t(as.matrix(S[test.cases, ]))
  X.test <- t(as.matrix(X[test.cases, ]))
  
  train.cases <- sort(setdiff(1:nrow(S), test.cases))
  Y.train <- Y.all[train.cases, ]
  S.train <- S[train.cases, ]
  X.train <- X[train.cases, ]
  
  censor.val <- log(0.5)
  censor.cases <- which(is.na(Y.train[ , 1]))
  Y.train[censor.cases, 1] <- censor.val
  
  iters <- 4000
  burn <- 2000
  
  fit.observed.GP <- mcmc.GP(Y.train, S.train, S.test, X.train, X.test, cutoff = censor.val, censor.cases,
                             beta.init = NULL, Sigma.init = NULL, latent.init = NULL,
                             phi.init = 0.1 * max(rdist.earth(S, miles= F)), r.init = 0.5,
                             # priors
                             sd.beta = 100,
                             shape.Sigma = 0.01, rate.Sigma = 0.01,
                             phi.upper = 0.5 * max(rdist.earth(S, miles= F)),
                             # mcmc settings
                             iters = iters, burn = burn, thin = 5)
  
  cis90 <- sapply(1:ncol(Y.test), function(var.no){
    cis <- quantile(fit.observed.GP$Y.pred[-c(1:burn), 1, var.no], probs = c(0.05, 0.95))
    cis})
  
  cis95 <- sapply(1:ncol(Y.test), function(var.no){
    cis <- quantile(fit.observed.GP$Y.pred[-c(1:burn), 1, var.no], probs = c(0.025, 0.975))
    cis})
  
  print(rep.no)
  
  list(cis90 = cis90, cis95 = cis95)})

stopCluster(cl)

save(coverage.all, file = "coverage.all.Rdata")

# set.seed(rep.no)
# test.cases <- sort(sample(rest.cases, ntest))

# distmat.censor.rest <- rdist(S[censor.cases, ], S[rest.cases, ])
# test.cases <- rest.cases[which(colMeans(distmat.censor.rest) <= sort(colMeans(distmat.censor.rest))[ntest])]

# censor.val <- as.vector(quantile(Y.all[ , 1], probs = censor.level))
# censor.cases <- which(Y.all[ , 1] <= censor.val)
# rest.cases <- setdiff(1:nrow(S), censor.cases)
# set.seed(rep.no)
# test.cases <- sort(sample(rest.cases, ntest))
# train.cases <- sort(setdiff(rest.cases, test.cases))
# final.train.cases <- sort(union(censor.cases, train.cases))
# Y.train <- Y.all[final.train.cases, ]
# Y.test <- Y.all[test.cases, ]
# S.train <- S[final.train.cases, ]
# S.test <- S[test.cases, ]
# X.train <- X[final.train.cases, ]
# X.test <- X[test.cases, ]
# censor.cases <- which(Y.train[ , 1] <= censor.val)
