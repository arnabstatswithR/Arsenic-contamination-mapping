rm(list = ls())

mydir <- "/home/hazraa/Documents/0_arsenic_small/simulation/simulated_data"
setwd(mydir)

S <- as.matrix(expand.grid(x = seq(0, 15, by = 1), y = seq(0, 15, by = 1)))

X <- cbind(rep(1, nrow(S)), as.matrix(apply(S, 2, scale)))

B.mat <- matrix(c(4, 0, 0, 6, 0, 0), 3, 2)
Sigma <- matrix(c(2, 1, 1, 2), 2, 2)
phi <- 2.5
r <- 0.8

library(fields)

distmat <- rdist(S)
cormat <- exp(-distmat / phi)

ns <- nrow(S)
np <- 2
nrep <- 100

Y.list <- lapply(1:nrep, function(rep.no){
  set.seed(rep.no)
  latent <- sqrt(r) * (t(chol(cormat)) %*% matrix(rnorm(ns * np), ns, np)) %*% chol(Sigma)
  Y <- X %*% B.mat + latent + sqrt(1 - r) * matrix(rnorm(ns * np), ns, np) %*% chol(Sigma)
Y})

save(Y.list, S, X, B.mat, Sigma, phi, r, file = "simulated_data.Rdata")

save(Y.list, S, X, B.mat, Sigma, phi, r, 
     file = "/home/hazraa/Documents/0_arsenic_small/simulation/high_censoring/fix_top/simulated_data.Rdata")

save(Y.list, S, X, B.mat, Sigma, phi, r, 
     file = "/home/hazraa/Documents/0_arsenic_small/simulation/high_censoring/impute/simulated_data.Rdata")

save(Y.list, S, X, B.mat, Sigma, phi, r, 
     file = "/home/hazraa/Documents/0_arsenic_small/simulation/high_censoring/ignore/simulated_data.Rdata")

save(Y.list, S, X, B.mat, Sigma, phi, r, 
     file = "/home/hazraa/Documents/0_arsenic_small/simulation/low_censoring/fix_top/simulated_data.Rdata")

save(Y.list, S, X, B.mat, Sigma, phi, r, 
     file = "/home/hazraa/Documents/0_arsenic_small/simulation/low_censoring/impute/simulated_data.Rdata")

save(Y.list, S, X, B.mat, Sigma, phi, r, 
     file = "/home/hazraa/Documents/0_arsenic_small/simulation/low_censoring/ignore/simulated_data.Rdata")

# low.censor <- as.vector(quantile(Y[ , 1], probs = 0.15))
# high.censor <- as.vector(quantile(Y[ , 1], probs = 0.45))

# censor.locs.low <- which(Y[ , 1] <= low.censor)
# censor.locs.high <- which(Y[ , 1] <= high.censor)
