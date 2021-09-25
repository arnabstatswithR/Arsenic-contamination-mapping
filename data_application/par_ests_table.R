rm(list = ls())

mydir <- "/home/hazraa/Documents/0_arsenic_small/00_final6/MCMC"
setwd(mydir)

load("clean_data3.Rdata")

load("fit.observed.GP.Rdata")

#---------------------

beta.ests <- cbind(apply(fit.observed.GP$beta[-(1:4000), ], 2, mean),
                   apply(fit.observed.GP$beta[-(1:4000), ], 2, sd),
                   apply(fit.observed.GP$beta[-(1:4000), ], 2, quantile, probs = 0.025),
                   apply(fit.observed.GP$beta[-(1:4000), ], 2, quantile, probs = 0.975))

Sigma.ests <- cbind(c(apply(fit.observed.GP$Sigma[-(1:4000), , ], c(2, 3), mean))[c(1, 5, 9, 2, 3, 6)],
                    c(apply(fit.observed.GP$Sigma[-(1:4000), , ], c(2, 3), sd))[c(1, 5, 9, 2, 3, 6)],
                    c(apply(fit.observed.GP$Sigma[-(1:4000), , ], c(2, 3), quantile, probs = 0.025))[c(1, 5, 9, 2, 3, 6)],
                    c(apply(fit.observed.GP$Sigma[-(1:4000), , ], c(2, 3), quantile, probs = 0.975))[c(1, 5, 9, 2, 3, 6)])

phi.ests <- c(mean(fit.observed.GP$phi[-(1:4000)]), sd(fit.observed.GP$phi[-(1:4000)]), 
              quantile(fit.observed.GP$phi[-(1:4000)], probs = c(0.025, 0.975)))
r.ests <- c(mean(fit.observed.GP$r[-(1:4000)]), sd(fit.observed.GP$r[-(1:4000)]), 
            quantile(fit.observed.GP$r[-(1:4000)], probs = c(0.025, 0.975)))

final <- rbind(beta.ests, Sigma.ests, phi.ests, r.ests)

library(xtable)

xtable(round(final, 2))


