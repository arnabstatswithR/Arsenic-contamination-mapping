rm(list = ls())

mydir <- "/home/hazraa/Documents/0_arsenic_small/00_final6/MCMC"
setwd(mydir)

load("clean_data3.Rdata")

load("fit.observed.GP.Rdata")

#---------------------

library(ggplot2)

histogram.plotter.censored <- function(X){
  p <- ggplot() + geom_histogram(aes(x= X, y=..density..), bins = 30) + #xlim(min(X), log(0.5)) + 
    geom_vline(xintercept = log(0.5), color = "blue", linetype = "dashed") +
  ylab("Density") +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10))
  p}

# 3, 9, 14

p1 <- histogram.plotter.censored(fit.observed.GP$Y.censored[-(1:4000), 4]) + xlab(expression(Y[1](s[4]^(c))))
p2 <- histogram.plotter.censored(fit.observed.GP$Y.censored[-(1:4000), 9]) + xlab(expression(Y[1](s[9]^(c))))
p3 <- histogram.plotter.censored(fit.observed.GP$Y.censored[-(1:4000), 18]) + xlab(expression(Y[1](s[18]^(c))))

# library(ks)
# kde.boundary(fit.observed.GP$Y.censored[ , 3], xmin = -10, xmax = log(0.5))

histogram.plotter.predicted <- function(X){
  p <- ggplot() + geom_histogram(aes(x= X, y=..density..), bins = 30) + 
    ylab("Density") +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10))
  p}

# p4 <- histogram.plotter.predicted(fit.observed.GP$Y.pred[ , 50, 1]) + xlab(expression(Y[1](s[50]^(0))))
# p5 <- histogram.plotter.predicted(fit.observed.GP$Y.pred[ , 150, 1]) + xlab(expression(Y[1](s[150]^(0))))
# p6 <- histogram.plotter.predicted(fit.observed.GP$Y.pred[ , 250, 1]) + xlab(expression(Y[1](s[250]^(0))))

p4 <- histogram.plotter.predicted(fit.observed.GP$Y.pred[-(1:4000), 287, 1]) + xlab(expression(Y[1](s[287]^(0))))
p5 <- histogram.plotter.predicted(fit.observed.GP$Y.pred[-(1:4000), 287, 2]) + xlab(expression(Y[2](s[287]^(0))))
p6 <- histogram.plotter.predicted(fit.observed.GP$Y.pred[-(1:4000), 287, 3]) + xlab(expression(Y[3](s[287]^(0))))

library(gridExtra)

p <- grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)

ggsave(p, filename = "histograms_censored_predicted.pdf", height = 6, width = 9)
