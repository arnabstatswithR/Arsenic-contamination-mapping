rm(list = ls())

mydir <- "/home/hazraa/Documents/0_arsenic_small/00_final6/MCMC"
setwd(mydir)

load("clean_data3.Rdata")

load("fit.observed.GP.Rdata")

#---------------------

library(ggplot2)

trace.plotter <- function(chain, parname){
  p <- ggplot() + geom_line(aes(x = 1:14000, y = chain)) + 
    xlab("Iteration") + ylab(parname) + 
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10)) + 
    geom_vline(xintercept = 2000, color = "red", linetype = "dashed") +
    scale_x_continuous(breaks = c(0, 4000, 8000, 12000))
  p}

p1 <- trace.plotter(fit.observed.GP$beta[ , 1], expression(beta[paste(1, ",", 1)]))
p2 <- trace.plotter(fit.observed.GP$Sigma[ , 1, 1], expression(Sigma[paste(1, ",", 1)]))
p3 <- trace.plotter(fit.observed.GP$phi, expression(phi)) #+ ylim(0, 10)
p4 <- trace.plotter(fit.observed.GP$r, "r")
p5 <- trace.plotter(fit.observed.GP$Y.censored[ , 1], expression(Y[1](s[1]^(c)))) + 
  geom_hline(yintercept = log(0.5), color = "blue", linetype = "dashed")
p6 <- trace.plotter(fit.observed.GP$Y.pred[ , 1, 1], expression(Y[1](s[1]^(0))))

library(gridExtra)

p <- grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, ncol = 3)

ggsave(p, filename = "trace_plots.pdf", height = 6, width = 9)

