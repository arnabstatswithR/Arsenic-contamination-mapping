rm(list = ls())

mydir <- "/home/hazraa/Documents/0_arsenic_small/real_revision/impute"
setwd(mydir)

load("clean_data3.Rdata")

available.cases <- sum(!is.na(Y[ , 1]))
keep.cases <- which(!is.na(Y[ , 1]))

true.vals <- t(sapply(1:available.cases, function(rep.no){Y[keep.cases[rep.no], ]}))

#--------------------------

load("coverage.all.Rdata")

lw1 <- t(sapply(1:available.cases, function(case.no){coverage.all[[case.no]]$cis95[ , 1]}))
lw2 <- t(sapply(1:available.cases, function(case.no){coverage.all[[case.no]]$cis95[ , 2]}))
lw3 <- t(sapply(1:available.cases, function(case.no){coverage.all[[case.no]]$cis95[ , 3]}))

df1 <- data.frame(cases = 1:95, obs.val = true.vals[ , 1], lower = lw1[ , 1], upper = lw1[ , 2])
df2 <- data.frame(cases = 1:95, obs.val = true.vals[ , 2], lower = lw2[ , 1], upper = lw2[ , 2])
df3 <- data.frame(cases = 1:95, obs.val = true.vals[ , 3], lower = lw3[ , 1], upper = lw3[ , 2])

library(ggplot2)

p1 <- ggplot(df1, aes(x = cases, y = lower)) + geom_errorbar(aes(ymin = lower, ymax = upper), size = 1.5, 
                            width = 0.25, position = position_dodge(), alpha = 0.5) + 
  geom_point(aes(x = cases, y = obs.val), color = "red", size = 2) + 
  ggtitle("log-As") + xlab(NULL) + ylab("") +  #xlab("Prediction locations") + ylab("Prediction intervals") + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, hjust = 0.5)) + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

p2 <- ggplot(df2, aes(x = cases, y = lower)) + geom_errorbar(aes(ymin = lower, ymax = upper), size = 1.5, 
                                                             width = 0.25, position = position_dodge(), alpha = 0.5) + 
  geom_point(aes(x = cases, y = obs.val), color = "red", size = 2) + 
  ylab("Prediction intervals") + ggtitle("log-Ba") + xlab(NULL) + # xlab("Prediction locations") + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 25),
        plot.title = element_text(size = 25, hjust = 0.5)) + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

p3 <- ggplot(df3, aes(x = cases, y = lower)) + geom_errorbar(aes(ymin = lower, ymax = upper), size = 1.5, 
                                                             width = 0.25, position = position_dodge(), alpha = 0.5) + 
  geom_point(aes(x = cases, y = obs.val), color = "red", size = 2) + 
  xlab("Prediction locations") + ggtitle("log-Ca") + ylab("") + #ylab("Prediction intervals") + 
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 25),
        plot.title = element_text(size = 25, hjust = 0.5)) + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

library(gridExtra)

p <- grid.arrange(p1, p2, p3, nrow = 3, ncol = 1)

ggsave(p, filename = "crossvalidation.pdf", height = 12, width = 20)
