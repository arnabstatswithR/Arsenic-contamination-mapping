rm(list = ls())

mydir <- "/home/hazraa/Documents/0_arsenic_small/real_revision/impute"
setwd(mydir)

load("clean_data3.Rdata")

available.cases <- sum(!is.na(Y[ , 1]))
keep.cases <- which(!is.na(Y[ , 1]))

true.vals <- t(sapply(1:available.cases, function(rep.no){Y[keep.cases[rep.no], ]}))
S.keep <- S[keep.cases, ]

#--------------------------

load("coverage.all.Rdata")

lw1 <- t(sapply(1:available.cases, function(case.no){coverage.all[[case.no]]$cis90[ , 1]}))
lw2 <- t(sapply(1:available.cases, function(case.no){coverage.all[[case.no]]$cis90[ , 2]}))
lw3 <- t(sapply(1:available.cases, function(case.no){coverage.all[[case.no]]$cis90[ , 3]}))

pred1 <- rowMeans(lw1)
pred2 <- rowMeans(lw2)
pred3 <- rowMeans(lw3)

#-------------------------

data.bd <- data.frame(Longitude = S.keep[ , 1], Latitude = S.keep[ , 2], 
                      As = true.vals[ , 1] - pred1, Ba = true.vals[ , 2] - pred2, Ca = true.vals[ , 3] - pred3)

library(ggmap)

bdbox <- make_bbox(lon = c(min(S[ , 1]), max(S[ , 1])+ 0.5), lat = c(max(S[ , 2]), min(S[ , 2])-0.7), f = .1)

bd <- get_map(location = bdbox, zoom = 7, maptype = "terrain")

p0 <- ggmap(bd)

#---------------------------

library(viridis)

baseData <- map_data("world")

p1 <- p0 + geom_point(data.bd, mapping = aes(x = Longitude, y = Latitude, color = As), size = 5) + #, alpha = 0.7
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("log-As residuals") + labs(size = "log-As") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size=20),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20)) + 
  scale_color_gradient(low="gray", high="black") + #scale_color_viridis(option = "magma") +
  theme(legend.key.size = unit(0.2, "in"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10), 
        legend.position = c(0.5,0.07), 
        legend.direction = "horizontal",
        legend.key.width = unit(0.25, "in")) + 
  geom_path(data=baseData, aes(x=long, y=lat, group=group), colour="black")

p2 <- p0 + geom_point(data.bd, mapping = aes(x = Longitude, y = Latitude, color = Ba), size = 5) + 
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("log-Ba residuals") + labs(size = "log-Ba") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size=20),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20)) + 
  scale_color_gradient(low="gray", high="black") + #scale_color_viridis(option = "magma") +
  theme(legend.key.size = unit(0.2, "in"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10), 
        legend.position = c(0.5,0.07), 
        legend.direction = "horizontal",
        legend.key.width = unit(0.25, "in")) + 
  geom_path(data=baseData, aes(x=long, y=lat, group=group), colour="black")

# , legend.position = c(0.5,0.07), legend.direction = "horizontal"

p3 <- p0 + geom_point(data.bd, mapping = aes(x = Longitude, y = Latitude, color = Ca), size = 5) + 
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("log-Ca residuals") + labs(size = "log-Ca") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size=20),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20)) + 
  scale_color_gradient(low="gray", high="black") + #scale_color_viridis(option = "magma") +
  theme(legend.key.size = unit(0.2, "in"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10), 
        legend.position = c(0.5,0.07), 
        legend.direction = "horizontal",
        legend.key.width = unit(0.25, "in")) + 
  geom_path(data=baseData, aes(x=long, y=lat, group=group), colour="black")

# , legend.position = c(0.5,0.07), legend.direction = "horizontal"

library(gridExtra)

p <- grid.arrange(p1, p2, p3, nrow = 1, ncol = 3)

ggsave(p, filename = "spatial_maps_residuals.pdf", height = 6, width = 13)

