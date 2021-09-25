
library(maps)
library(viridis)
library(ggplot2)
library(ggmap)

map.heatmap2 <- function (lat, lon, data, mainTitle = NULL, legendTitle = NULL, zlim = NULL){
  
  # Set limits for x, y, z if not specified as parameters
  if(is.null(xlim)){xlim <- range( lon,na.rm=TRUE)}
  if(is.null(ylim)){ylim <- range( lat,na.rm=TRUE)}
  if(is.null(zlim)){zlim <- range(data,na.rm=TRUE)}
  
  bdbox <- make_bbox(lon = c(min(lon), max(lon)), lat = c(max(lat), min(lat)), f = .1)
  bd <- get_map(location = bdbox, zoom = 7, maptype = "terrain")
  
  dataframe <- data.frame(x = lon, y = lat, data = data)
  
  p <- ggmap(bd)
  p <- p + geom_tile(dataframe, mapping = aes(x = x, y = y, fill = data), height = 0.2, width = 0.2, alpha = 1) + # 
    labs(title=mainTitle, x="Longitude", y="Latitude") + 
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          plot.title = element_text(size=10, hjust = 0.5),
          legend.text = element_text(size = 8), 
          legend.title = element_text(size = 8), 
          legend.key.size = unit(0.2, "in")) + 
    scale_fill_viridis(name = legendTitle, limits = zlim)
  
  baseData <- map_data("world")
  p <- p + geom_path(data=baseData, aes(x=long, y=lat, group=group), colour="black")
  
  return(p)}
