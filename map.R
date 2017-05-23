#map    see ggplot2.R also 
install.packages("maps")
install.packages("ggmap")
library(maps)
library(ggmap)

chicago = get_map(location = "chicago", zoom = 11)
ggmap(chicago)
ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))

latlon = as.data.frame(table(round(mvt$Longitude,2),round(mvt$Latitude,2)))
str(latlon)
latlon$lon = as.numeric(as.character(latlon$Var1))
latlon$lat = as.numeric(as.character(latlon$Var2))

ggmap(chicago) + geom_point(data = latlon, aes(x = lon, y = lat, color = Freq)) + scale_color_gradient(name = "Total MV Theft", low = "white", high = "red")


a = ggmap(chicago) + geom_point(data = latlon, aes(x = lon, y = lat, color = Freq, size = Freq)) + scale_color_gradient(name = "Total MV Theft", low = "yellow", high = "red")
pdf("chicago2.pdf")
print(a)
dev.off()
#heatmap
ggmap(chicago) + geom_tile( data = latlon, aes(x = lon, y = lat, alpha = Freq), fill = "red")

latlon2 = latlon[latlon$Freq>0,]
