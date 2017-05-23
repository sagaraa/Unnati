murders = read.csv("murders.csv")
str(murders)

statemap = map_data("state")
str(statemap)

ggplot(statemap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

murders$region = tolower(murders$State)
murderMap = merge(murders, statemap)

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color = "black") + scale_fill_gradient(low = "white", high = "red", guide = "legend")

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color = "black") + scale_fill_gradient(low = "white", high = "red", guide = "legend")
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = (Murders/Population*100000))) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend", limits = c(0,10))


# intl

intl = read.csv("intl.csv")
str(intl)

ggplot(intl, aes(x = Region, y = PercentOfIntl)) + geom_bar(stat = "identity") + geom_text(aes(label = PercentOfIntl), vjust = -0.4)
intl = intl[order(intl$PercentOfIntl, decreasing = TRUE),]  

intlall = read.csv("Intlall.csv", stringsAsFactors = FALSE)
str(intlall)
intlall[is.na(intlall)] = 0
str(intlall)
worldmap = map_data("world")
str(worldmap)
worldmap = merge(worldmap, intlall, by.x = "region", by.y = "Citizenship")
ggplot(worldmap, aes(x = long, y = lat)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")

worldmap = morldmap[order(worldmap$group, worldmap$order)]
