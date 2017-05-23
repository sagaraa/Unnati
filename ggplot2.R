library(ggplot2)
who = read.csv("WHO.csv")
scatter = ggplot(who, aes(x = GNI, y = FertilityRate))
scatter + geom_point()
scatter + geom_line(color = "blue", size = 1, shape = 15)

scatter + geom_point(color = "blue", size = 2, shape = 15) + ggtitle("FertilityRate Vs GNI")

fertilityGNI = scatter + geom_point(color = "blue", size = 2, shape = 4) + ggtitle("FertilityRate Vs GNI")

pdf("fertilityGNI.pdf")
print(fertilityGNI)
dev.off()


ggplot(who, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()
names(who)
ggplot(who, aes(x= log(FertilityRate) , y = Under15)) + geom_point() + stat_smooth(method = "lm", level = 0.99)

ggplot(who, aes(x= log(FertilityRate) , y = Under15)) + geom_point() + stat_smooth(method = "lm", se = FALSE, color = "orange")

a = ggplot(who, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point()
pdf("plot.pdf")
print(a)
dev.off()
