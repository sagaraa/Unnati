library(ggplot2)
database = read.csv("database.csv")
names(database)
a = as.character(database$Counselor)
b = database$New
c = database$Followup
d = database$Interested
e = database$Walkin
f = database$Enrolled
g = database$Closed
#h = database$Total
df = rbind(b,c,d,e,f,g)
df= as.data.frame(df)
colnames(df) = a
color2 = c("Red","blue")
color = c("red","blue","green","magenta","orange","black")
barplot(df, col = color,srt = 90, las =2,names.arg = a)
legend("topright" , fill =  c("red","blue","green","magenta","orange","black"), legend =  c("New","FollowUp","Interested","Walkin","Enrolled","Closed"),bty = "n")
write.csv(df,"df.csv", row.names = TRUE)
df = read.csv("df.csv")
ggplot(df, aes(x=, y= )) + geom_bar(stat = "identity") + geom_text(aes(label = database$Total), vjust = -0.4) + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) + scale_fill_manual(values = color)


df3 = read.csv("database3.csv")
a = df3$Counselor
b = df3$New
c = df3$OverallEfficiency
df3 = rbind(b,c)
df3 = as.data.frame(df3)
colnames(df3) = a
barplot(df3, las = 2, col = color2)
legend("topright",  fill = c("red","blue")  ,legend = c("New Pending","OverallEfficiency"))
str(df3)
View(df3)
