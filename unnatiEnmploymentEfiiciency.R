database = read.csv("database.csv")
database$closedvalue = ifelse(database$ClosedEfficiency > 40, -1, 0)
database$interestedvalue = ifelse(database$InterestedEfficiency > 20, 1,0)
database$enrolledvalue = ifelse(database$EnrolledEfficiency > 10,1,0)
database$walkinvalue = ifelse(database$WalkinEfficiency > 15, 1,0)
database$followupvalue = ifelse(database$FollowupEfficiency > 15, 0.5,0)
str(database)
library(ggplot2)
database = database[database$TotalOld > 100,]
database$OverallEfficiency =database$closedvalue + database$interestedvalue + database$enrolledvalue +database$walkinvalue + database$followupvalue
database = transform(database, Counselor = reorder(Counselor, -OverallEfficiency))
ggplot(database, aes(x = Counselor, y = OverallEfficiency, fill = factor(label))) + geom_bar(stat = "identity") + geom_text(aes(label = OverallEfficiency), vjust = -0.4) + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) + scale_fill_manual( breaks = levels(database$label), values = c("red","dark orange","green"))
range(database$OverallEfficiency)
database$OverallEfficiency = database$OverallEfficiency*100/3.5
database$OverallEfficiency = round(database$OverallEfficiency,0)
write.csv(database,"database3.csv")
database$label = ifelse(database$OverallEfficiency > 50, 2,ifelse(database$OverallEfficiency> 20, 1,0))


df = data.frame(database$Counselor,database$OverallEfficiency,database$New)
df2 = data.frame(database$Counselor,database$New)
ndf = rbind(df,df2)
args(rbind)
df$database.New = df$database.New - 100

par(mfrow = c(2,1))
barplot(df$database.OverallEfficiency,df$database.Counselor, xlim = c(0,40), ylim = c(0,100))
barplot(df$database.New, df$database.New)
