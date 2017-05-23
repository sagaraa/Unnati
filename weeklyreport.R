weekly = read.csv("WeeklyReportCenterwise.csv")
weekly$ODOTotal = weekly$ODODiff = weekly$New = weekly$Missed = weekly$Target = NULL
under = weekly[weekly$SDOTotal == 0,]
weekly = weekly[weekly$SDOTotal != 0,]
weekly$ClosedEfficiency = weekly$Closed/weekly$SDOTotal*100
weekly$FollowupEfficiency = weekly$Followup/weekly$SDOTotal*100
weekly$InterestedEfficiency = weekly$Interested/weekly$SDOTotal*100
weekly$WalkinEfficiency = weekly$Walkin/weekly$SDOTotal*100
weekly$EnrolledEfficiency = weekly$Enrolled/weekly$SDOTotal*100


weekly$closedvalue = ifelse(weekly$ClosedEfficiency > 40, -1, 0)
weekly$interestedvalue = ifelse(weekly$InterestedEfficiency > 20, 1,0)
weekly$enrolledvalue = ifelse(weekly$EnrolledEfficiency > 10,1,0)
weekly$walkinvalue = ifelse(weekly$WalkinEfficiency > 15, 1,0)
weekly$followupvalue = ifelse(weekly$FollowupEfficiency > 15, 0.5,0)
str(weekly)
weekly$OverallEfficiency =weekly$closedvalue + weekly$interestedvalue + weekly$enrolledvalue +weekly$walkinvalue + weekly$followupvalue
range(weekly$OverallEfficiency)
weekly$OverallEfficiency = weekly$OverallEfficiency*100/2.5
weekly$OverallEfficiency = round(weekly$OverallEfficiency,0)
weekly = transform(weekly, Counselor = reorder(Counselor, -OverallEfficiency))
weekly$OverallEfficiency =weekly$closedvalue + weekly$interestedvalue + weekly$enrolledvalue +weekly$walkinvalue + weekly$followupvalue
weekly$label = ifelse(weekly$OverallEfficiency > 50, 2,ifelse(weekly$OverallEfficiency> 20, 1,0))
library(ggplot2)
ggplot(weekly, aes(x = Counselor, y = OverallEfficiency, fill = factor(label))) + geom_bar(stat = "identity") + geom_text(aes(label = OverallEfficiency), vjust = -0.4) + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) + scale_fill_manual( breaks = levels(weekly$label), values = c("red","dark orange","green"))

a = as.character(weekly$Counselor)
b = weekly$New
c = weekly$Followup
d = weekly$Interested
e = weekly$Walkin
f = weekly$Enrolled
g = weekly$Closed
#h = weekly$Total
df = rbind(b,c,d,e,f,g)
#df= as.data.frame(df)
colnames(df) = a
color2 = c("Red","blue")
color = c("red","blue","green","magenta","orange","black")
plot = barplot(df, col = color,srt = 90, las =2,names.arg = a)
legend("topright" , fill =  c("red","blue","green","magenta","orange","black"), legend =  c("New","FollowUp","Interested","Walkin","Enrolled","Closed"),bty = "n")
pdf("NumberwiseWeeklyPerformance.pdf")
dev.copy(png, "NumberwiseWeeklyPerformance.png")
dev.off()
write.csv(weekly,"weeklyreport.csv", row.names = FALSE)
a = as.character(under$Counselor)
write(a, "underperformed.txt")

