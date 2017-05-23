list.files()
center = read.csv("CenterValue.csv")
library(ggplot2)

table(unnati$Status[unnati$Status == "New",], unnati$Center)
a = as.data.frame(table(test$Center))
write.csv(a,"New_Center_wise.csv")

closed = subset(unnati, unnati$Status == "Closed")
table(closed$Reason)

colnames(c) = c("ReferringPerson", "NumberOfInvalidNo")
ggplot(c, aes(x = ReferringPerson, y = NumberOfInvalidNo)) + geom_bar(stat = "identity") + geom_text(aes(label = NumberOfInvalidNo), vjust = -0.4)

write.csv(c, "Invaild_ReferringFrom.csv")
test = unnati[unnati$Status == "New",]
write.csv(test, "New.csv")
x = as.data.frame(table(test$Center))
colnames(x ) = c("Center", "NumberOfStudent")
write.csv(x,"New.csv")

referred = read.csv("ReferredValue.csv")
referred = referred[order(referred$ReferredValue, decreasing = TRUE),]
tail(referred,10)
referred$ReferredValue = referred$ReferredValue *100
referred$ReferredValue = round(referred$ReferredValue, digits = 2)
referred = transform(referred, X = reorder(X,-ReferredValue))
referred = referred[referred$ReferredValue >3,]
referredPlot = ggplot(referred, aes(x = X, y = ReferredValue)) + geom_bar(stat = "identity") + geom_text(aes(label = ReferredValue), vjust = -0.4) +theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 80, hjust = 1))
pdf("Referred.pdf")
print(referredPlot)
dev.off()

center = read.csv("CenterValue.csv")
center = center[is.na(center$CenterValue) == FALSE,]
center$CenterValue = center$CenterValue*100
center$CenterValue = round(center$CenterValue, digits = 2)
center = transform(center, X = reorder(X, -CenterValue))
center = center[center$CenterValue > 5,]
centerplot =ggplot(center, aes(x = X, y = CenterValue)) + geom_bar(stat = "identity", fill = "dark blue") + geom_text(aes(label = CenterValue), vjust = -0.4) + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
pdf("center.pdf")
print(centerplot)
dev.off()

priority = read.csv("PriorityValue.csv")
pie(priority$PriorityValue, label = c("High","Medium","Low"), col = c("dark blue","blue","light blue"))
priority$PriorityValue = priority$PriorityValue*100
priority$PriorityValue = round(priority$PriorityValue,2)
priority = transform(priority, X = reorder(X, -PriorityValue))
priorityplt = ggplot(priority, aes(x = X, y = PriorityValue)) + geom_bar(stat = "identity", fill = "dark blue") + geom_text(aes(label = PriorityValue), vjust = -0.4) + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
pdf("priority.pdf")
print(priorityplot)
dev.off()

reasons = read.csv("ReasonValue.csv")
reasons$ReasonValue = reasons$ReasonValue*100
reasons$ReasonValue = round(reasons$ReasonValue,2)
reasons = transform(reasons, X = reorder(X, -ReasonValue))
reasons = reasons[reasons$ReasonValue != 0,]
reasonplot = ggplot(reasons, aes(x = X, y = ReasonValue)) + geom_bar(stat = "identity", fill = "dark blue") + geom_text(aes(label = ReasonValue), vjust = -0.4) + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
pdf("reason.pdf")
print(reasonplot)
dev.off()

source = read.csv("SourceValue.csv")
source = source[is.na(source$SourceValue) == FALSE,]
source$SourceValue = source$SourceValue * 100
source$SourceValue = round(source$SourceValue,2)
source = transform(source,X=reorder(X,-SourceValue))
source = source[source$SourceValue != 0,]
sourceplot = ggplot(source, aes(x = X, y = SourceValue)) + geom_bar(stat = "identity", fill = "dark blue") + geom_text(aes(label = SourceValue), vjust = -0.4) + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 65, hjust = 1))
pdf("sourceplot.pdf")
print(sourceplot)
dev.off()

sourceType = read.csv("SourceTypeValue.csv")
sourceType = sourceType[is.na(sourceType$SourceTypeValue) == FALSE,]
sourceType$SourceTypeValue = sourceType$SourceTypeValue * 100
sourceType$SourceTypeValue = round(sourceType$SourceTypeValue,2)
sourceType = transform(sourceType,X=reorder(X,-SourceTypeValue))
sourceType = sourceType[sourceType$SourceTypeValue != 0,]
sourceTypeplot = ggplot(sourceType, aes(x = X, y = SourceTypeValue)) + geom_bar(stat = "identity", fill = "dark blue") + geom_text(aes(label = SourceTypeValue), vjust = -0.4) + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
pdf("sourceTypeplot.pdf")
print(sourceTypeplot)
dev.off()
