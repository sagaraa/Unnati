referred = read.csv("ReferredValue.csv")
colnames(referred) = c("ReferredFrom","No_OF_0","No_OF_1","ReferredValue")
test = merge(test,referred)
test$No_OF_0 = NULL
test$No_OF_1 = NULL
rm(referred)

centre = read.csv("CenterValue.csv")
colnames(centre) = c("Center","a","b","CenterValue")
centre$a = NULL
centre$b = NULL
test = merge(test,centre)
rm(centre)

SourceType = read.csv("SourceTypeValue.csv")
colnames(SourceType) = c("SourceType", "a","b","SourceTypeValue")
SourceType$a = NULL
SourceType$b = NULL
test = merge(test,SourceType)
rm(SourceType)

priority = read.csv("PriorityValue.csv")
colnames(priority) = c("Priority", "a","b","PriorityValue")
priority$a = NULL
priority$b = NULL
test = merge(test,priority)
rm(priority)

Course = read.csv("CourseValue.csv")
colnames(Course) = c("Course", "a","b","CourseValue")
Course$a = NULL
Course$b = NULL
test = merge(test,Course)
rm(Course)

Source = read.csv("SourceValue.csv")
colnames(Source) = c("Source", "a","b","SourceValue")
Source$a = NULL
Source$b = NULL
test = merge(test,Source)
rm(Source)

Reason = read.csv("ReasonValue.csv")
colnames(Reason) = c("Reason", "a","b","ReasonValue")
Reason$a = NULL
Reason$b = NULL
test = merge(test,Reason)
rm(Reason)

test$StatusValue = predict(unnati_log, newdata = test, type = "response")

target = subset(test, test$StatusValue > 0.5)
nrow(target)
write.csv(target, "target.csv", row.names = FALSE)
