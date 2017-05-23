unnati = read.csv("all_unnati.csv")
test = unnati[unnati$Status == "New",]
train = unnati[unnati$Status != "New",]
train$StatusValue[train$Status == "Enrolled"] = 1
train$StatusValue[train$Status == "Interested - Call Back"] = 1
train$StatusValue[train$Status == "Closed"] = 0
train$StatusValue[train$Status == "FollowUp - Call Back"] = 0
train$StatusValue[train$Status == "Not Known"] = 0
train$StatusValue[train$Status == "Personal Visit-Walkin"] = 1
table(train$StatusValue)
table(train$Status)


library(caTools)
set.seed(143)
split = sample.split(train$StatusValue, SplitRatio = 0.7)
training = subset(train, split == TRUE)
testing = subset(train, split == FALSE)

referred = as.data.frame(unclass(table(training$ReferredFrom, training$Status == c("Enrolled", "Interested - Call Back"))))
colnames(referred) = c("Not_Enrolled", "Enrolled")
referred$Not_Enrolled = as.numeric(as.character(referred$Not_Enrolled))
referred$Enrolled = as.numeric(referred$Enrolled)
referred$ReferredValue = referred$Enrolled/(referred$Not_Enrolled + referred$Enrolled)
referred = referred[order(referred$ReferredValue, decreasing = TRUE), ]
write.csv(referred, file = "ReferredValue.csv")
rm(referred)
referred = read.csv("ReferredValue.csv")
colnames(referred) = c("ReferredFrom","No_OF_0","No_OF_1","ReferredValue")
referred$No_OF_0 = NULL
referred$No_OF_1 = NULL
training = merge(training,referred)

center = data.frame(unclass(table(training$Center, training$Status == c("Enrolled", "Interested - Call Back"))))
colnames(center) = c("Not_Enrolled", "Enrolled_Interested")
center$Not_Enrolled = as.numeric(center$Not_Enrolled)
center$Enrolled_Interested = as.numeric(center$Enrolled_Interested)
center$CenterValue = center$Enrolled_Interested/(center$Not_Enrolled + center$Enrolled_Interested)
center = center[order(center$CenterValue, decreasing = TRUE), ]
write.csv(center,"CenterValue.csv")
rm(center)
centre = read.csv("CenterValue.csv")
colnames(centre) = c("Center","a","b","CenterValue")
centre$a = NULL
centre$b = NULL
training = merge(training,centre)

SourceValue = data.frame(unclass(table(training$Source , training$Status == c("Enrolled", "Interested - Call Back"))))
colnames(SourceValue) = c("Not_Enrolled", "Enrolled_Interested")
SourceValue$Not_Enrolled = as.numeric(SourceValue$Not_Enrolled)
SourceValue$Enrolled_Interested = as.numeric(SourceValue$Enrolled_Interested)
SourceValue$SourceValue = SourceValue$Enrolled_Interested/(SourceValue$Not_Enrolled + SourceValue$Enrolled_Interested)
SourceValue = SourceValue[order(SourceValue$SourceValue, decreasing = TRUE), ]
write.csv(SourceValue,"SourceValue.csv")
rm(SourceValue)
source = read.csv("SourceValue.csv")
colnames(source) = c("Source","a","b","SourceValue")
source$a = NULL
source$b = NULL
training = merge(training,source)

PriorityValue = data.frame(unclass(table(training$Priority , training$Status == c("Enrolled", "Interested - Call Back"))))
colnames(PriorityValue) = c("Not_Enrolled", "Enrolled_Interested")
PriorityValue$Not_Enrolled = as.numeric(PriorityValue$Not_Enrolled)
PriorityValue$Enrolled_Interested = as.numeric(PriorityValue$Enrolled_Interested)
PriorityValue$PriorityValue = PriorityValue$Enrolled_Interested/(PriorityValue$Not_Enrolled + PriorityValue$Enrolled_Interested)
PriorityValue = PriorityValue[order(PriorityValue$PriorityValue, decreasing = TRUE), ]
write.csv(PriorityValue,"PriorityValue.csv")
rm(PriorityValue)
priority = read.csv("PriorityValue.csv")
colnames(priority) = c("Priority", "a","b","PriorityValue")
priority$a = NULL
priority$b = NULL
training = merge(training,priority)

CourseValue = data.frame(unclass(table(training$Course , training$Status == c("Enrolled", "Interested - Call Back"))))
colnames(CourseValue) = c("Not_Enrolled", "Enrolled_Interested")
CourseValue$Not_Enrolled = as.numeric(CourseValue$Not_Enrolled)
CourseValue$Enrolled_Interested = as.numeric(CourseValue$Enrolled_Interested)
CourseValue$CourseValue = CourseValue$Enrolled_Interested/(CourseValue$Not_Enrolled + CourseValue$Enrolled_Interested)
CourseValue = CourseValue[order(CourseValue$CourseValue, decreasing = TRUE), ]
write.csv(CourseValue,"CourseValue.csv")
rm(CourseValue)
Course = read.csv("CourseValue.csv")
colnames(Course) = c("Course", "a","b","CourseValue")
Course$a = NULL
Course$b = NULL
training = merge(training,Course)

SourceTypeValue = data.frame(unclass(table(training$SourceType , training$Status == c("Enrolled", "Interested - Call Back"))))
colnames(SourceTypeValue) = c("Not_Enrolled", "Enrolled_Interested")
SourceTypeValue$Not_Enrolled = as.numeric(SourceTypeValue$Not_Enrolled)
SourceTypeValue$Enrolled_Interested = as.numeric(SourceTypeValue$Enrolled_Interested)
SourceTypeValue$SourceTypeValue = SourceTypeValue$Enrolled_Interested/(SourceTypeValue$Not_Enrolled + SourceTypeValue$Enrolled_Interested)
SourceTypeValue = SourceTypeValue[order(SourceTypeValue$SourceTypeValue, decreasing = TRUE), ]
write.csv(SourceTypeValue,"SourceTypeValue.csv")
rm(SourceTypeValue)
SourceType = read.csv("SourceTypeValue.csv")
colnames(SourceType) = c("SourceType", "a","b","SourceTypeValue")
SourceType$a = NULL
SourceType$b = NULL
training = merge(training,SourceType)

ReasonValue = data.frame(unclass(table(training$Reason , training$Status == c("Enrolled", "Interested - Call Back"))))
colnames(ReasonValue) = c("Not_Enrolled", "Enrolled_Interested")
ReasonValue$Not_Enrolled = as.numeric(ReasonValue$Not_Enrolled)
ReasonValue$Enrolled_Interested = as.numeric(ReasonValue$Enrolled_Interested)
ReasonValue$ReasonValue = ReasonValue$Enrolled_Interested/(ReasonValue$Not_Enrolled + ReasonValue$Enrolled_Interested)
ReasonValue = ReasonValue[order(ReasonValue$ReasonValue, decreasing = TRUE), ]
write.csv(ReasonValue,"ReasonValue.csv")
rm(ReasonValue)
Reason = read.csv("ReasonValue.csv")
colnames(Reason) = c("Reason", "a","b","ReasonValue")
Reason$a = NULL
Reason$b = NULL
training = merge(training,Reason)

training$StatusValue = as.numeric(training$StatusValue)
unnati_log = glm(StatusValue~ReferredValue+CenterValue+SourceValue+PriorityValue+CourseValue+SourceTypeValue+ReasonValue, data = training , family = "binomial") 
trainingPrediction = predict(unnati_log, data = training, type = "response")

testing = merge(testing,referred)
testing = merge(testing,centre)
testing = merge(testing,SourceType)
testing = merge(testing,priority)
testing = merge(testing,Course)
testing = merge(testing,source)
testing = merge(testing,Reason)
testingPrediction = predict(unnati_log, newdata = testing, type = "response")

test = merge(test,referred)
rm(referred)
test = merge(test,centre)
rm(centre)
test = merge(test,SourceType)
rm(SourceType)
test = merge(test,priority)
rm(priority)
test = merge(test,Course)
rm(Course)
test = merge(test,source)
rm(source)
test = merge(test,Reason)
rm(Reason)

test$StatusValue = predict(unnati_log, newdata = test, type = "response")

target = subset(test, test$StatusValue > 0.4)
nrow(target)
write.csv(target, "target.csv", row.names = FALSE)


table(training$StatusValue, trainingPrediction > 0.5)
table(testing$StatusValue, testingPrediction > 0.5)
