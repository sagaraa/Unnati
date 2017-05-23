unnati = read.csv("unnati.csv")
names(unnati)
unnati$CampaignName = NULL
unnati$Email = NULL
unnati$MobileNumber = NULL
unnati$BatchApplied = NULL
unnati$Remarks = NULL
names(unnati)
unnati$CreatedOn = NULL
unnati$X = NULL
unnati$X.1 = NULL
write.csv(unnati, "unnatiblr.csv")
table(unnati$Course)
unnati2 = read.csv("unnati2.csv", na.strings = " ")
table(unnati2$Course, unnati2$Status)
sub_unnati = subset(unnati, unnati$Status != "New")
table(sub_unnati$Status,sub_unnati$Course)
nrow(table(sub_unnati$ReferredFrom))
nrow(table(sub_unnati$ReferredTo))
table(sub_unnati$ReferredFrom, sub_unnati$Status)

b = data.frame(unclass(table(sub_unnati$ReferredFrom, sub_unnati$Status == c("Enrolled", "Interested - Call Back"))))
colnames(b) = c("Not_Enrolled", "Enrolled_Interested")
b$Reffered_From = as.factor(b$Reffered_From)
b$Not_Enrolled = as.numeric(b$Not_Enrolled)
b$Enrolled = as.numeric(b$Enrolled)
b$efficiency = b$Enrolled_Interested/(b$Not_Enrolled + b$Enrolled_Interested)
b = b[order(b$efficiency, decreasing = TRUE), ]
write.csv(b, file = "Ordered_Reffered_From.csv")


c = data.frame(unclass(table(sub_unnati$Center, sub_unnati$Status == c("Enrolled", "Interested - Call Back"))))
colnames(c) = c("Not_Enrolled", "Enrolled_Interested")
c$Not_Enrolled = as.numeric(c$Not_Enrolled)
c$Enrolled_Interested = as.numeric(c$Enrolled_Interested)
c$efficiency = c$Enrolled_Interested/(c$Not_Enrolled + c$Enrolled_Interested)
c = c[order(c$efficiency, decreasing = TRUE), ]
write.csv(c, file = "Ordered_Centre_wise.csv")

SourceValue = data.frame(unclass(table(train$Source , train$Status == c("Enrolled", "Interested - Call Back"))))
colnames(SourceValue) = c("Not_Enrolled", "Enrolled_Interested")
SourceValue$Not_Enrolled = as.numeric(SourceValue$Not_Enrolled)
SourceValue$Enrolled_Interested = as.numeric(SourceValue$Enrolled_Interested)
SourceValue$SourceValue = SourceValue$Enrolled_Interested/(SourceValue$Not_Enrolled + SourceValue$Enrolled_Interested)
SourceValue = SourceValue[order(SourceValue$SourceValue, decreasing = TRUE), ]
SourceValue = SourceValue[order(SourceValue$SourceValue, decreasing = TRUE),]
write.csv(SourceValue,"SourceValue.csv")

d = data.frame(unclass(table(sub_unnati$Course, sub_unnati$Status == c("Enrolled", "Interested - Call Back"))))
nrow(table(train$Reason))
