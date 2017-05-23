mvt = read.csv("mvt.csv", stringsAsFactors = FALSE)
str(mvt)
mvt$Date = strptime(mvt$Date, format = "%m/%d/%y %H:%M")
mvt$Weekdays = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
str(mvt)

weekdaysCount = as.data.frame(table(mvt$Weekdays))
str(weekdaysCount)
ggplot(weekdaysCount, aes( x = Var1, y = Freq)) + geom_line(aes(group = 1))

weekdaysCount$Var1 = factor(weekdaysCount$Var1, ordered = TRUE, levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

ggplot(weekdaysCount, aes( x = Var1, y = Freq)) + geom_line(aes(group = 1), linetype = 2)
ggplot(weekdaysCount, aes( x = Var1, y = Freq)) + geom_line(aes(group = 1), alpha = 0.3)

table(mvt$Weekdays, mvt$Hour)
a = as.data.frame(table(mvt$Weekdays, mvt$Hour))
str(a)
a$hour = as.numeric(as.character(a$Var2))
ggplot(a, aes(x = hour, y = Freq)) + geom_line(aes(group = Var1, color = Var1), size = 2)
a$Var1 = factor(a$Var1, ordered = TRUE, levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

ggplot(a, aes(x = hour, y = Var1)) + geom_tile(aes(fill = Freq))
ggplot(a, aes(x = hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = "Total MV theft", low = "white", high = "red") + theme(axis.title.y = element_blank())

