# RR Week 2 assignment

setwd("~/Documents/Documenten/R/Reproducible Research/Assignment week 2")
library(lubridate)
library(xtable)

#read in files
activity <- read.csv("activity.csv")
activity <- transform(activity, date = as.Date(activity$date))
activity <- transform(activity, steps = as.numeric(activity$steps))
activity <- transform(activity, interval = as.numeric(activity$interval))

#histogram of steps total
hist(activity$steps, xlab = "number of steps", ylab = "amount of steps", col = "green")

#mean and median number of steps
mean_steps <- tapply(activity$steps, activity$date, mean)
median_steps <- tapply(activity$steps, activity$date, median)
frame <- data.frame(mean_steps, median_steps)
frame

#plot of steps on average each day
agg <- aggregate(steps ~ date, activity, mean)
plot(agg$date, agg$steps, type = "l")

# which interval has most steps on average per day?
agg_int <- aggregate(.~ interval, activity, max)
agg_int[which.max(agg_int$steps), ]

#missing values
table(is.na(activity))
library(Hmisc)
activity_noNA <- activity
activity_noNA$steps <- with(activity_noNA, impute(steps, mean))

#histogram for imputed set
hist(activity_noNA$steps, xlab = "number of steps", ylab = "amount of steps", col = "green")
mean_steps_noNA <- tapply(activity_noNA$steps, activity_noNA$date, mean)
median_steps_noNa <- tapply(activity_noNA$steps, activity_noNA$date, median)
frame_noNA <- data.frame(mean_steps_noNA, median_steps_noNa)

#differ from earlier solution?
diff <- cbind(frame, frame_noNA)

#weekdays
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
activity_noNA$daytype <- factor((weekdays(activity_noNA$date) %in% weekday), levels = c(FALSE, TRUE), labels = c("Weekend", "Weekday"))
agg_day <- aggregate(steps ~ interval + daytype, activity_noNA, mean)
par(mfrow = c(2, 1))
g <- ggplot(data = agg_day, aes(interval, steps))
p <- g + geom_line(color = "red") + facet_grid(daytype~.) + labs(title = "Average steps per interval on weekdays or in the weekend")
ggsave('steps.png', width = 16, height = 9, dpi = 100)