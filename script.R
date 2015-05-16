library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)

##Loading and preprocessing the data

data <- read.csv("data/activity.csv")
data$date <- ymd(data$date)

plot1data <- summarise(group_by(data, date), step = sum(steps))

png("figure/hist.png", width=800, height=300)
summarise(group_by(data, date), step = sum(steps))%>%
ggplot() + 
    geom_histogram(aes(x=date, weight=step)) +
    ylab("Steps per day") +
    scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("3 day"), minor_breaks = "1 day")
dev.off()

c <- summarise(group_by(data, date), step = sum(steps))
qplot(c$date, y = c$step,
    geom="histogram", stat= "identity"
    )

summarise(group_by(data, date), mean = mean(steps), median = median(steps))%>%
    ggplot() +
    geom_line(aes(date, y=mean, color="Mean")) +
    geom_line(aes(date, y=median, color= "Median")) +
    labs(y="")


png("figure/steps.png", width=800, height=300)
plot2data <- summarise(group_by(data, date), mean = mean(steps, na.rm = TRUE), median = median(steps))
plot(plot2data$date, 
     plot2data$mean, 
     type="l", 
     col = "blue", 
     ylab = "",
     xlab = "") 
lines(plot2data$date, plot2data$median, col = "red")
legend("topleft", c("Mean", "Median"), lty = c(1,1), col = c("blue","red"))
dev.off()    

data$f <- factor(data$interval)

data$time <- ymd_hm(paste("2015/05/13", format(update(data$date, minute = data$interval), "%H:%M"), sep = " "))

png("figure/dailyactivity.png", width=800, height=300)
summarise(group_by(data, time), mean = mean(steps, na.rm = TRUE))%>%
    ggplot() +
    geom_line(aes(time, y=mean)) + 
    scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("2 hour")) +
    labs(y="Steps per period")
dev.off()

summarise(group_by(data, interval), mean = mean(steps, na.rm = TRUE))%>%
arrange(desc(mean))%>%
    head(1)
    

sum(is.na(data$steps))
intervalsmean <- summarise(group_by(data, interval), mean = mean(steps, na.rm = TRUE))
datawithoutna <- data
ind <- which(is.na(data$steps), arr.ind = TRUE)
for (i in 1:length(ind)) {
    interval <- datawithoutna$interval[ind[i]]
    datawithoutna$steps[ind[i]] <- intervalsmean$mean[intervalsmean$interval == interval]
}
sum(is.na(datawithoutna$steps))

png("figure/histwithoutnas.png", width=800, height=300)
summarise(group_by(datawithoutna, date), step = sum(steps))%>%
    ggplot() + 
    geom_histogram(aes(x=date, weight=step)) +
    ylab("Steps per day") +
    scale_x_datetime(labels = date_format("%m/%d"), breaks = date_breaks("5 day"), minor_breaks = "1 day")
dev.off()

png("figure/stepswithoutnas.png", width=800, height=300)
plot5data <- summarise(group_by(datawithoutna, date), mean = mean(steps, na.rm = TRUE), median = median(steps))
plot(plot5data$date, 
     plot5data$mean, 
     type="l", 
     col = "blue", 
     ylab = "",
     xlab = "") 
lines(plot5data$date, plot5data$median, col = "red")
legend("topleft", c("Mean", "Median"), lty = c(1,1), col = c("blue","red"))
dev.off()

datawithoutna$weekday <- ifelse(weekdays(datawithoutna$date) == "Sab" | weekdays(datawithoutna$date) == "Dom", "weekend", "weekday")
datawithoutna$weekday <- factor(c("weekday","weekend"))

png("figure/activitypatterns.png", width=800, height=300)
summarise(group_by(datawithoutna, time, weekday), mean = mean(steps))%>%
    ggplot() +
    geom_line(aes(time, y=mean)) + 
    scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("2 hour")) +
    facet_wrap( ~ weekday, nrow = 2, ncol = 1 ) +
    labs(y = "Number of Steps")
dev.off()

knit2html('PA1_template.Rmd', 
                    output = 'PA1_template.html',
                    header = '<title>PA1_template</title>',
                    options=c("smartypants","mathjax","highlight_code"))