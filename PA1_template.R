# Load the data
data <- read.csv("activity.csv", header = T, sep = ",")

# Histogram of the total number of steps taken each day
totalSteps <- tapply(data$steps, data$date, sum, na.rm=T)
hist(totalSteps, xlab = "Steps per Day", main = "Histogram of Total Steps Taken Each Day")

# Mean and median number of steps taken each day
print(c("The mean is", round(mean(totalSteps))))
print(c("The median is", round(median(totalSteps))))

# Time series plot of the average number of steps taken
avgSteps <- tapply(data$steps, data$interval, mean, na.rm=T)
plot(avgSteps ~ unique(data$interval), type="l", xlab = "5-min Interval")

# The 5-minute interval that, on average, contains the maximum number of steps
avgSteps[which.max(avgSteps)]

# Code to describe and show a strategy for imputing missing data
table(is.na(data) == TRUE)
summary(data)
dataNoNA <- data  
for (i in 1:nrow(data)){
  if(is.na(data$steps[i])){
    dataNoNA$steps[i]<- avgSteps[[as.character(data[i, "interval"])]]
  }
}

# Histogram of the total number of steps taken each day after missing values are imputed
totalSteps <- tapply(dataNoNA$steps, dataNoNA$date, sum, na.rm=T)
hist(totalSteps, xlab = "Steps per Day", main = "Histogram of Total Steps Taken Each Day (no NAs)")

# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
dataNoNA$weekday <- c("weekday")
dataNoNA[weekdays(as.Date(dataNoNA[, 2])) %in% c("Saturday", "Sunday", "samedi", "dimanche", "saturday", "sunday", "Samedi", "Dimanche"), ][4] <- c("weekend")
table(dataNoNA$weekday == "weekend")

dataNoNA$weekday <- factor(dataNoNA$weekday)
dataNoNA_weekend <- subset(dataNoNA, dataNoNA$weekday == "weekend")
dataNoNA_weekday <- subset(dataNoNA, dataNoNA$weekday == "weekday")

mean_dataNoNA_weekday <- tapply(dataNoNA_weekday$steps, dataNoNA_weekday$interval, mean)
mean_dataNoNA_weekend <- tapply(dataNoNA_weekend$steps, dataNoNA_weekend$interval, mean)

library(lattice)
df_weekday <- data.frame(interval = unique(dataNoNA_weekday$interval), avg = as.numeric(mean_dataNoNA_weekday), day = rep("weekday", length(mean_dataNoNA_weekday)))
df_weekend <- data.frame(interval = unique(dataNoNA_weekend$interval), avg = as.numeric(mean_dataNoNA_weekend), day = rep("weekend", length(mean_dataNoNA_weekend)))
df_final <- rbind(df_weekday, df_weekend)

xyplot(avg ~ interval | day, data = df_final, layout = c(1, 2), 
       type = "l", ylab = "Number of steps")

