unzip("activity.zip")
data <- read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"))

library(dplyr)
data2 <- group_by(data, date)
stepsperday <- summarize(data2, sum(steps, na.rm=TRUE))
colnames(stepsperday)[2] <- "steps"
hist(stepsperday$steps)

##mean total number of steps taken per day
mean(stepsperday$steps)

## median of the total number of steps taken per day
median(stepsperday$steps)


##calculate mean number of steps per 5-min interval averaged across all days
data3 <- group_by(data, interval)
stepsperinterval <- summarize (data3, mean(steps, na.rm = TRUE))

## plots the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
plot(stepsperinterval, type="l")

## Finds which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
colnames(stepsperinterval)[2] <- "stepsavg"
s <- max(stepsperinterval$stepsavg)
data4 <- filter(stepsperinterval, stepsavg == s)
data4
z <- data4[1,1]

## number of NAs for steps variable
n1 <- length(which(is.na(data$steps)))
n1


##adds a column containing the mean number of steps across days for corresponding interval to each row
data5 <- merge(data, stepsperinterval, by = "interval")
##NAs for the steps variable are replaced by mean value across day for corresponding interval
data5$steps[is.na(data5$steps)] <- data5$stepsavg[is.na(data5$steps)]
##creates a dataset equal to the original one, with missing data filled in.
data5 <- select(data5, -stepsavg)
data5 <- arrange(data5, date, interval)
data5 <- select(data5, steps, date, interval)
##checks that the dataset contains no NA
length(which(is.na(data5$steps)))

data7<- data5
data7$date <- strptime(data7$date, format="%Y-%m-%d")

##creates the weekday column 
data7$weekday <- format(data7$date, "%u")
data7$weekday <- as.numeric(data7$weekday)

data7$weekday[data7$weekday <= 5] <- "weekday"
data7$weekday[data7$weekday == "6"|data7$weekday == "7"] <- "weekend"

class(data7$weekday)

data8 <- select(data7, -date)
data8 <- group_by(data8, weekday, interval)
data9 <- summarize(data8, mean(steps))
colnames(data9)[3] <- "steps"

qplot(x= interval, y= steps, data=data9,geom=c("line"),facets=.~weekday)	