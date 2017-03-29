# Reproducible Research: Course Project 1

### Load and preprocess the data:

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

* Load and look at data:
```{r load, echo=TRUE}
data <- read.csv("activity.csv")
summary(data)
str(data)
head(data)
```

* Transforming the data:
```{r transform, echo=TRUE}
data$day <- weekdays(as.Date(data$date))
data$DateTime <- as.POSIXct(data$date, format = "%Y-%m-%d")
cleanData<- subset(data, !is.na(data$steps))
head(cleanData)
```

### What is mean total number of steps taken per day?

```{r library, warning=FALSE}
library(ggplot2)
library(plyr)
```

* Calculate the total number of steps taken per day:
```{r sum, echo=TRUE}
sumTable <- aggregate(data$steps ~ data$date, FUN = sum)
colnames(sumTable) <- c("Date", "Steps")
```

* Make a histogram of the total number of steps taken each day:
```{r hist, echo=TRUE}
hist(sumTable$Steps, breaks = 5, xlab = "Steps", main = "Total Steps per Day", col = "blue")
```

* Calculate and report the mean and median of the total number of steps taken per day:
```{r mean, echo=TRUE, results='asis'}
as.integer(mean(sumTable$Steps))
as.integer(median(sumTable$Steps))
```

### What is the average daily activity pattern?

* Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days (y-axis)
```{r pattern, echo=TRUE}
intervalTable <- ddply(cleanData, .(interval), summarize, Avg = mean(steps))
g <- ggplot(intervalTable, aes(x = interval, y = Avg), xlab = "Interval", ylab = "Average Number of Steps")
g + geom_line() + xlab("5-minute Interval") + ylab("Average Number of Steps") + ggtitle("Average Number of Steps per Interval")
```

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r max, echo=TRUE, results='asis'}
maxSteps <- max(intervalTable$Avg)
intervalTable[intervalTable$Avg == maxSteps, 1]
```

### Imputing missing values:

* Calculate and report the total number of missing values in the dataset:
```{r totalNA, echo=TRUE, results='asis'}
sum(is.na(data$steps))
```

The original data set has 2304 rows with missing values.  
My strategy for filling in missing data will be to substitute NAs with the average 5-minute interval based on the day of the week.  

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```{r fillNA, echo=TRUE}
avgTable <- ddply(cleanData, .(interval, day), summarize, Avg = mean(steps))
dataNA <- data[is.na(data$steps), ]
filledData <- merge(dataNA, avgTable, by = c("interval", "day"))
```

* Create a new dataset that is equal to the original dataset but with the missing data filled in:
```{r newData, echo=TRUE}
newData <- filledData[ , c(6,4,1,2,5)]
colnames(newData) <- c("steps", "date", "interval", "day", "DateTime")
mergedData <- rbind(cleanData, newData)
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r new, echo=TRUE, results='asis'}
sumTable2 <- aggregate(mergedData$steps ~ mergedData$date, FUN=sum)
colnames(sumTable2)<- c("Date", "Steps")
as.integer(mean(sumTable2$Steps))
as.integer(median(sumTable2$Steps))
hist(sumTable2$Steps, breaks = 5, xlab = "Steps", main = "Total Steps per Day with NAs Fixed", col = "red")
hist(sumTable$Steps, breaks = 5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col = "blue", add = T)
legend("topright", c("Imputed Data", "Data without NA"), fill = c("red", "blue"))
```

The new mean of the imputed data is 10821 steps compared to the old mean of 10766 steps, so the difference is 55 steps on average per day.

The new median of the imputed data is 11015 steps compared to the old median of 10765 steps, so the difference is 250 steps for the median.

### Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day:
```{r newVar, echo=TRUE, message=FALSE}
mergedData$date <- as.Date(strptime(mergedData$date, format="%Y-%m-%d"))
mergedData$day <- weekdays(mergedData$date)
for (i in 1:nrow(mergedData)) {
    if (mergedData[i,]$day %in% c("Saturday","Sunday")) {
        mergedData[i,]$day<-"weekend"
    }
    else{
        mergedData[i,]$day<-"weekday"
    }
}
stepsByDay <- aggregate(mergedData$steps ~ mergedData$interval + mergedData$day, mergedData, mean)
```

* Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days:
```{r plot, echo=TRUE, message=FALSE}
names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```







