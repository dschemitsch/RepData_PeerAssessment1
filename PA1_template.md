---
title: "PA1_template.html"
output: html_document
---
##Loading and preprocessing the data  
Load necessary packages.

```{r, echo=FALSE}
require(data.table)
require(dplyr)
require(ggplot2)
require(timeDate)
```

Read in the data, omitting the missing data.

```{r}
activity <- na.omit(read.csv("./activity.csv"))
```

##What is mean total number of steps taken per day?

Summarize the data to determine the total number of steps taken by date.

```{r}
steps <- as.data.frame(activity %>% group_by(date) %>% summarize(sum(steps)))
colnames(steps) <- c("date","sum.steps")
print(steps)
```
 
Create histogram of the total number of steps taken each day (each column represents the frequency of that bin of steps).


```{r}
hist(as.numeric(steps[,2]), breaks = 10, 
main = "Histogram of total steps taken each day", 
xlab = "Total steps taken each day",
col = "red",
labels = TRUE)
```

Calculate mean and median of the data.

```{r}
mean(as.numeric(steps[,2]))
median(as.numeric(steps[,2]))
```

#What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

First, aggregate data by the average number of steps per interval and then rename the columns.

```{r}
steps2 <- as.data.frame(activity %>% group_by(interval) %>% summarize(mean(steps)))
colnames(steps2) <- c("interval","meansteps")
```

Then, plot the aggregated time series data. The vertical green line represents the max value.

```{r}
y <- ggplot(steps2, aes(x = interval, y = meansteps)) + 
    labs(y = "Mean number of steps",
           x = "Interval") +
    ggtitle("Mean number of steps taken per interval")
      
y + geom_line(size = 1) + 
geom_vline(xintercept = arrange(steps2, desc(meansteps))[1,1], color = "green", size = 1)
```

Find the interval with the max number of steps.

```{r}
arrange(steps2, desc(meansteps))[1,1]
```

#Impute missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
activity <- (read.csv("./activity.csv"))
nrow(activity[!complete.cases(activity),])
```

Replace NA values with their interval means.

```{r}
activity <- as.data.table(activity)
activity$steps <- as.numeric(activity$steps)

activity2 = copy(activity)

activity2 <- activity2[,steps := ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps), by = interval]
```

Summarize date to see the total number of steps per day.

```{r}
steps3 <- as.data.frame(activity2 %>% group_by(date) %>% summarize(sum(steps)))
colnames(steps3) <- c("date","sum.steps")
```

Create historgarm with imputed data.

```{r}
hist(as.numeric(steps3[,2]), breaks = 10, 
main = "Histogram of total steps taken each day (with imputed data)", 
xlab = "Total steps taken each day",
col = "red",
labels = TRUE)
```

Find mean and median with the new imputed data.

```{r}
mean(as.numeric(steps3[,2]))
median(as.numeric(steps3[,2]))
```

After replacing the missing data, the mean value stays the same and the median value changes slightly. The histogram with imputed missing values show that the middle bin has values now - the other bins aren't affected.

#Are there differences in activity patterns between weekdays and weekends?

Determine whether each day is a weekday (TRUE) or not (FALSE).

```{r}
activity2[,weekday:=isWeekday(date)]
```

Summarize data by interval and weekday Vs. weenend and relabel True/False values as "Weekend" or "Weekday".

```{r}
activity_w <- activity2[, mean(steps), by = list(interval, weekday)]
activity_w$day <- factor(activity_w$weekday, labels = c("Weekend", "Weekday"))
```

Plot data by weekday vs. weekend.

```{r} 
y <- ggplot(activity_w, aes(x = interval, y = V1)) + 
    labs(y = "Mean number of steps",
           x = "Interval") +
    ggtitle("Mean number of steps taken per interval")+
    facet_grid(day~.)
      

y + geom_line(size = 1)
```

Comparing the plots visually, we can see some differences between the weekday and weekend data:

* more steps taken on weekdays in intervals 500 to 1000 than on the weekends. 
* generally more steps taken on weekends in intervals 1000 to 2400 than on the weekdays.