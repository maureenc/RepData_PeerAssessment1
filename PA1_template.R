library(ggplot2)

setwd("/Users/maureen/DataScience/ReproducibleResearch/week2_assignment")

# ------------------------------------------------------------------------------
# 1. Loading and preprocessing the data

# read in the data
data <- read.csv("activity.csv",header=TRUE,sep=",")

# convert the data from character to POSIXct format
data$date <- as.POSIXct(data$date)

# check the contents of data
str(data)

# ------------------------------------------------------------------------------
# 2. What is mean total number of steps taken per day?

# calculate the total number of steps for each day;
# ... aggregate automatically removes NA values
nsteps <- aggregate(steps ~ date, data, sum)

# make a histogram of the total number of steps taken each day
hist(nsteps$steps,breaks=20,xlab="total number of steps per day",ylab="N",
     main="Histogram of total number of steps")

# calculate the mean if the total number of steps per day
nsteps_mean <- mean(nsteps$steps,na.rm=TRUE)
print(c("Mean of N_steps  :"))
print(nsteps_mean)
#[1] 10766.19

# calculate the median if the total number of steps per day
nsteps_median <- median(nsteps$steps,na.rm=TRUE)
print(c("Median of N_steps  :"))
print(c(nsteps_median))
#[1] 10765

# ------------------------------------------------------------------------------
# 3. What is the average daily activity pattern?

# calculate the mean number of steps per interval, averaged over all days
nsteps_int <- aggregate(steps ~ interval,data,mean)

# make a plot
plot(nsteps_int$interval,nsteps_int$steps,type="l",xlab="interval",
     ylab="mean number of steps",main="Mean number of steps per interval 
     averaged over each day")

# what is the interval where the mean number of steps is largest
imax <- which( nsteps_int$steps == max(nsteps_int$steps) )

# what is the interval number where the maximum in mean(nsteps_int$steps) 
# occurs?
# print(c("imax=",imax))
nsteps_int$interval[which(nsteps_int$steps == max(nsteps_int$steps))]
#[1] 835

# ------------------------------------------------------------------------------
# 4. Imputing missing values

# what is the total number of missing data values:
sum(is.na(data$steps))
#[1] 2304

# fill in missing values by substituing with the average for the corresponding
# interval
data_filled <- data
for (i in which(is.na(data$steps))) {
         interval_match <- which(nsteps_int$interval == data[i,3])
         data_filled[i,1] <- nsteps_int[interval_match,2]
}

# compute the total number of steps for each day, from filled-in dataset
nsteps_filled <- aggregate(steps ~ date, data_filled, sum)

# histogram of total number of steps each day using the filled-in dataset
hist(nsteps_filled$steps,breaks=20,xlab="total number of steps per day",
     ylab="N",main="Histogram of total number of steps for the filled-in 
     dataset")

# compute the mean number of steps per day
nsteps_filled_mean <- mean(nsteps_filled$steps,na.rm=TRUE)
print("Mean of N_steps  :")
print(nsteps_filled_mean)
#[1] 10766.19

# compute the median number of steps per day
nsteps_filled_median <- median(nsteps_filled$steps,na.rm=TRUE)
print("Median of N_steps  : ")
print(nsteps_filled_median)
#[1] 10766.19

# ------------------------------------------------------------------------------
# 5. Are there differences in activity patterns between weekdays and weekends?

# create a new factor variable and add to the filled-in dataset
day <- weekdays(data_filled[,2])
wk <- day
iweekend <- which(day == "Saturday" | day == "Sunday")
wk[iweekend] <- "weekend"
wk[which(wk != "weekend")] <- "weekday"
wk <- as.factor(wk)
data_filled_wk <- cbind(data_filled,wk)

# group the mean number of steps per weekday/weekendday and per interval
nsteps_filled_wk <- aggregate(steps ~ interval + wk,data_filled_wk,mean)

# plot two histograms side-by-side
g <- ggplot(nsteps_filled_wk,aes(interval,steps))
g <- g + geom_line() + facet_grid(. ~ wk)
print(g)

# ------------------------------------------------------------------------------


