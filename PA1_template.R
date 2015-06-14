## get and load activity
setwd("~/GitHub/RepData_PeerAssessment1")
unzip("activity.zip")
activity <- read.csv("~/GitHub/RepData_PeerAssessment1/activity.csv",colClasses="character")

## get dplyr library
if (!"dplyr" %in% installed.packages()) install.packages(dplyr) 
require(dplyr)
 
if (!"ggplot2" %in% installed.packages()) install.packages(ggplot2) 
require(ggplot2)

## transform data set
activity$date <- as.Date(activity$date, format="%Y-%m-%d")
activity$steps <-as.numeric(activity$steps)
activity$interval <-as.numeric(activity$interval)

#activity.na <- na.omit(activity)
act.df = tbl_df(activity)

spd <- summarise(select(group_by(act.df,date),steps),TotalSteps=sum(steps,na.rm=TRUE))

                 
                 
spd <- filter(spd,TotalSteps !=0)                 
#steps_per_day <- aggregate(steps ~ date, activity, sum)

ggplot(spd, aes(x = TotalSteps)) + 
  geom_histogram(fill = "green", binwidth = 1000) + 
  labs(title="Histogram of Steps Taken per Day", 
       x = "Number of Steps per Day", y = "Number of times (Count)") + 
  theme_bw() 


hist(steps_per_day$steps,col=3,main="Histogram of Frequency of steps per day",
     xlab="Number of steps" )

spd.mean <- mean(spd$TotalSteps)

spd.median <- median(spd$TotalSteps)


 steps_per_interval <- aggregate(x=list(meanSteps=activity$steps), 
                                by = list(interval = activity$interval),
                                FUN=mean, na.rm=TRUE)

spi <-summarise(select(group_by(act.df,interval),steps),meanSteps=mean(steps,na.rm=TRUE))


ggplot(data=spi, aes(x=interval, y=meanSteps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken") 

intmax <- filter(spi , meanSteps==max(meanSteps))
interval.max <-intmax[,1]

interval.max <- which.max(steps_per_interval$meanSteps)


sumNA<-sum(is.na(activity$steps))
print(sumNA)


join <- left_join(act.df,spi)
NAreplaced<-mutate(join,Steps = (ifelse(is.na(steps), meanSteps, steps)))
sumstepsNA<-summarise(select(group_by(NAreplaced,date),Steps),totalSteps=sum(Steps,na.rm=TRUE))
plot(sumstepsNA$date,sumstepsNA$totalSteps,type="h",xlab="By date",ylab="Total steps")   

nasum <- summarise(sumstepsNA,mean = mean(totalSteps),median=median(totalSteps))


NAreplaced.days<-mutate(NAreplaced,day=weekdays(date))
NAreplaced.newfactor<-mutate(NAreplaced.days,new=ifelse(day=="Saturday"|day=="Sunday","Weekend","Weekday"))
NAreplaced.newfactor$new<-as.factor(NAreplaced.newfactor$new)


sumsteps<-summarise(select(group_by(NAreplaced.newfactor,interval , new ,Steps),stepmean=mean(Steps,na.rm=TRUE)))



ggplot(sumsteps, aes(interval, stepmean)) + 
  geom_line() + 
  facet_grid(new ~ .) +
  xlab("5-minute interval") + 
  ylab("average number of steps")
