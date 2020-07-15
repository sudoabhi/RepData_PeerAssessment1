library(dplyr)
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
#download.file(url,"Data.zip")
#unzip("Data.zip")
list.files()

data<-read.csv("activity.csv")
data$date<-as.Date(as.character(data$date), format="%Y-%m-%d")

####################

# 1
data2<-group_by(data,date)
data21<-summarise(data2,steps=sum(steps,na.rm = T))

hist(data21$steps,main="Histogram of steps",xlab = "Steps Taken Per Day",col = "Red",
     ylim = c(0,20), breaks = seq(0,25000, by=2500))

summary(data21$steps)

#################

#2
data3<-group_by(data,interval)
data31<-summarise(data3,means=mean(steps,na.rm = T))

with(data31,plot(interval,means,type = "l",ylab = "Avg Num of Steps",xlab = "5-Minute Interval"))
title(main="Time series plot of avg steps")

filter(data31,data31$means==max(data31$means))$interval

####################

#3
#steps_na<-is.na(data$steps)
steps_na<-sapply(data$steps,is.na)
sum(steps_na)

#I am replacing with means of 5-minute intervals
to_replace<-match(data$interval,data31$interval)
means<-data31$means[to_replace]

data4<-mutate(data, steps = ifelse(steps_na, yes = means, no = data$steps))

# data4<-data
# for(i in 1:nrow(data)){
#     if(is.na(data[i,1])){
#         
#         data4[i,1]<-data31[data31$interval==data[i,3],2]
#         print(i)
#     }
# }

data41<-group_by(data4,date)
data42<-summarise(data41,steps=sum(steps,na.rm = T))

hist(data42$steps,main="Histogram of steps",xlab = "Steps Taken Per Day",col = "Blue",
     ylim = c(0,30), breaks = seq(0,25000, by=2500))

summary(data42$steps)

par(mfrow=c(1,2),oma=c(0,0,2,0))
hist(data21$steps,main="Before Imputing Data",xlab = "Steps Taken Per Day",col = "Red",
     ylim = c(0,30), breaks = seq(0,25000, by=2500))
hist(data42$steps,main="After Imputing Data",xlab = "Steps Taken Per Day",col = "Blue",
     ylim = c(0,30), breaks = seq(0,25000, by=2500))
title("Histogram of steps", outer = TRUE)
par(mfrow=c(1,1),oma=c(0,0,0,0))

#############################

#4
weekday<-weekdays(data4$date)
weekday<-ifelse(weekday=="Saturday"|weekday=="Sunday",yes="weekend",no="weekday")
weekday<-as.factor(weekday)
data4<-mutate(data4,daytype=weekday)

data43<-group_by(data4,interval,daytype)
data44<-summarise(data43,means=mean(steps,na.rm = T))

library(ggplot2)
g<-ggplot(data44,aes(interval,means,color=daytype)) + geom_line()+ scale_color_manual(values = c("red","blue")) +
    facet_grid(daytype ~ .)+
    xlab("Interval")+ylab("Average Number of steps")
print(g)

# with(data44,{
#     par(mfrow=c(2,1))
#     with(data44[data44$daytype=="weekday",],
#          plot(interval,means,type = "l",ylab = "Avg Num of Steps",xlab = "5-Minute Interval",main = "Weekday"))
#     
#     with(data44[data44$daytype=="weekend",],
#          plot(interval,means,type = "l",ylab = "Avg Num of Steps",xlab = "5-Minute Interval",main = "Weekend"))
#     par(mfrow=c(1,1))
#     })

library(lattice)
xyplot(means ~ interval | daytype, data44, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")




