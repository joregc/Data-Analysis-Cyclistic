#Packages installed
#install.packages("tidyverse")

library(tidyr)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
trips01 <- read_csv("./Data/202201-divvy-tripdata.csv")


list_of_files <- list.files(path = "Data/",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)


#adds 12 files from 2022 to single dataframe
print(list_of_files)

all_trips <- readr::read_csv(list_of_files, id = "file_name")

str(trips01)



users <- all_trips %>%
   count( member_casual)
print(users)

users %>%
  summarise(total_users = sum(n))


#checking for NA values in the dataset
filter(all_trips, is.na(ended_at))
filter(all_trips, is.na(ended_at) | is.na(started_at) | is.na(rideable_type))

#Count of each type of user
users <- all_trips %>%
  count( member_casual)
print(users)


#creates a dataframe with travels duration
alltripwithDuration <- select(all_trips, ride_id, rideable_type, started_at, ended_at, member_casual)
alltripwithDuration <- mutate(alltripwithDuration, durationSecs = ended_at - started_at)
alltripwithDuration <- mutate(alltripwithDuration, durationMin = alltripwithDuration$durationSecs/60)
alltripwithDuration <- mutate(alltripwithDuration, durationMin =minute(seconds_to_period(durationSecs)))

str(alltripwithDuration)
head(alltripwithDuration)


##RUN TO SUMMARIZE STATS FOR VARIABLE
tripsWeekday <- select(alltripwithDuration, started_at, ended_at, durationSecs, durationMin, member_casual, rideable_type) 
tripsWeekday <- tripsWeekday %>%
  mutate(WeekdayName = wday(started_at, label = TRUE), Weekday = wday(started_at, locale =  Sys.getlocale("LC_TIME")), Duration = ended_at - started_at, DurationInMin = Duration/60)
head(tripsWeekday)



alltripwithDuration %>%
  group_by(member_casual) %>%
  summarise(AVG_Min_duration =  mean(durationMin))

alltripwithDuration %>%
  group_by(member_casual) %>%
  summarise(duracion = sum(durationSecs))

#count of trips per month

tripsPerMonth <- alltripwithDuration %>%
  count(month(started_at))
tripsPerMonth$`month(started_at)` <- month.abb[tripsPerMonth$`month(started_at)`] 
tripsPerMonth

#Trips per month by costumer
tripsPerMonthByConstumer <- alltripwithDuration %>%
  group_by(member_casual, month(started_at)) %>%
  count(member_casual)

head(tripsPerMonthByConstumer, 15)
tripsPerMonthByConstumer <- tripsPerMonthByConstumer %>%
  mutate(monthName = month.abb[`month(started_at)`])
tripsPerMonthByConstumer

ggplot(data = tripsPerMonthByConstumer) +
  geom_bar(mapping = aes(x = fct_inorder(monthName), y = n,fill = member_casual), stat='identity', position = "dodge") +
  labs( title = "Number of rides each month per type of user",
        x = "Month",
        y = "# of rides"
        )+
  scale_y_continuous(labels = scales::comma)


#Trips per type of ride
alltripwithDuration %>%
  count(alltripwithDuration$rideable_type)

#trips per type and costumer
tripsPerRideAndCustomer <- alltripwithDuration %>%
  count(rideable_type, member_casual)
ggplot(data = tripsPerRideAndCustomer) + 
  geom_bar(mapping = aes(x = rideable_type, y = n, fill = member_casual), stat= 'identity', position = "dodge")



#Trips per weekday
tripsWeekday <- select(alltripwithDuration, started_at, ended_at,  member_casual, rideable_type, durationSecs, durationMin) 
tripsWeekday <- tripsWeekday %>%
  mutate(WeekdayName = wday(started_at, label = TRUE), Weekday = wday(started_at))
tripsWeekday

weekday_names <- c("Sun","Mon", "Tue", "Wen", "Thu", "Fri", "Sat", "Sun")
countOfTripsPerDay <- tripsWeekday %>%
  count(Weekday) %>%
  mutate(Weekday = weekday_names[Weekday])

countOfTripsPerDay %>%
  arrange(n) %>%
ggplot() +
  geom_bar(mapping = aes(x =reorder(Weekday, n), y = n, fill = Weekday), stat = 'Identity')

#Trips per weekday and costumer
countOfTripsPerDayCostumer <- tripsWeekday %>%
  count(Weekday, member_casual) %>%
  mutate(Weekday = weekday_names[Weekday])

countOfTripsPerDayCostumer %>%
  ggplot(mapping = aes(x =fct_inorder(Weekday), y = n, fill = member_casual)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(
        x = "Weekday",
        y = "# of rides",

  )+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_discrete(name ="Customer")



#AVG ride duration

tripsWeekday %>%
  group_by(Weekday) %>%
  ggplot(mapping = aes(x = WeekdayName, y = mean(Duration))) +
  geom_bar(stat = 'identity')

newData <- tripsWeekday %>%
  group_by(Weekday, WeekdayName) %>%
  summarize(avgDuration = mean(Duration)/60, .groups = 'drop') %>%
  ggplot(mapping = aes(x = WeekdayName, y = avgDuration)) + 
  geom_bar(stat = 'identity') + 
  labs(
    title = 'Avg ride duration by day',
    y = 'Min'
  )
  head(newData)
  
  #AVG ride duration and costumer
  
  tripDurationWeekAndCostumer <- tripsWeekday %>%
    group_by(Weekday, WeekdayName, member_casual)
  
    tripsWeekday %>%
    group_by(Weekday, WeekdayName, member_casual) %>%
    summarize(avgDuration = mean(Duration)/60, .groups = 'drop') %>%
    ggplot(mapping = aes(x = WeekdayName, y = avgDuration, fill = member_casual)) + 
    geom_bar(stat = 'identity', position = 'dodge') + 
    labs(
      title = 'Avg ride duration by day and Costumer',
      y = 'Min'
    )
  
  print(tripDurationWeekAndCostumer)
  
  #Boxplot with outliers
  
  str(tripsWeekday)
  head(tripsWeekday)
  tripsWeekday %>%
    ggplot() +
    geom_boxplot(mapping = aes(x = WeekdayName, y = DurationInMin))
  
  tripsWeekday %>%
    ggplot( ) +
    geom_boxplot(mapping = aes(x = WeekdayName, y = DurationInMin), outlier.shape =  NA ) +
    coord_cartesian(ylim = c(0, 60))
  
  res <- tripsWeekday %>%
    select(Weekday) %>%
    group_by(Weekday)
res  
  max(tripsWeekday$Duration)

  #Remove negative durations
  tripsWeekday <- tripsWeekday %>%
    filter(started_at < ended_at)


#Getting IQR 25 & 75:
groupedByDays <-tripsWeekday%>% group_by(WeekdayName)
Qtls <- tapply(tripsWeekday$DurationInMin, tripsWeekday$WeekdayName , quantile)
print(Qtls)

Q1s <- sapply(1:7, function(i) Qtls[[i]][2])
Q3s <- sapply(1:7, function(i) Qtls[[i]][4])

#Getting IQR Limits

IQRs <- tapply(tripsWeekday$DurationInMin, tripsWeekday$WeekdayName, IQR)

lower <- Q1s - 1.5 * Q1s
upper <- Q3s + 1.5 * Q3s


tripswDay <- split(tripsWeekday, tripsWeekday$WeekdayName)
head(tripswDay[2])


lower[1]
upper[2]

noOutlier <- NULL
for (i in 1:7){
  out <- subset(tripswDay[[i]], (tripswDay[[i]]$DurationInMin > lower[i]) & (tripswDay[[i]]$DurationInMin < upper[i]))
 noOutlier <- rbind(noOutlier, out) 
}

dim(noOutlier)
tail(noOutlier)



noOutlier %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = WeekdayName, y = DurationInMin), outlier.shape = NA )

head(noOutlier)

# STATS WITHOUTH OUTLIERS-------------------

tripsPerMonth <- noOutlier %>%
  count(month(started_at))

tripsPerMonth$`month(started_at)` <- month.abb[tripsPerMonth$`month(started_at)`]

head(tripsPerMonth)

tripsPerMonth %>%
  ggplot(mapping =  aes(x =tripsPerMonth$`month(started_at)`, y = n)) +
  geom_bar(stat='identity') + 
  scale_x_discrete(limits = month.abb)+
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = 'Number of trips per month',
    x = 'Month',
    y = 'Number of trips'
  )


#Trips per weekday and costumer
countOfTripsPerDayCostumer <- noOutlier %>%
  count(Weekday, member_casual) %>%
  mutate(Weekday = weekday_names[Weekday])

countOfTripsPerDayCostumer %>%
  ggplot(mapping = aes(x =fct_inorder(Weekday), y = n, fill = member_casual)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(
    x = "Weekday",
    y = "# of rides",
    
  )+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_discrete(name ="Customer")


#Trips per month by costumer
tripsPerMonthByConstumer <- noOutlier %>%
  group_by(member_casual, month(started_at)) %>%
  count(member_casual)

tripsPerMonthByConstumer <- tripsPerMonthByConstumer %>%
  mutate(monthName = month.abb[`month(started_at)`])

ggplot(data = tripsPerMonthByConstumer) +
  geom_bar(mapping = aes(x = fct_inorder(monthName), y = n,fill = member_casual), stat='identity', position = "dodge") +
  labs( title = "Number of rides each month per type of user",
        x = "Month",
        y = "# of rides"
  )+
  scale_y_continuous(labels = scales::comma)

#Trips per weekday and costumer
countOfTripsPerDayCostumer <- noOutlier %>%
  count(Weekday, member_casual) %>%
  mutate(Weekday = weekday_names[Weekday])

countOfTripsPerDayCostumer %>%
  ggplot(mapping = aes(x =fct_inorder(Weekday), y = n, fill = member_casual)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(
    x = "Weekday",
    y = "# of rides",
    
  )+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_discrete(name ="Customer")

#AVG ride duration and costumer

avgDurationByCustomer <- noOutlier %>%
  group_by(member_casual) %>%
  summarize(mean(durationMin))

avgDurationByCustomer

avgDurationByCustomer %>%
ggplot(mapping = aes(x = member_casual, y = `mean(durationMin)`), color = member_casual ) +
  geom_bar(stat = 'identity')+
  labs(
    x = 'Member',
    y = 'AVG Duration',
    title = 'Average duration'
  )
  
tripDurationWeekAndCostumer <- noOutlier %>%
  group_by(Weekday, WeekdayName, member_casual)

tripsWeekday %>%
  group_by(Weekday, WeekdayName, member_casual) %>%
  summarize(avgDuration = mean(Duration)/60, .groups = 'drop') %>%
  ggplot(mapping = aes(x = WeekdayName, y = avgDuration, fill = member_casual)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  labs(
    title = 'Avg ride duration by day and Costumer',
    y = 'Min'
  )

tripsWeekday %>%
  group_by(member_casual) %>%
  summarize(mean(durationMin))


#trips per type and costumer
tripsPerRideAndCustomer <- noOutlier %>%
  count(rideable_type, member_casual)
ggplot(data = tripsPerRideAndCustomer) + 
  geom_bar(mapping = aes(x = rideable_type, y = n, fill = member_casual), stat= 'identity', position = "dodge")+
  labs(
    title = 'Type of ride per costumer',
    x = 'Rideable type',
    y = 'Number of users'
  )

#hist Starting time

repeatedhours <- noOutlier %>%
  count(hour(started_at), member_casual)

repeatedhours

repeatedhours%>%
  ggplot(mapping = aes(x = `hour(started_at)` , y=n,  color = member_casual )) +
  geom_point() +
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  labs(y = '# of rides per hour',
       x = 'Hour of the day'
       )+
  scale_x_continuous(breaks=seq(0,23,by=1))
 
#hist Starting time and day

repeatedhours_day <- noOutlier %>%
  count(hour(started_at),  WeekdayName, member_casual)

repeatedhours_day

graph <- repeatedhours_day%>%
  ggplot(mapping = aes(x = `hour(started_at)` , y=n,  color = member_casual )) +
  geom_point() +
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  labs(y = '# of rides per hour',
       x = 'Hour of the day'
  )+
  scale_x_continuous(breaks=seq(0,23,by=2))

graph + facet_wrap(~WeekdayName, scales = 'free')

# --------



