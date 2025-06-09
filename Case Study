getwd()

install.packages(tidyverse)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(stats)
library(dplyr)

#### load .csv data frames 
Apr_2020 <- read.csv("cyclistic_trip_data_2020_04.csv")
May_2020 <- read.csv("cyclistic_trip_data_2020_05.csv")
Jun_2020 <- read.csv("cyclistic_trip_data_2020_06.csv")
Jul_2020 <- read.csv("cyclistic_trip_data_2020_07.csv")
Aug_2020 <- read.csv("cyclistic_trip_data_2020_08.csv")
Sep_2020 <- read.csv("cyclistic_trip_data_2020_09.csv")
Oct_2020 <- read.csv("cyclistic_trip_data_2020_10.csv")
Nov_2020 <- read.csv("cyclistic_trip_data_2020_11.csv")
Dec_2020 <- read.csv("cyclistic_trip_data_2020_12.csv")
Jan_2021 <- read.csv("cyclistic_trip_data_2021_01.csv")
Feb_2021 <- read.csv("cyclistic_trip_data_2021_02.csv")
Mar_2021 <- read.csv("cyclistic_trip_data_2021_03.csv")

###Checking for the structure of the dataframes
str(Apr_2020)
str(May_2020)
str(Jun_2020)
str(Jul_2020)
str(Aug_2020)
str(Sep_2020)
str(Oct_2020)
str(Nov_2020)
str(Dec_2020)
str(Jan_2021)
str(Feb_2021)
str(Mar_2021)

####Verfying the column names
colnames(Apr_2020)
colnames(May_2020)
colnames(Jun_2020)
colnames(Jul_2020)
colnames(Aug_2020)
colnames(Sep_2020)
colnames(Oct_2020)
colnames(Nov_2020)
colnames(Dec_2020)
colnames(Jan_2021)
colnames(Feb_2021)
colnames(Mar_2021)

####Preveiwing the dataframes
head(Apr_2020)
head(May_2020)
head(Jun_2020)
head(Jul_2020)
head(Aug_2020)
head(Sep_2020)
head(Oct_2020)
head(Nov_2020)
head(Dec_2020)
head(Jan_2021)
head(Feb_2021)
head(Mar_2021)

####processing & cleaning the data
###changes to columns by conflicting the variables to character variables.
Apr_2020 <- mutate(Apr_2020, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
May_2020 <- mutate(May_2020, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
Jun_2020 <- mutate(Jun_2020, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
Jul_2020 <- mutate(Jul_2020, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
Aug_2020 <- mutate(Aug_2020, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
Sep_2020 <- mutate(Sep_2020, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
Oct_2020 <- mutate(Oct_2020, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
Nov_2020 <- mutate(Nov_2020, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
Dec_2020 <- mutate(Dec_2020, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
Jan_2021 <- mutate(Jan_2021, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
Feb_2021 <- mutate(Feb_2021, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
Mar_2021 <- mutate(Mar_2021, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))


###stacking all dataframes into one big dataframes
All_trips <- bind_rows(Apr_2020, May_2020, Jun_2020, Jul_2020, Aug_2020, Sep_2020, Oct_2020, Nov_2020, Dec_2020, Jan_2021, Feb_2021, Mar_2021)

###Removing columns that are not relevant for the analysis
All_trips <- All_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))


###Data clean up for anaylsis
###Inspecting the new columns
colnames(All_trips)
nrow(All_trips)
dim(All_trips)
head(All_trips)
str(All_trips)
summary(All_trips)

#####Using recode to change the member_casual
All_trips <-  All_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

table(All_trips$member_casual)

###Formatting the date, month, day, year for each ride
All_trips$date <- as.Date(All_trips$started_at) #The default format is yyyy-mm-dd
All_trips$month <- format(as.Date(All_trips$date), "%m")
All_trips$day <- format(as.Date(All_trips$date), "%d")
All_trips$year <- format(as.Date(All_trips$date), "%Y")
All_trips$day_of_week <- format(as.Date(All_trips$date), "%A")

###Calculating the Ride_length
All_trips$ride_length <- difftime(All_trips$ended_at,All_trips$started_at)

###Checking the new dataframes
str(All_trips)

####converting ride_length to numeric
All_trips$ride_length <- as.numeric(as.character(All_trips$ride_length))
is.numeric(All_trips$ride_length)

###Removing bad data
All_trips_v2 <- All_trips[!(All_trips$start_station_name == "HQ QR" | All_trips$ride_length<0),]


## Removing the NA values to get a more precise data summary
All_trips_v3 <- na.omit(All_trips_v2)


#####Analyzing the new dataframes
summary(All_trips_v2$ride_length)

# See the average ride time by each day for members vs casual users
aggregate(All_trips_v2$ride_length ~ All_trips_v2$member_casual + All_trips_v2$day_of_week, FUN = mean)

## Order the weekdays structure for confirmation
All_trips_v3$day_of_week <- ordered(All_trips_v3$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

###the average ride time by each day for members vs casual users
aggregate(All_trips_v2$ride_length ~ All_trips_v2$member_casual + All_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
All_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

###the customer types
customer_summary <- All_trips_v3 %>%
  group_by(member_casual) %>%
  summarize(avg_length = mean(ride_length), median_length = median(ride_length), max_length = max(ride_length), min_length = min(ride_length))


### the weekday types while splitting the customer types
weekday_summary <- All_trips_v3 %>%
  group_by(day_of_week, member_casual) %>%  
  summarize(avg_length = mean(ride_length), median_length = median(ride_length), max_length = max(ride_length), min_length = min(ride_length))

###visualizing the number of rides by rider type
All_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

###creating a visualization for average duration
All_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

####Export summary
counts <- aggregate(All_trips_v2$ride_length ~ All_trips_v2$member_casual + All_trips_v2$day_of_week, FUN = mean)

###save the name in the current working directory
write.table(counts, file = "average_ride_length.csv", row.names=F, sep=",")

write.table(All_trips, file = "All_trips.csv", row.names=F, sep=",")

write.table(customer_summary, file = "customer.csv", row.names=F, sep=",")

write.table(weekday_summary, file = "weekday.csv", row.names=F, sep=",")

write.table(All_trips_v3, file = "All_trips_cleaned_data.csv", row.names=F, sep=",")
