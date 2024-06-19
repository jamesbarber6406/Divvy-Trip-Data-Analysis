library(tidyverse)
library(conflicted)
conflict_prefer("filter","dplyr")
conflict_prefer("lag","dplyr")

# ADD DATA FROM 2013

v1_2013 <- read_csv("Divvy_Trips_2013.csv")


vf_2013 <- rename(v1_2013, ride_id = "trip_id",
                  rideable_type = "bikeid",
                  started_at = "starttime",
                  ended_at = "stoptime",
                  start_station_name = "from_station_name",
                  end_station_name = "to_station_name",
                  start_station_id = "from_station_id",
                  end_station_id = "to_station_id",
                  member_casual = "usertype")

vf_2013 <- mutate(vf_2013, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

# ADD DATA FROM 2014

q1q2_2014 <- read_csv("Divvy_Trips_2014_Q1Q2.csv")
m7_2014 <- read_csv("Divvy_Trips_2014_07.csv")
m0809_2014 <- read_csv("Divvy_Trips_2014_0809.csv")
q4_2014 <- read_csv("Divvy_Trips_2014_Q4.csv")

vf_2014 <- bind_rows(q1q2_2014,m7_2014,m0809_2014,q4_2014)

vf_2014 <- rename(vf_2014, ride_id = "trip_id",
                    rideable_type = "bikeid",
                    started_at = "starttime",
                    ended_at = "stoptime",
                    start_station_name = "from_station_name",
                    end_station_name = "to_station_name",
                    start_station_id = "from_station_id",
                    end_station_id = "to_station_id",
                    member_casual = "usertype")


# for some reason started_at must be converted to numeric first before datetime
# but ended_at can be converted directly to datetime
vf_2014 <- mutate(vf_2014, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type),
                  started_at = mdy_hm(vf_2014$started_at),
                  ended_at = mdy_hm(vf_2014$ended_at))


# ADD DATA FROM 2015

q1_2015 <- read_csv("Divvy_Trips_2015_Q1.csv")
q2_2015 <- read_csv("Divvy_Trips_2015_Q2.csv")
m7_2015 <- read_csv("Divvy_Trips_2015_07.csv")
m8_2015 <- read_csv("Divvy_Trips_2015_08.csv")
m9_2015 <- read_csv("Divvy_Trips_2015_09.csv")
q4_2015 <- read_csv("Divvy_Trips_2015_Q4.csv")

vf_2015 <- bind_rows(q1_2015,q2_2015,m7_2015,m8_2015,m9_2015,q4_2015)

vf_2015 <- rename(vf_2015, ride_id = "trip_id",
                  rideable_type = "bikeid",
                  started_at = "starttime",
                  ended_at = "stoptime",
                  start_station_name = "from_station_name",
                  end_station_name = "to_station_name",
                  start_station_id = "from_station_id",
                  end_station_id = "to_station_id",
                  member_casual = "usertype")


vf_2015 <- mutate(vf_2015, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type),
                  started_at = mdy_hm(vf_2015$started_at),
                  ended_at = mdy_hm(vf_2015$ended_at))

# ADD DATA FROM 2016

q1_2016 <- read_csv("Divvy_Trips_2016_Q1.csv")
m4_2016 <- read_csv("Divvy_Trips_2016_04.csv")
m5_2016 <- read_csv("Divvy_Trips_2016_05.csv")
m6_2016 <- read_csv("Divvy_Trips_2016_06.csv")
q3_2016 <- read_csv("Divvy_Trips_2016_Q3.csv")
q4_2016 <- read_csv("Divvy_Trips_2016_Q4.csv")

v1_2016 <- bind_rows (q1_2016,m4_2016,m5_2016,m6_2016)

v1_2016$starttime = mdy_hm(v1_2016$starttime)
v1_2016$stoptime = mdy_hm(v1_2016$stoptime)

v1_2016 <- mutate(v1_2016, starttime = as.POSIXct(starttime, tz = ""),
                  stoptime = as.POSIXct(stoptime, tz = ""))

v2_2016 <- bind_rows (q3_2016,q4_2016)

v2_2016$starttime = mdy_hms(v2_2016$starttime)
v2_2016$stoptime = mdy_hms(v2_2016$stoptime)

v2_2016 <- mutate(v2_2016, starttime = as.POSIXct(starttime, tz = ""),
                  stoptime = as.POSIXct(stoptime, tz = ""))

vf_2016 <- bind_rows (v1_2016,v2_2016)

vf_2016 <- rename(vf_2016, ride_id = "trip_id",
                  rideable_type = "bikeid",
                  started_at = "starttime",
                  ended_at = "stoptime",
                  start_station_name = "from_station_name",
                  end_station_name = "to_station_name",
                  start_station_id = "from_station_id",
                  end_station_id = "to_station_id",
                  member_casual = "usertype")

vf_2016 <- mutate(vf_2016, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

# ADD DATA FROM 2017

q1_2017 <- read_csv("Divvy_Trips_2017_Q1.csv")
q2_2017 <- read_csv("Divvy_Trips_2017_Q2.csv")
q3_2017 <- read_csv("Divvy_Trips_2017_Q3.csv")
q4_2017 <- read_csv("Divvy_Trips_2017_Q4.csv")


v1_2017 <- q4_2017
v1_2017$start_time = mdy_hm(v1_2017$start_time)
v1_2017$end_time = mdy_hm(v1_2017$end_time)

v2_2017 <- bind_rows(q1_2017,q2_2017,q3_2017)
v2_2017$start_time = mdy_hms(v2_2017$start_time)
v2_2017$end_time = mdy_hms(v2_2017$end_time)

vf_2017 <- bind_rows (v1_2017,v2_2017)

vf_2017 <- rename(vf_2017, ride_id = "trip_id",
                  rideable_type = "bikeid",
                  started_at = "start_time",
                  ended_at = "end_time",
                  start_station_name = "from_station_name",
                  end_station_name = "to_station_name",
                  start_station_id = "from_station_id",
                  end_station_id = "to_station_id",
                  member_casual = "usertype")

vf_2017 <- mutate(vf_2017, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type),
                  started_at = as.POSIXct(started_at, tz = ""),
                  ended_at = as.POSIXct(ended_at, tz = ""))

vf_2017 <- vf_2017[order(vf_2017$started_at),]
vf_2017 <- vf_2017[153:3829014,]
vf_2017 <- vf_2017[order(vf_2017$ended_at),]


# ADD 2018 TO DATA

q1_2018 <- read_csv("Divvy_Trips_2018_Q1.csv")
q2_2018 <- read_csv("Divvy_Trips_2018_Q2.csv")
q3_2018 <- read_csv("Divvy_Trips_2018_Q3.csv")
q4_2018 <- read_csv("Divvy_Trips_2018_Q4.csv")

q1_2018 <- rename(q1_2018, ride_id = "01 - Rental Details Rental ID",
                  rideable_type = "01 - Rental Details Bike ID",
                  started_at = "01 - Rental Details Local Start Time",
                  ended_at = "01 - Rental Details Local End Time",
                  start_station_name = "03 - Rental Start Station ID" ,
                  end_station_name = "02 - Rental End Station Name",
                  start_station_id = "03 - Rental Start Station ID",
                  end_station_id = "02 - Rental End Station ID",
                  member_casual = "User Type")

v1_2018 <- bind_rows(q2_2018,q3_2018,q4_2018)

v1_2018 <- rename(v1_2018, ride_id = "trip_id",
                  rideable_type = "bikeid",
                  started_at = "start_time",
                  ended_at = "end_time",
                  start_station_name = "from_station_name",
                  end_station_name = "to_station_name",
                  start_station_id = "from_station_id",
                  end_station_id = "to_station_id",
                  member_casual = "usertype")

vf_2018 <- bind_rows(q1_2018,v1_2018)

vf_2018 <- mutate(vf_2018, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

#add the data from 2019
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")

v1_2019 <- bind_rows (q1_2019,q3_2019,q4_2019)

v1_2019 <- rename(v1_2019, ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  end_station_name = to_station_name,
                  start_station_id = from_station_id,
                  end_station_id = to_station_id,
                  member_casual = usertype)

q2_2019 <- rename(q2_2019, ride_id = "01 - Rental Details Rental ID",
                  rideable_type = "01 - Rental Details Bike ID",
                  started_at = "01 - Rental Details Local Start Time",
                  ended_at = "01 - Rental Details Local End Time",
                  start_station_name = "03 - Rental Start Station Name",
                  end_station_name = "02 - Rental End Station Name",
                  start_station_id = "03 - Rental Start Station ID",
                  end_station_id = "02 - Rental End Station ID",
                  member_casual = "User Type")

vf_2019 <- bind_rows (v1_2019, q2_2019)

vf_2019 <- mutate(vf_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

# add data from 2020
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")
m4_2020 <- read_csv("202004-divvy-tripdata.csv")
m5_2020 <- read_csv("202005-divvy-tripdata.csv")
m6_2020 <- read_csv("202006-divvy-tripdata.csv")
m7_2020 <- read_csv("202007-divvy-tripdata.csv")
m8_2020 <- read_csv("202008-divvy-tripdata.csv")
m9_2020 <- read_csv("202009-divvy-tripdata.csv")
m10_2020 <- read_csv("202010-divvy-tripdata.csv")
m11_2020 <- read_csv("202011-divvy-tripdata.csv")
m12_2020 <- read_csv("202012-divvy-tripdata.csv")

m12_2020 <- mutate(m12_2020, start_station_id = as.double(start_station_id),
                    end_station_id = as.double(end_station_id))

vf_2020 <- bind_rows(q1_2020,m4_2020,m5_2020,m6_2020,m7_2020,m8_2020,m9_2020,
                     m10_2020,m11_2020,m12_2020)

#add data from 2021
m1_2021 <- read_csv("202101-divvy-tripdata.csv")
m2_2021 <- read_csv("202102-divvy-tripdata.csv")
m3_2021 <- read_csv("202103-divvy-tripdata.csv")
m4_2021 <- read_csv("202104-divvy-tripdata.csv")
m5_2021 <- read_csv("202105-divvy-tripdata.csv")
m6_2021 <- read_csv("202106-divvy-tripdata.csv")
m7_2021 <- read_csv("202107-divvy-tripdata.csv")
m8_2021 <- read_csv("202108-divvy-tripdata.csv")
m9_2021 <- read_csv("202109-divvy-tripdata.csv")
m10_2021 <- read_csv("202110-divvy-tripdata.csv")
m11_2021 <- read_csv("202111-divvy-tripdata.csv")
m12_2021 <- read_csv("202112-divvy-tripdata.csv")


vf_2021 <- bind_rows(m1_2021,m2_2021,m3_2021,m4_2021,m5_2021,m6_2021,m7_2021,
                     m8_2021,m9_2021,m10_2021,m11_2021,m12_2021)

# convert column data to match others
vf_2021 <- mutate(vf_2021, start_station_id = as.double(start_station_id),
                    end_station_id = as.double(end_station_id))

# add data from 2022
m1_2022 <- read_csv("202201-divvy-tripdata.csv")
m2_2022 <- read_csv("202202-divvy-tripdata.csv")
m3_2022 <- read_csv("202203-divvy-tripdata.csv")
m4_2022 <- read_csv("202204-divvy-tripdata.csv")
m5_2022 <- read_csv("202205-divvy-tripdata.csv")
m6_2022 <- read_csv("202206-divvy-tripdata.csv")
m7_2022 <- read_csv("202207-divvy-tripdata.csv")
m8_2022 <- read_csv("202208-divvy-tripdata.csv")
m9_2022 <- read_csv("202209-divvy-tripdata.csv")
m10_2022 <- read_csv("202210-divvy-tripdata.csv")
m11_2022 <- read_csv("202211-divvy-tripdata.csv")
m12_2022 <- read_csv("202212-divvy-tripdata.csv")

vf_2022 <- bind_rows(m1_2022,m2_2022,m3_2022,m4_2022,m5_2022,m6_2022,m7_2022,
                     m8_2022,m9_2022,m10_2022,m11_2022,m12_2022)

# convert column data to match others
vf_2022 <- mutate(vf_2022, start_station_id = as.double(start_station_id),
                  end_station_id = as.double(end_station_id))

#add data from 2023

m1_2023 <- read_csv("202301-divvy-tripdata.csv")
m2_2023 <- read_csv("202302-divvy-tripdata.csv")
m3_2023 <- read_csv("202303-divvy-tripdata.csv")
# m4_2023 doesn't aggregate properly due to formatting differences
# so must be added later
m5_2023 <- read_csv("202305-divvy-tripdata.csv")
m6_2023 <- read_csv("202306-divvy-tripdata.csv")
m7_2023 <- read_csv("202307-divvy-tripdata.csv")
m8_2023 <- read_csv("202308-divvy-tripdata.csv")
m9_2023 <- read_csv("202309-divvy-tripdata.csv")
m10_2023 <- read_csv("202310-divvy-tripdata.csv")
m11_2023 <- read_csv("202311-divvy-tripdata.csv")
m12_2023 <- read_csv("202312-divvy-tripdata.csv")

vf_2023 <- bind_rows(m1_2023,m2_2023,m3_2023,m5_2023,m6_2023,m7_2023,
                     m8_2023,m9_2023,m10_2023,m11_2023,m12_2023)

# convert column data to match others
vf_2023 <- mutate(vf_2023, start_station_id = as.double(start_station_id),
                  end_station_id = as.double(end_station_id))

#add data from 2024
m1_2024 <- read_csv("202401-divvy-tripdata.csv")
m2_2024 <- read_csv("202402-divvy-tripdata.csv")
m3_2024 <- read_csv("202403-divvy-tripdata.csv")
m4_2024 <- read_csv("202404-divvy-tripdata.csv")

vf_2024 <- bind_rows(m1_2024,m2_2024,m3_2024,m4_2024)

# convert column data to match others
vf_2024 <- mutate(vf_2024, start_station_id = as.double(start_station_id),
                  end_station_id = as.double(end_station_id))

#combine the tables
all_trips <- bind_rows(vf_2019,vf_2020,vf_2021,vf_2022,vf_2023,vf_2024,vf_2018,
                       vf_2017,vf_2016,vf_2015,vf_2014,vf_2013)

#add a ride_length column
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#make the ride_length numeric so that calculations can be performed on it
is.factor(all_trips$ride_length)
is.numeric(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#add m4_2023 so it can be cleaned and aggregated
m4_2023 <- read_csv("202304-divvy-tripdata.csv")
m4_2023 <- mutate(m4_2023, start_station_id = as.double(start_station_id),
                  end_station_id = as.double(end_station_id))
m4_2023$ride_length <- as.numeric(seconds(m4_2023$ride_length))
m4_2023$started_at = ymd_hm(m4_2023$started_at)
m4_2023$ended_at = ymd_hm(m4_2023$ended_at)

all_trips <- bind_rows(all_trips,m4_2023)

#remove the irrelevant columns
all_trips <- all_trips %>% 
  select(-c(start_lat,start_lng,end_lat,end_lng,birthyear,gender,
            "tripduration",
            "01 - Rental Details Duration In Seconds Uncapped",
            "Member Gender",
            "05 - Member Details Member Birthday Year",
            "03 - Rental Start Station Name",
            "birthday"))

# to make the table consistent, change some of the variable names
all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual"))

#add new columns for date,month,day,year of each ride
#this allows more data to be aggregated
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


#create a new df where "bad" data is removed
all_trips_v2 <- all_trips[!(all_trips$ride_length<0 | all_trips$ride_length>1440),]

#conduct descriptive analysis
mean(all_trips_v2$ride_length, na.rm=TRUE) #625 seconds
median(all_trips_v2$ride_length, na.rm=TRUE) #559 seconds
max(all_trips_v2$ride_length, na.rm=TRUE) #1440 second
min(all_trips_v2$ride_length, na.rm=TRUE) #0 second

#average ride time by each day for members vs casuals
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
            all_trips_v2$day_of_week, FUN = mean)

#fix the days of week being out of order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,
                                    levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

#re run to view it better
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
            all_trips_v2$day_of_week, FUN = mean)

#some months included a "dependent" member type, but this was discontinued soon after
all_trips_v2 <- all_trips_v2[all_trips_v2$member_casual != "Dependent",]

#analyze ridership data by type and weekday
all_trips_v2 %>%
  mutate(weekday = wday(started_at,label = TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarize(number_of_rides = n() #calculates the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)


#export summary file for further analysis
counts3 <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
                      all_trips_v2$year + all_trips_v2$month + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts3, file = 'avg_ride_length_by_year_3.csv')
