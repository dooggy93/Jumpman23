library(dplyr)
df <- analyze_me

str(df)

library(Hmisc)
describe(df)

#Changing timestamp
library(lubridate)
df$when_the_delivery_started <- ymd_hms(substr(df$when_the_delivery_started,1,19))
df$when_the_Jumpman_arrived_at_pickup <- ymd_hms(substr(df$when_the_Jumpman_arrived_at_pickup,1,19))
df$when_the_Jumpman_left_pickup <- ymd_hms(substr(df$when_the_Jumpman_left_pickup,1,19))
df$when_the_Jumpman_arrived_at_dropoff <- ymd_hms(substr(df$when_the_Jumpman_arrived_at_dropoff,1,19))

#Day of week and weekend flag
df$wday_delivery_started <- wday(df$when_the_delivery_started)
df$weekend_delivery_started <- ifelse(df$wday_delivery_started %in% c(1,7),1,0)
df$day_del_started <- (day(df$when_the_delivery_started))

#Time duration columns
df$delivery_time <- difftime(df$when_the_Jumpman_arrived_at_dropoff,
                             df$when_the_Jumpman_left_pickup,
                             units="hours")

df$loading_time <- difftime(df$when_the_Jumpman_left_pickup,
                            df$when_the_Jumpman_arrived_at_pickup,
                            units="hours")

df$jumpman_arrival_time <- difftime(df$when_the_Jumpman_arrived_at_pickup,
                                    df$when_the_delivery_started,
                                    units="hours")

#Delivery distance
library(geosphere)
df$delivery_distance <- 0
for(i in 1:nrow(df))
{
  df[i,'delivery_distance'] <- distm(c(df[i,"dropoff_lat"],df[i,"dropoff_lon"]),
                                     c(df[i,"pickup_lat"],df[i,"pickup_lon"]),
                                     fun=distHaversine)/1609.34
}

#Average speed of delivery
df$jumpman_avg_speed <- df$delivery_distance/as.numeric(df$delivery_time)

library(dplyr)
df_unique <- df %>% distinct(delivery_id, .keep_all = TRUE)

#MAPPING
#Dropoff locations by week/weekend
library(leaflet)
leaflet() %>% setView(-73.972887,40.732828,zoom=12) %>% addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(data=subset(df_unique,weekend_delivery_started==0),
                   lat=~dropoff_lat,lng=~dropoff_lon,weight=1,radius=3,opacity=1,color="Orange") %>%
  addCircleMarkers(data=subset(df_unique,weekend_delivery_started==1),
                   lat=~dropoff_lat,lng=~dropoff_lon,weight=1,radius=2,opacity=1,color="Blue") %>%
  addLegend("bottomright",colors =c("Blue", "Orange"),labels= c("Weekend","Weekday"),opacity = 1)

#Pickup location by week/weekend
leaflet() %>% setView(-73.972887,40.732828,zoom=12) %>% addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(data=subset(df_unique,weekend_delivery_started==0),
                   lat=~pickup_lat,lng=~pickup_lon,weight=1,radius=3,opacity=1,color="Orange") %>%
  addCircleMarkers(data=subset(df_unique,weekend_delivery_started==1),
                   lat=~pickup_lat,lng=~pickup_lon,weight=1,radius=2,opacity=1,color="Blue")%>%
  addLegend("bottomright",colors =c("Blue", "Orange"),labels= c("Weekend","Weekday"),opacity = 1)

#Unique number of customers
paste(length(unique(df_unique$customer_id))," Unique Customers")

library(ggplot2)
ggplot(data.frame(as.vector(table(df_unique$customer_id))))+
  geom_histogram(bins=30,aes(x=as.vector.table.df_unique.customer_id..))+
  ggtitle("Customer Order Frequency")+
  xlab("Orders per customer")+
  ylab("Number of Customers")

describe(as.vector(table(df_unique$customer_id)))

#Unique number of jumpmen
paste(length(unique(df_unique$jumpman_id)), "Unique Jumpmen")

ggplot(data.frame(as.vector(table(df_unique$jumpman_id))))+
  geom_histogram(bins=30,aes(x=as.vector.table.df_unique.jumpman_id..))+
  ggtitle("Jumpman Order Frequency")+
  xlab("Orders per Jumpman")+
  ylab("Number of Jumpman")

describe(as.vector(table(df_unique$jumpman_id)))

#Customer aqcuisition per day
cust_acq <- df_unique %>%
  group_by(customer_id) %>%
  summarise(first_day=min(day(when_the_delivery_started)))

ggplot(cust_acq,aes(x=first_day,y=1))+
  stat_summary(fun.y=sum,geom="line")+
  ggtitle("Number of New Customers Acquired per Day")+
  ylab("Number of Customers")+
  xlab("Days in October 2014")

#Range when deliveries started
paste("Dates range from ",
      min(df_unique$when_the_delivery_started),
      " to ",
      max(df_unique$when_the_delivery_started)
)

#Delivery trends by hour of day
ggplot(df_unique,aes(x=hour(when_the_delivery_started), 1,group=1)) +
  stat_summary(fun.y = sum,geom = "bar")+
  ggtitle("Deliveries by Hour of the Day")+
  ylab("Number of Deliveries")+xlab("Hour of the Day")

#Delivery trends by weekday/weekend
df_wday_hour <- df_unique %>%
  group_by(weekend_delivery_started,hour(when_the_delivery_started)) %>%
  summarise(count=n())

df_wday_hour$count <- ifelse(df_wday_hour$weekend_delivery_started == 1,df_wday_hour$count/8,df_wday_hour$count/22)

colnames(df_wday_hour) <- c("weekend","hour","count")

df_wday_hour$weekend <- ifelse(df_wday_hour$weekend == 1, "Yes","No")

ggplot(df_wday_hour,aes(x=hour,y=count,group=weekend,color=weekend))+
  geom_line(size=2)+
  ggtitle("Delivery Trends by Hour of the Day Weekend/Weekday")+
  xlab("Hour of the day")+ylab("Number of Deliveries")

#Delivery trends by day of week
ggplot(df_unique,aes(x=wday(when_the_delivery_started,label=T), 1,group=1)) +
  stat_summary(fun.y = sum,geom = "line")+
  ggtitle("Deliveries by Days of the Week")+
  ylab("Number of Deliveries")+xlab("Days of the Week")

#Vehicle usage week
ggplot(df_unique,aes(x=vehicle_type, 1,group=1)) +
  stat_summary(fun.y = sum,geom = "bar")+
  ggtitle("Number of Deliveries by Vehicle Type")+
  xlab("Vehicle Type")+ylab("Number of Deliveries")

ggplot(df_unique,aes(x=wday(when_the_delivery_started,label=T), 1,group=vehicle_type,color=vehicle_type)) +
  stat_summary(fun.y = sum,geom = "line",size=1)+
  ggtitle("Vehicle Usage by Days of the Week")+
  xlab("Days of the Week")+ylab("Number of Deliveries")

ggplot(df_unique,aes(x=wday(when_the_delivery_started,label=T),1,fill=vehicle_type))+
  geom_bar(position="fill",stat="identity")+
  ggtitle("Proportion of Deliveries by Vehicle Type by Days of Week")+
  xlab("Days of the Week")+ylab("Proportion of Deliveries")

#Vehicle usage month
ggplot(df_unique,aes(x=as.factor(day_del_started), 1,group=vehicle_type,color=vehicle_type)) +
  stat_summary(fun.y = sum,geom = "line",size=1)+
  ggtitle("Vehicle Usage by Days of the Month")+
  xlab("Days of October")+ylab("Number of Deliveries")

#Delivery time
ggplot(df_unique,aes(x=vehicle_type,y=delivery_time))+
  geom_boxplot()+
  ggtitle("Delivery Time by Vehicle Types")+
  xlab("Vehicle Types")+ylab("Delivery Time (Hours)")

#Delivery time by pickup place
ggplot(dftop,aes(x=pickup_place,y=delivery_time))+
  geom_boxplot()+
  ggtitle("Delivery Time by Pickup Place")+
  xlab("Pickup Place")+ylab("Delivery Time (Hours)")

dftop = subset(df_unique, pickup_place %in% pickup_place [1:4])
dftop$pickup_place = droplevels(dftop$pickup_place)

jptop = subset(df_unique, customer_id %in% customer_id [1:3])
jptop$customer_id = droplevels(dftop$customer_id)


#Delivery time by day of week
ggplot(df_unique,aes(x=wday(when_the_delivery_started,label=T),y=delivery_time))+
  geom_boxplot()+
  ggtitle("Loading Time variation across Days of the Week")+
  xlab("Days of the Week")+ylab("Delivery Time (Hours)")

#Preptime by vehicle
ggplot(df_unique,aes(x=vehicle_type,y=loading_time))+
  geom_boxplot()+
  ggtitle("Prep Time Variation by Vehicle Types")+
  xlab("Vehicle Types")+ylab("Prep Time (Hours)")

#Preptime by pickup place
ggplot(dftop,aes(x=pickup_place,y=loading_time))+
  geom_boxplot()+
  ggtitle("Prep Time by Pickup Place")+
  xlab("Pickup Place")+ylab("Prep Time (Hours)")

#Delivery distance by vehicle
ggplot(df_unique,aes(x=vehicle_type,y=delivery_distance))+
  geom_boxplot()+
  ggtitle("Delivery Distances by Vehicle Type")+
  xlab("Vehicle Type")+
  ylab("Distance (Miles)")

#Average delivery speed
ggplot(df_unique,aes(x=vehicle_type,y=jumpman_avg_speed))+
  geom_boxplot()+
  ggtitle("Avg. Delivery Speed by Vehicle Type")+
  xlab("Vehicle Type")+
  ylab("Speed (MPH)")+ylim(0,20)

#More popular places
paste(length(unique(df_unique$pickup_place))," unique pickup locations")

df_pickup_place <- df %>%
  group_by(pickup_place) %>%
  summarise(count=n())

library(ggplot2)
qplot(df_pickup_place$count, geom="histogram",bins=200)+
  ggtitle("Frequency of Deliveries from pickup locations")+
  xlab("Delivery Frequency")+ylab("Number of Pickup Locations")

describe(as.factor(df_pickup_place$count))
