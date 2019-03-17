load("Expedia.Rdata")

library(ggplot2)
library(dplyr)

#counting missing values
missing_values <- sapply(all_data, function(x) sum(is.na(x)))
missing_values

#counting unique values
unique_values <- sapply(all_data, function(x) length(unique(x)))
unique_values

#stats
summary(all_data)
str(all_data)

#date_time
hist(as.Date(all_data$date_time),"weeks")
hist(as.Date(all_data$date_time),"months")
max(all_data$date_time) #2014-12-31
min(all_data$date_time) #2013-01-07
#two years' data from 2013-01-07 to 2014-12-31 
#concentrating between 2014-03 to 2014-12

#site_name
hist(all_data$site_name)
site_name=all_data %>%
  group_by(site_name) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(site_name))
site_name

ggplot(data=site_name,
       aes(y=count,x=reorder(site_name,count)))+
  xlab("site_name")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#posa_continent
posa_continent=all_data %>%
  group_by(posa_continent) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(posa_continent))
posa_continent

ggplot(data=posa_continent,
       aes(y=count,x=reorder(posa_continent,count)))+
  xlab("posa_continent")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#user_location_country
hist(all_data$user_location_country)
user_location_country=all_data %>%
  group_by(user_location_country) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(user_location_country))
user_location_country

ggplot(data=user_location_country[1:20,],
       aes(y=count,x=reorder(user_location_country,count)))+
  xlab("user_location_country")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#user_location_region
user_location_region=all_data %>%
  group_by(user_location_region) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(user_location_region))
user_location_country

ggplot(data=user_location_region[1:20,],
       aes(y=count,x=reorder(user_location_region,count)))+
  xlab("user_location_region")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#user_location_city
user_location_city=all_data %>%
  group_by(user_location_city) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(user_location_city))
user_location_city

ggplot(data=user_location_city[1:20,],
       aes(y=count,x=reorder(user_location_city,count)))+
  xlab("user_location_region")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#orig_destination_distance
hist(all_data$orig_destination_distance)

#user_id
#may delete this variable

#is_mobile
is_mobile=all_data %>%
  group_by(is_mobile) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(is_mobile))
is_mobile

ggplot(data=is_mobile,
       aes(y=count,x=reorder(is_mobile,count)))+
  xlab("is_mobile")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#is_package
is_package=all_data %>%
  group_by(is_package) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(is_package))
is_package

ggplot(data=is_package,
       aes(y=count,x=reorder(is_package,count)))+
  xlab("is_package")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#channel
channel=all_data %>%
  group_by(channel) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(channel))
channel

ggplot(data=channel,
       aes(y=count,x=reorder(channel,count)))+
  xlab("channel")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#srch_ci
hist(all_data$srch_ci,"weeks")
hist(all_data$srch_ci,"months")

srch_ci=all_data %>%
  group_by(srch_ci) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(srch_ci))
srch_ci

ggplot(data=srch_ci[1:20,],
       aes(y=count,x=reorder(srch_ci,count)))+
  xlab("srch_ci")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#srch_co
hist(all_data$srch_co,"weeks")
hist(all_data$srch_co,"months")

boxplot(as.numeric(all_data$srch_co-all_data$srch_ci),xlim=c(0,60))

#srch_adults_cnt
srch_adults_cnt=all_data %>%
  group_by(srch_adults_cnt) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(srch_adults_cnt))
srch_adults_cnt

ggplot(data=srch_adults_cnt,
       aes(y=count,x=reorder(srch_adults_cnt,count)))+
  xlab("srch_adults_cnt")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#srch_children_cnt
srch_children_cnt=all_data %>%
  group_by(srch_children_cnt) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(srch_children_cnt))
srch_children_cnt

ggplot(data=srch_children_cnt,
       aes(y=count,x=reorder(srch_children_cnt,count)))+
  xlab("srch_children_cnt")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#srch_rm_cnt
srch_rm_cnt=all_data %>%
  group_by(srch_rm_cnt) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(srch_rm_cnt))
srch_rm_cnt

ggplot(data=srch_rm_cnt,
       aes(y=count,x=reorder(srch_rm_cnt,count)))+
  xlab("srch_rm_cnt")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#srch_destination_id
srch_destination_id=all_data %>%
  group_by(srch_destination_id) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(srch_destination_id))
srch_destination_id

ggplot(data=srch_destination_id[1:20,],
       aes(y=count,x=reorder(srch_destination_id,count)))+
  xlab("srch_destination_id")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#srch_destination_type_id
srch_destination_type_id=all_data %>%
  group_by(srch_destination_type_id) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(srch_destination_type_id))
srch_destination_type_id

ggplot(data=srch_destination_type_id,
       aes(y=count,x=reorder(srch_destination_type_id,count)))+
  xlab("srch_destination_type_id")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#hotel_continent
hotel_continent=all_data %>%
  group_by(hotel_continent) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(hotel_continent))
hotel_continent

ggplot(data=hotel_continent,
       aes(y=count,x=reorder(hotel_continent,count)))+
  xlab("hotel_continent")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#hotel_country
hotel_country=all_data %>%
  group_by(hotel_country) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(hotel_country))
hotel_country

ggplot(data=hotel_country[1:20,],
       aes(y=count,x=reorder(hotel_country,count)))+
  xlab("hotel_country")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#hotel_market
hotel_market=all_data %>%
  group_by(hotel_market) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(hotel_market))
hotel_market

ggplot(data=hotel_market[1:20,],
       aes(y=count,x=reorder(hotel_market,count)))+
  xlab("hotel_market")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#is_booking
is_booking=all_data %>%
  group_by(is_booking) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(is_booking))
is_booking

ggplot(data=is_booking,
       aes(y=count,x=reorder(is_booking,count)))+
  xlab("is booking or not")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#cnt
boxplot(all_data$cnt)
hist(all_data$cnt)

cnt=all_data %>%
  group_by(cnt) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(cnt))
cnt

ggplot(data=cnt[1:20,],
       aes(y=count,x=reorder(cnt,count)))+
  xlab("cnt")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#hotel_cluster
hotel_cluster=all_data %>%
  group_by(hotel_cluster) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(hotel_cluster))
hotel_cluster

ggplot(data=hotel_cluster[1:20,],
       aes(y=count,x=reorder(hotel_cluster,count)))+
  xlab("hotel_cluster")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#group by country, cluster
country_hotel_cluster=all_data %>%
  group_by(hotel_country,hotel_cluster) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

country_hotel_cluster=as.data.frame(country_hotel_cluster)
country_hotel_cluster

#group by orig_destination_distance, cluster
distance_cluster=all_data %>%
  group_by(orig_destination_distance,hotel_cluster) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(orig_destination_distance))

distance_cluster=as.data.frame(distance_cluster)
distance_cluster

ggplot(distance_cluster, aes(x=orig_destination_distance,y=count,colour=hotel_cluster))+ geom_line()+coord_flip()+
  scale_colour_gradientn(colours=rainbow(4))
#the variable is somewhat useful?

#group by orig_destination_distance, srch_destination_type_id
distance_type=all_data %>%
  group_by(orig_destination_distance,srch_destination_type_id) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(orig_destination_distance))

distance_type=as.data.frame(distance_type)
distance_type

ggplot(distance_type, aes(x=orig_destination_distance,y=count,colour=srch_destination_type_id))+ geom_line()+coord_flip()+
  scale_colour_gradientn(colours=rainbow(4))
#the variable is not well separated by srch_destination_type_id

#group by orig_destination_distance, hotel_continent
distance_continent=all_data %>%
  group_by(orig_destination_distance,hotel_continent) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(orig_destination_distance))

distance_continent=as.data.frame(distance_continent)
distance_continent

ggplot(distance_continent, aes(x=orig_destination_distance,y=count,colour=hotel_continent))+ geom_line()+coord_flip()+
  scale_colour_gradientn(colours=rainbow(4))
#a little bit better separated but still not as good

#group by orig_destination_distance, user_location_country
distance_country=all_data %>%
  group_by(orig_destination_distance,user_location_country) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(orig_destination_distance))

distance_country=as.data.frame(distance_country)
distance_country

ggplot(distance_country, aes(x=orig_destination_distance,y=count,colour=user_location_country))+ geom_line()+coord_flip()+
  scale_colour_gradientn(colours=rainbow(4))
#worse than hotel_continent

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

library(lubridate)
all_data=all_data[!(is.na(all_data$srch_ci))|!(is.na(all_data$srch_co)),]
clean_data = all_data[, -c(2, 6, 8, 17)]
clean_data$date_time = ymd_hms(clean_data$date_time)
clean_data$month = month(clean_data$date_time)
library(dplyr)
clean_data$new_user_location_country = -1
clean_data[clean_data$user_location_country == 66,]$new_user_location_country = 1
clean_data[clean_data$user_location_country == 205,]$new_user_location_country = 2
clean_data[clean_data$user_location_country == 3,]$new_user_location_country = 3
clean_data[clean_data$user_location_country == 69,]$new_user_location_country = 4
clean_data = clean_data[, -3]
# user_location_country finished cleaning
#----------------------------------------------------------------------------------

temp = c(174,348,354,442,220,50,462,135,155,258,337,311,363,448,318,226,0,385,184,351)
clean_data$new_user_location_region = -1
j = 1
for (i in temp) {
  clean_data[clean_data$user_location_region == i,]$new_user_location_region = j
  j = j + 1
}
clean_data = clean_data[, -3]
# user_location_region finished cleaning
#----------------------------------------------------------------------------------

clean_data <- clean_data %>%
  group_by(hotel_country) %>%
  mutate(orig_destination_distance = replace(orig_destination_distance,
                                             is.na(orig_destination_distance), 
                                             mean(orig_destination_distance, na.rm = T)))
# user_original_destination_distance finished cleaning
#----------------------------------------------------------------------------------

clean_data$hotel_duration_day = clean_data$srch_co - clean_data$srch_ci
clean_data$srch_ahead_day = clean_data$srch_ci - as.Date(clean_data$date_time)
clean_data$check_in_month = month(clean_data$srch_ci)
clean_data = subset(clean_data, select = -c(srch_ci, srch_co))
# user_srch_ci user_srch_out finished cleaning
#----------------------------------------------------------------------------------

temp = c(50,8,198,105,70,204,77,182,106,144, 163,63,48,99,5,171,126,22,68,168)
clean_data$new_hotel_country = -1
j = 1
for (i in temp) {
  clean_data[clean_data$hotel_country == i,]$new_hotel_country = j
  j = j + 1
}
clean_data = subset(clean_data, select = -hotel_country)
# user_hotel_country finished cleaning
#----------------------------------------------------------------------------------

hotel_market=clean_data %>%
  group_by(hotel_market) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) 
medium_hotel_market=list(hotel_market[16:130,1])
extreme_high_hotel_market = list(hotel_market[1:2,1])
high_hotel_market = list(hotel_market[3:15,1])
low_hotel_market = list(hotel_market[131:nrow(hotel_market),1])

clean_data$new_hotel_market = -1
  # -1 represent low frequency of hotel market
for (i in extreme_high_hotel_market[[1]][[1]]) {
  clean_data[clean_data$hotel_market == i,]$new_hotel_market = 1
  # 1 represent extreme high frequency of hotel market
}
for (i in high_hotel_market[[1]][[1]]) {
  clean_data[clean_data$hotel_market == i,]$new_hotel_market = 2
  # 2 represent high frequency of hotel market
}
for (i in medium_hotel_market[[1]][[1]]) {
  clean_data[clean_data$hotel_market == i,]$new_hotel_market = 3
  # 3 represent medium frequency of hotel market
}
clean_data = subset(clean_data, select = -hotel_market)
# user_hotel_market finished cleaning
#----------------------------------------------------------------------------------

clean_data = subset(clean_data, select = -date_time)
