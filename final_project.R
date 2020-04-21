install.packages("hrbrthemes")
library(tidyverse)
library(magrittr)
library(ggplot2)
library(stringr)
library(lubridate)
library(hrbrthemes)
library(ggplot2)

data <- read.csv("C:/Users/Amit/Desktop/MS in Data Science/Sem4/Data  Wrangling/Project/NJ Data/Combined_data.csv")

df1 <- data

#sample of data
head(df1,30)


#changing column names
colnames(df1) <- c("trip_duration","start_time","stop_time","start_stn_id","start_stn_name","start_stn_lat",
                   "start_stn_long","end_stn_id","end_stn_name","end_stn_lat","end_stn_lang","bike_id",
                   "user_type","birth_year","gender")

df1 <- df1 %>% select("trip_duration","start_time","start_stn_id","start_stn_name","start_stn_lat",
                      "start_stn_long","end_stn_id","end_stn_name","end_stn_lat","end_stn_lang",
                      "bike_id","user_type","birth_year","gender")

df1$start_stn_name = as.character(df1$start_stn_name)
df1$end_stn_name = as.character(df1$end_stn_name)

df1 %>% group_by(user_type) %>% summarise(n = n())

df1 %>% group_by(user_type) %>% summarise(n = n()) %>% ggplot(aes(x=user_type, y=n)) + 
  geom_bar(stat = "identity")

##Since start time is in format 2019-01-01 03:09:09.7110 we will seperate start_time into start_date and start_time
df1 <- df1 %>% separate(start_time,c("start_date","start_time"),sep = " ")

class(df1$start_date)
df1$start_date  <- ymd(df1$start_date)
#df1$start_date <- as.Date(df1$start_date,"%Y-%m-%d")

?as.Date

############# Age-Wise distribution of data

Age_df <- df1
typeof(Age_df$birth_year)
Age_df <- Age_df %>% mutate(Age = 2020 - Age_df$birth_year)

Age_plot <- ggplot(Age_df, aes(x=Age)) + 
  geom_histogram(color = "black",fill = "steelblue")+
  scale_x_continuous(breaks=seq(0,200,10)) + ggtitle("Distribution of Trip Times") + 
  xlab("Age") +
  ylab("Number of Trips")

############

######### Age-group wise Pie chart

Age_df_cat <- Age_df %>% mutate(Age_group =  
  case_when(
  (Age >= 17 & Age < 25) ~ 1,
  (Age >= 25 & Age < 35) ~ 2,
  (Age >= 35 & Age < 45) ~ 3,
  (Age >= 45 & Age < 55) ~ 4,
  TRUE ~ 5
))

class(Age_df_cat$Age_group)
Age_df_cat$Age_group <- as.factor(Age_df_cat$Age_group)

#total_count <- nrow(Age_df_cat)
#group_count <- Age_df_cat %>% group_by(Age_group) %>% summarise(n = n())
#group_count <- group_count %>% mutate(Percentage = 100*n/total_count)







total_count <- Age_df_cat %>% filter(gender == 1 | gender == 2) %>% nrow()

group_data <-Age_df_cat %>% filter(gender == 1 | gender == 2) %>% group_by(gender,Age_group) %>% summarise(n = n())

group_data <- group_data %>% mutate(percent = n/total_count*100)

group_data$gender[group_data$gender == 1] <- "Male"
group_data$gender[group_data$gender == 2] <- "Female"

Age_df_cat$gender <- as.factor(Age_df_cat$gender)

basic <- ggplot(group_data, aes(fill=gender, y=percent, x=Age_group)) + 
  geom_bar(position="stack", stat="identity") + labs(gender = "Gender")


basic + scale_x_discrete(labels=c("1" = "17-24", "2" = "25-34", "3" = "35-44", "4" = "45-54", "5" = "55 and up"))
########

##########

plot2 <- df1 %>% group_by(start_date) %>% summarise(total_trips = n()) %>% ggplot(aes(x = start_date,y = total_trips)) + 
    geom_line(color="darkorange") + xlab("") + geom_smooth(color = "steelblue") + xlab("TimeLine")+ylab("Total Trips") 

plot2  

#########


######Top 5 stations with with most starts
top_starts <- df1 %>% group_by(start_stn_name) %>% summarise(total = n()) %>% top_n(5,total)

top_starts <- top_starts[order(-top_starts$total),]

plot1 <- ggplot(data = top_starts,aes(x = start_stn_name,y=total)) + 
  geom_bar(stat = "identity",fill="steelblue") + 
  geom_text(aes(label=total), vjust=1.6, color="white", size=3.5)+
  ggtitle("Stations with top 5 starts") + xlab("Station names") + ylab("Total")+
  theme_minimal() 

plot1

#######

#Most Popular trips

popular_trips <- df1 %>% group_by(start_stn_name,end_stn_name) %>% summarise(total = n())

popular_trips <- popular_trips[order(-popular_trips$total),]

popular_trips$trips <- paste(popular_trips$start_stn_name,popular_trips$end_stn_name,sep = " to ")

plot4 <- popular_trips[1:10,] %>% ggplot(aes(x = trips, y = total)) + geom_bar(stat = "identity") +
  ggtitle("Most Popular Trips") + xlab("Trips") + ylab("Total") +  
  theme(plot.title = element_text(color="steelblue", size=14, face="bold.italic"),
        axis.title.x = element_text(color="#993333", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(face="bold", size=9))+
            coord_flip()  

plot4 + scale_x_discrete(limits=c("McGinley Square to Sip Ave","Dixon Mills to Grove St PATH","Monmouth and 6th to Grove St PATH",
                                  "Brunswick St to Grove St PATH","Grove St PATH to Marin Light Rail",
                                  "Jersey & 6th St to Grove St PATH","Marin Light Rail to Grove St PATH",
                                  "Brunswick & 6th to Grove St PATH","Grove St PATH to Hamilton Park",
                                  "Hamilton Park to Grove St PATH"))
##########

## Average Trips by Day of Week

df1$day_of_week <- weekdays(df1$start_date)

temp <- df1 %>% group_by(day_of_week) %>% summarise(count = n())

avg_day_of_week <- df1 %>% group_by(start_date,day_of_week) %>% 
  summarise(n = n()) %>% group_by(day_of_week) %>% summarise(days_count = n())

avg_day_of_week_final <- inner_join(temp,avg_day_of_week) %>% mutate(avg_count = count/days_count)

avg_day_of_week_final$day_of_week <- factor(avg_day_of_week_final$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                         "Friday", "Saturday", "Sunday"))
avg_day_of_week_final[order(avg_day_of_week_final$day_of_week),]
#avg_day_of_week_final$day_of_week <- as.character(avg_day_of_week_final$day_of_week)

avg_day_of_week_final %>% ggplot(aes(x = day_of_week,y = avg_count,group = 1)) + 
    geom_line(color = "steelblue",size=0.71) + 
    geom_point(color = "red") + ylim(500,1500) +
    ggtitle("Average Trips by Day of Week") + xlab("Day of week") + ylab("Avg. trips") +
    theme_grey()

##########


########Average Trips by time of a day
head(df1,30)
  #we will retrieve hour element from time

df2 <- df1 %>% separate(start_time,c("start_hr"), sep = ":")
df2$start_hr <- as.numeric(df2$start_hr)
typeof(df2$day_of_week)

weekdays <- df2%>% filter(day_of_week == "Monday" | day_of_week == "Tuesday" | day_of_week == "Wednesday" | day_of_week == "Thursday" | day_of_week == "Friday")
weekdays <- weekdays %>% group_by(start_hr) %>% summarise(hourly_count = n())
weekdays <- weekdays %>% mutate(avg_count = hourly_count/261)
weekdays$var <- "work days"
  
weekends <- df2%>% filter(day_of_week == "Saturday" | day_of_week == "Sunday")
weekends <- weekends %>% group_by(start_hr) %>% summarise(hourly_count = n())
weekends <- weekends %>% mutate(avg_count = hourly_count/104)
weekends$var <- "weekends" 

combined <- rbind(weekdays,weekends)

plot3 <- combined %>% ggplot(aes(x = start_hr,y = avg_count,col = var)) +
            geom_line(size =0.72) + scale_x_continuous(breaks=seq(0,23,4),labels=c("00:00", "04:00", "08:00","12:00","16:00","20:00")) + 
            ggtitle("Average Trips by time of day 2019")+
            xlab("Time") + ylab("Average Trips") + theme_grey() +
            scale_color_brewer(palette="Dark2") + theme(legend.position="bottom")
    

plot3
  
##########

######## Distribution of tripduration

trip_time_min <- df1 %>% select(trip_duration) %>% mutate(trip_in_mins = trip_duration/60)

(trip_time_min[trip_time_min$trip_in_mins < 40,]) %>% ggplot(aes(x = trip_in_mins)) + geom_histogram(binwidth = 1,color = "black",fill = "steelblue") +
  scale_x_continuous(breaks=seq(0,40,1)) + ggtitle("Distribution of Trip Times") + xlab("Trip Duration(minutes)") +
  ylab("Number of Trips")

nrow(trip_time_min[trip_time_min$trip_in_mins < 40,])

##only 10000 trips and more than 60 mins so we are not considering it for plotting the graph
404947 - 394600

######

######### Effect of weather on temprature 
weather <- final_combined_weather_data

weather <- weather %>% select(data.weather.date,data.weather.avgtempF)

weather$data.weather.date  <- ymd(weather$data.weather.date)



df1_with_weather <- inner_join(df1,weather, by= c("start_date"="data.weather.date"))

df1_with_weather$data.weather.avgtempF <- as.numeric(df1_with_weather$data.weather.avgtempF)

df1_with_weather %>% ggplot(aes(x = data.weather.avgtempF)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Effect of weather on bike trips") + xlab("Avg. Temprature") + 
  theme_ipsum()

######

########Another effect of weather on temprature
df3 <- df1_with_weather %>% group_by(start_date) %>% summarise(n = n())
df3 <- inner_join(df3,weather,by= c("start_date"="data.weather.date"))
#############


###########

cor(df3$n,as.numeric(df3$data.weather.avgtempF))

##########



