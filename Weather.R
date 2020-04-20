install.packages("RCurl")

library(tidyverse)
library(httr)
library(curl)
library(RCurl)
library(jsonlite)

api.key <- "65125913f8a8445c851190650201604"
  

url1 <- paste0(
  "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=", 
  api.key, "&q=Jersey%20City&format=json&date=2019-01-01&enddate=2019-01-31&includelocation=yes&tp=24"
)

url2 <- paste0(
  "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=", 
  api.key, "&q=Jersey%20City&format=json&date=2019-02-01&enddate=2019-02-28&includelocation=yes&tp=24"
)

url3 <- paste0(
  "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=", 
  api.key, "&q=Jersey%20City&format=json&date=2019-03-01&enddate=2019-03-31&includelocation=yes&tp=24"
)

url4 <- paste0(
  "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=", 
  api.key, "&q=Jersey%20City&format=json&date=2019-04-01&enddate=2019-04-30&includelocation=yes&tp=24"
)

url5 <- paste0(
  "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=", 
  api.key, "&q=Jersey%20City&format=json&date=2019-05-01&enddate=2019-05-31&includelocation=yes&tp=24"
)

url6 <- paste0(
  "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=", 
  api.key, "&q=Jersey%20City&format=json&date=2019-06-01&enddate=2019-06-30&includelocation=yes&tp=24"
)

url7 <- paste0(
  "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=", 
  api.key, "&q=Jersey%20City&format=json&date=2019-07-01&enddate=2019-07-31&includelocation=yes&tp=24"
)

url8 <- paste0(
  "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=", 
  api.key, "&q=Jersey%20City&format=json&date=2019-08-01&enddate=2019-08-31&includelocation=yes&tp=24"
)

url9 <- paste0(
  "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=", 
  api.key, "&q=Jersey%20City&format=json&date=2019-09-01&enddate=2019-09-30&includelocation=yes&tp=24"
)

url10 <- paste0(
  "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=", 
  api.key, "&q=Jersey%20City&format=json&date=2019-10-01&enddate=2019-10-31&includelocation=yes&tp=24"
)

url11 <- paste0(
  "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=", 
  api.key, "&q=Jersey%20City&format=json&date=2019-11-01&enddate=2019-11-30&includelocation=yes&tp=24"
)

url12 <- paste0(
  "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=", 
  api.key, "&q=Jersey%20City&format=json&date=2019-12-01&enddate=2019-12-31&includelocation=yes&tp=24"
)



month1 <- url1 %>% fromJSON() %>% as.data.frame()
month2 <- url2 %>% fromJSON() %>% as.data.frame()
month3 <- url3 %>% fromJSON() %>% as.data.frame()
month4 <- url4 %>% fromJSON() %>% as.data.frame()
month5 <- url5 %>% fromJSON() %>% as.data.frame()
month6 <- url6 %>% fromJSON() %>% as.data.frame()
month7 <- url7 %>% fromJSON() %>% as.data.frame()
month8 <- url8 %>% fromJSON() %>% as.data.frame()
month9 <- url9 %>% fromJSON() %>% as.data.frame()
month10 <- url10 %>% fromJSON() %>% as.data.frame()
month11 <- url11 %>% fromJSON() %>% as.data.frame()
month12 <- url12 %>% fromJSON() %>% as.data.frame()



final_combined_weather_data <- rbind(month1,month2,month3,month4,month5,month6,month7,month8,month9,month10,month11,month12)







url <- "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=65125913f8a8445c851190650201604&q=Newark&format=json&date=2019-01-01&enddate=2019-01-31&includelocation=yes&tp=24"

url2 <- "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=65125913f8a8445c851190650201604&q=Jersey%20City&format=json&date=2019-01-01&enddate=2019-01-31&includelocation=yes&tp=24"



