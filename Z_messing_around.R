library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

w2010_2018<-readRDS("rds/weather_2010_2018.rds")
w2010_2018<-w2010_2018 %>% 
  mutate(year=year(datetime)) 
w2017<-w2010_2018%>% 
  filter(year==2017) %>% 
  pivot_longer(names_to = "parameter", cols = c(2:10),values_to = "measurement")

a<-ggplot(w2017, aes(x=datetime, y=measurement))+
  geom_line()+
  facet_grid(parameter~., scales = "free_y")
a

na_count <-sapply(weather, function(y) sum(length(which(is.na(y)))))
na_count<-data.frame(na_count)
na<-filter(weather, is.na(plotdate)==TRUE)
