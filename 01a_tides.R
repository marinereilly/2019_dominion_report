#####load packages#####
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)

#####Load tidal data#####
# filenames<-list.files(path = "H://0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/ysi_noaa_wq_weather_tides/data/tides/2019",
#                       pattern = "*.csv", 
#                       full.names = F)
# 
# final_output <- purrr::map_df(filenames, function(x) {
#   data <- read_csv(paste0("H://0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/ysi_noaa_wq_weather_tides/data/tides/2019/",x),
#                    na = "-")
#   cbind(file_id = x, data)
# })
# 
# tides<-final_output %>% 
#   mutate(date=ymd(Date),
#          datetime=ymd_hms(paste0(Date," ",`Time (GMT)`))) %>% 
#   select(-Date,-`Time (GMT)`) %>% 
#   arrange(datetime)
# tides <- tides[rowSums(is.na(tides[,3:4]))!=2,]
# tides <- tides %>% 
#   mutate(wrong=`Predicted (m)`-`Verified (m)`)

tides<-readRDS("tides.RDS")

tides<-tides %>% 
  rename(datetime=`time (GMT)`,wl=`verified water level at Solomons Island, MD (meters rel. to NAVD)`) %>% 
  mutate(tiki=case_when(
    wl>=0.54   ~ 1,
    TRUE       ~ 0
    ))

a<-tides %>% 
  ggplot(aes(x=datetime, y=wl))+
  geom_line(color="steelblue3")+theme_classic()+
  scale_y_continuous(limits = c(-1.25,1.25), breaks = seq(-1.25,1.25,0.25))+
  geom_hline(aes(yintercept=0))+
  geom_hline(aes(yintercept=0.54),linetype="dashed", color="navy", size=1)+
  theme(axis.line.x = element_blank())+
  ylab("Verified Water Level (m NAVD 88)")+
  xlab("Date")+
  annotate("text",
           x=as.POSIXct("2010-03-05"), y=1,
           label = "Flooding at Tiki Bar",
           color="navy",
           fontface=2)+
  ggtitle("Water Levels from Solomons Island Tidal Station")
a

a<-tides %>% 
  date_cols() %>% 
  group_by(year,doy) %>% 
  summarise(fdays=sum(tiki)) %>%
  filter(fdays!=0) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  summarise(floods=length(fdays)) %>% 
  mutate(year=factor(year)) %>% 
  ggplot()+
  geom_col(aes(x=year, y=floods, fill=year), color="black")+
  scale_alpha_manual(values=c("0"=0, "1"=1))+theme_classic()+
  scale_fill_manual(values=ypal2019)+
  ggtitle("Days of Flooding at Tiki Bar on Solomons Island")+
  ylab("Days Flooded (water level >0.54m NAVD88)")+
  geom_text(aes(x=year,y=floods,label= floods),
                position=position_dodge(.9),
                vjust=1.75,
                size=4,fontface=2)
a
# a<-tides %>% 
#   ggplot(aes(x=datetime, y=wrong))+
#   geom_point()+theme_classic()+scale_color_manual()
# a
