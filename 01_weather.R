#####load packages#####
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(viridis)

#####load weather data#####
w2010_2018<-readRDS("rds/weather_2010_2018.rds")

w2019<-read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/ysi_noaa_wq_weather_tides/data/2019_weather.csv", header=FALSE, sep=",", skip = 3, stringsAsFactors = FALSE)
w_head<-c("datetime","delete","delete2","air_temp","bp","hail","rainfall","rel_hum",
          "wind_dir","wind_spd","wind_max","wind_min", "delete3", "delete4")
colnames(w2019)<-w_head
w2019<-w2019 %>% 
  select(-starts_with("delete")) %>% 
  mutate(datetime=ymd_hms(datetime))
w2019 <- w2019[rowSums(is.na(w2019[,5:9]))!=5,] #remove rows with NA in col 5,6,7,8 and 9
weather<-bind_rows(w2010_2018,w2019)
saveRDS(weather,"rds/weather_2010_2019.rds")

#####Rainfall#####
weather<-readRDS("rds/weather_2010_2019.rds")
weather<-weather[rowSums(is.na(weather)) != ncol(weather),]
weather<-weather[rowSums(is.na(weather[,2:4])) != 3,]
weather<-weather %>% 
  mutate(year=year(datetime),
         doy=yday(datetime),
         month=month(datetime),
         day=day(datetime))%>% 
  mutate(plotdate=ymd(paste0("1996-",month,"-",day)))#need a leap year as fake year

all_rain<-weather %>% 
  select(datetime,rainfall,11:15)

day_rain<-all_rain %>% 
  group_by(year,month,day) %>% 
  summarise(daily_rain=sum(rainfall)) %>%
  ungroup() %>% 
  mutate(date=ymd(paste0(year,"-",month,"-",day)),
         plotdate=ymd(paste0("1996-",month,"-",day)))

month_rain<-all_rain %>% 
  group_by(year, month) %>% 
  summarise(monthly_rain=sum(rainfall)) %>% 
  ungroup() %>% 
  mutate(date=ymd(paste0(year,"-",month,"-15")),
         plotdate=ymd(paste0("1996-",month,"-15")))
wet_days<-day_rain %>% 
  group_by(year) %>% 
  summarise(wet_days=sum(daily_rain > 0))
year_rain<-all_rain %>% 
  group_by(year) %>% 
  summarise(rainfall=sum(rainfall))

water_year<-all_rain %>% 
  mutate(wy=case_when(
    month>=10     ~ year+1,
    month<10      ~ year),
    date=date(datetime)) %>% 
  group_by(wy) %>% 
  arrange(date) %>% 
  mutate(wy_cum_rainfall = cumsum(rainfall)) %>% 
  ungroup() 

monthly_rain_cumulative<-month_rain %>%
  arrange(date) %>% 
  group_by(year) %>% 
  mutate(cumulative_rf=cumsum(monthly_rain))

#####rainfall plots#####
a<-month_rain %>% 
  filter(year=="2019") %>% 
  ggplot(aes(x=date, y=monthly_rain))+
  geom_col(color="black", fill="steelblue")+theme_classic()+
  ylab("monthly rainfall (in)")+ggtitle("2019 Monthly Rainfall")+
  geom_text(aes(label= sprintf("%0.2f",
                               round(monthly_rain, digits = 2))),
            position=position_dodge(.9),
            vjust=1.5,
            size=3.25)
a

a<-month_rain %>% 
  filter(is.na(year)==FALSE) %>% 
  mutate(year=factor(year)) %>% 
  ggplot(aes(x=plotdate, 
             y=monthly_rain,
             color=year))+
  geom_line(size=1.5)+
  scale_color_manual(values = ypal2019)+
  theme_classic()+
  scale_x_date(date_labels = "%b", breaks = "months")+
  ggtitle("Monthly Rainfall")+xlab("Date")+
  ylab("Monthly Rainfall (in)")
a

a<-month_rain %>% 
  ggplot()+
  geom_line(aes(x=date, y=monthly_rain), size=1.5)+
  theme_classic()+scale_x_date(breaks = "years", date_labels = "%Y")
a

a<-full_join(wet_days,year_rain) %>% 
  mutate(year=factor(year)) %>% 
  ggplot(aes(x=wet_days,y=rainfall, color=year))+
  geom_smooth(method=lm, linetype="dashed", color="black", se=FALSE )+
  geom_point(size=2)+theme_classic()+scale_color_manual(values=ypal2019)
  
a

a<-water_year %>% 
  mutate(wy_date=case_when(month>=10     ~ paste0("1995-",month,"-",day),
                           month<10      ~ paste0("1996-",month,"-",day)
  )) %>% 
  mutate(wy_date=ymd(wy_date)) %>% 
  mutate(wy=factor(wy)) %>% 
  group_by(date) %>% 
  filter(datetime==max(datetime)) %>%
  filter(wy!=2010) %>% 
  ggplot(aes(x=wy_date, color=wy, y=wy_cum_rainfall, size=wy))+
  geom_step()+
  scale_color_manual(values = year_color_highlight)+
  scale_size_manual(values=year_size_highlight)+theme_classic()+
  scale_x_date(breaks = c(as.Date("1995-10-01"),as.Date("1996-11-01"),
                          as.Date("1995-12-01"),as.Date("1996-01-01"),
                          as.Date("1996-02-01"),as.Date("1996-03-01"),
                          as.Date("1996-04-01"),as.Date("1996-05-01"),
                          as.Date("1996-06-01"),as.Date("1996-07-01"),
                          as.Date("1996-08-01"),as.Date("1996-09-01"),
                          as.Date("1996-10-01")),
               labels = c("Oct","", "Dec","","Feb","","Apr","","Jun","","Aug","","Oct"))+
  xlab("Date")+ylab("Cumulative Rainfall (in)")+
  labs(color = "Water Year", 
       size = "Water Year")+
  ggtitle("Cumulative Rainfall for the Water Year")
a  

a<-monthly_rain_cumulative %>%
  ungroup() %>% 
  mutate(year=factor(year)) %>% 
  filter(year!="2010") %>% 
  ggplot(aes(x=plotdate,y=cumulative_rf, color=year,size=year))+
  geom_step()+scale_color_manual(values=ypal2019)+theme_classic()+
  ggtitle("Cumulative Monthly Rainfall")+ylab("Cumulative Rainfall (in)")+
  xlab("Date")+scale_x_date(date_labels = "%b")+
  scale_size_manual(values=year_size_highlight)+labs(color="Year",size="Year")
a

#####Temperature Data#####
weather<-readRDS("rds/weather_2010_2019.rds")
weather<-weather[rowSums(is.na(weather)) != ncol(weather),]
weather<-weather[rowSums(is.na(weather[,2:4])) != 3,]
weather<-weather %>% 
  mutate(year=year(datetime),
         doy=yday(datetime),
         month=month(datetime),
         day=day(datetime))%>% 
  mutate(plotdate=ymd(paste0("1996-",month,"-",day)))#need a leap year as fake year

air_temperature<-weather %>% 
  select(1:2,11:15)
daily_air<-air_temperature %>% 
  group_by(date(datetime)) %>% 
  summarize(daily_mean=mean(air_temp, na.rm=T)) %>% 
  mutate(datetime=ymd_h(paste0(`date(datetime)`," 12"))) %>% 
  mutate(year=year(datetime),
         doy=yday(datetime),
         month=month(datetime),
         day=day(datetime))%>% 
  mutate(plotdate=ymd(paste0("1996-",month,"-",day)))

residuals<-air_temperature %>% 
  filter(year!=2018) %>% 
  group_by(plotdate) %>% 
  summarize(mean_date_temp=mean(air_temp, na.rm = TRUE)) %>% 
  left_join(daily_air,.) %>% 
  filter(year!=2018) %>%
  mutate(residual=daily_mean-mean_date_temp)
residuals$month_lab<-factor(month.abb[residuals$month],levels = month.abb)

#####Temperature Plots#####
a<-ggplot(residuals)+
  geom_point(aes(x=datetime, y=residual, color=factor(year)))+geom_hline(aes(yintercept=0),color="black")+
  geom_smooth(aes(x=datetime, y=residual),method = lm)+scale_x_datetime(breaks = "years", date_labels = "%Y")
a

a<-residuals %>% 
 season(.) %>%
  mutate(season2=case_when(
    month==1     ~ "Winter",
    month==2     ~ "Winter",
    month==4     ~ "Spring",
    month==6     ~ "Spring",
    month==5     ~ "Spring",
    month==9     ~ "Summer",
    month==7     ~ "Summer",
    month==8     ~ "Summer",
    month==12     ~ "Autumn",
    month==10    ~ "Autumn",
    month==11    ~ "Autumn",
    month==3    ~ "Winter"
  )) %>%
  ggplot()+
  geom_point(aes(x=datetime, y=residual, color=factor(year)))+
  geom_hline(aes(yintercept=0),color="black")+
  geom_smooth(aes(x=datetime, y=residual),method = lm, se=F,
              color="black", linetype="dashed")+
  scale_x_datetime(breaks = "4 years", minor_breaks="years",
                   date_labels = "%Y")+
  scale_color_manual(values=ypal2019)+
  facet_grid(.~month_lab)+
  theme_bw()+xlab("Year")+
  ylab("Mean Daily Temperature - Mean Temperature by Day of Year (C)")+
  ggtitle("Air Temperature Anomalies by Month")+
  labs(color="Year")
a

a<-residuals %>% 
  season(.) %>% 
  mutate(season2=case_when(
    month==1     ~ "Winter",
    month==2     ~ "Winter",
    month==4     ~ "Spring",
    month==6     ~ "Spring",
    month==5     ~ "Spring",
    month==9     ~ "Summer",
    month==7     ~ "Summer",
    month==8     ~ "Summer",
    month==12     ~ "Autumn",
    month==10    ~ "Autumn",
    month==11    ~ "Autumn",
    month==3    ~ "Winter"
  )) %>% 
  mutate(month_lab=month.abb[month]) %>% 
  ggplot()+
  geom_point(aes(x=datetime, y=daily_mean, color=factor(year)))+
  #geom_hline(aes(yintercept=0),color="black")+
  #geom_smooth(aes(x=datetime, y=residual),method = lm, se=F,
   #           color="black", linetype="dashed")+
  scale_x_datetime(breaks = "3 years", date_labels = "%Y")+
  scale_color_manual(values=ypal2019)+
  facet_grid(.~month)+
  theme_bw()
a
#####Wind Data#####
weather<-readRDS("rds/weather_2010_2019.rds")
wind<-weather %>% 
  date_cols() %>% 
  season() %>% 
  select(datetime, 7:16) %>% 
  mutate(hour=hour(datetime))
wind<-wind %>% 
  mutate(group_dir=case_when(
    wind_dir<=22                   ~ 0,
    wind_dir>22 & wind_dir<= 68    ~ 45,
    wind_dir>68 & wind_dir<= 112   ~ 90,
    wind_dir>112 & wind_dir<= 158  ~ 135,
    wind_dir>158 & wind_dir<= 202  ~ 180,
    wind_dir>202 & wind_dir<= 248  ~ 225,
    wind_dir>248 & wind_dir<= 292  ~ 270,
    wind_dir>292 & wind_dir<= 338  ~ 315,
    wind_dir>338                   ~ 0
  ))


wind_freq<-wind %>% 
  group_by(year) %>% 
  count(., group_dir)
wind_zero<-wind_freq %>% 
  filter(group_dir==0)
wind_zero$group_dir<-360
wind_freq<-wind_freq %>% 
  bind_rows(., wind_zero)

wind_percent<-wind_freq %>% 
  group_by(year) %>% 
  summarize(all_n=sum(n)) %>% 
  left_join(wind_freq,.) %>% 
  mutate(percent=n/all_n*100)

high_wind<-wind %>% 
  filter(wind_spd>=18)
high_wind_freq<-high_wind %>% 
  group_by(year) %>% 
  count(., group_dir)
highwind_zero<-high_wind_freq %>% 
  filter(group_dir==0)
highwind_zero$group_dir=360
high_wind_freq<-high_wind_freq %>% 
  bind_rows(., highwind_zero)

high_percent<-high_wind_freq %>% 
  group_by(year) %>% 
  summarize(all_n=sum(n)) %>% 
  left_join(high_wind_freq, .) %>% 
  mutate(percent=n/all_n*100)

beaufort<-wind %>% 
  mutate(beaufort=case_when(
    wind_spd <= 1                      ~ 0,
    wind_spd <= 4 & wind_spd > 1       ~ 1,
    wind_spd <= 7 & wind_spd > 4       ~ 2,
    wind_spd <= 12 & wind_spd > 7      ~ 3,
    wind_spd <= 18 & wind_spd > 12     ~ 4,
    wind_spd <= 23 & wind_spd > 18     ~ 5,
    wind_spd <= 30 & wind_spd > 23     ~ 6,
    wind_spd <= 38 & wind_spd > 30     ~ 7,
    wind_spd <= 46 & wind_spd > 38     ~ 8,
    wind_spd <= 55 & wind_spd > 46     ~ 9
  ))
beaufort_freq<-beaufort %>% 
  group_by(year, beaufort) %>% 
  count(., group_dir)
b_zero<-beaufort_freq %>% 
  filter(group_dir==0)
b_zero$group_dir<-360
b_freq<-beaufort_freq %>% 
  full_join(.,b_zero)

b_percent<-b_freq %>% 
  group_by(year) %>% 
  summarize(all_n=sum(n)) %>% 
  left_join(b_freq,.) %>% 
  mutate(percent=n/all_n*100)

rm(b_zero,highwind_zero,wind_zero, beaufort_freq)

s_freq<-wind %>% 
  group_by(year,season) %>% 
  count(., group_dir) 
s_freq2<-s_freq %>% 
  filter(group_dir==0) %>% 
  mutate(group_dir=360) %>% 
  rbind(s_freq,.) 
s_percent<-s_freq2%>% 
  group_by(year,season) %>% 
  summarize(all_n=sum(n)) %>% 
  left_join(s_freq2,.) %>% 
  mutate(percent=n/all_n*100) %>% 
  ungroup() %>% 
  mutate(year=factor(year),
         season=factor(season, levels = c("Winter", "Spring","Summer","Autumn"))) 
  

#####Wind Plots####
b_percent<-b_percent %>% 
  ungroup()

b_plot<-b_percent %>%
  mutate(beaufort=factor(beaufort, 
                         levels = c("8", "7", "6", "5", "4", "3", "2", "1", "0"))) %>% 
  filter(year=="2019") %>% 
  mutate(year=factor(year)) %>% 
  filter(!group_dir==360) %>% 
  ggplot()+
  geom_bar(aes(x=group_dir, y=percent, fill=beaufort), color="black", stat = "identity")+
  theme_minimal()+
  scale_fill_viridis(discrete=TRUE, option="magma", direction = -1)+
  ggtitle("2019 Wind Speed on the Beaufort Scale")+
  guides(fill=guide_legend(title="Beaufort Number"))+
  annotate(geom = "text",
           x=c(270,270,270),
           y=c(5,10,15),
           label = c("5%","10%","15%"))+
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())+
  coord_polar(theta="x", start = -0.383972)+
  scale_x_continuous(breaks = c(0, 45, 90, 135,180,225,270,315),  
                     labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), 
                     minor_breaks = NULL,
                     expand = c(0, 0))+ scale_y_continuous(expand = c(0, 0))#+
  #facet_wrap(year~.)
b_plot

wind_percent<-wind_percent%>% 
  ungroup() %>% 
  mutate(year=factor(year))

w_plot<-wind_percent  %>% 
  filter(year!="NA") %>% 
  ggplot()+
  geom_line(aes(x=group_dir, y=percent, color=year), size=1)+
  geom_line(data=subset(wind_percent, year=="2019"), 
            aes(x=group_dir, y=percent, color=year),linetype="solid", size=2)+
  theme_minimal()+
  annotate(geom = "text",
           x=c(270,270,270,270),
           y=c(10,15,20,25),
           label = c("10%","15%","20%","25%"))+
  scale_color_manual(values=ypal2019)+
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())+
  ggtitle("Winds at Cove Point Marsh")+
  scale_x_continuous(breaks = c(0, 45, 90, 135,180,225,270,315), limits = c(0,360), 
                     labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), minor_breaks = NULL)+
  coord_polar(theta="x", start = )+
  guides(color=guide_legend(title="Year"))
w_plot

high_percent<-high_percent%>% 
  ungroup() %>% 
  mutate(year=factor(year))

h_plot<-high_percent  %>% 
  filter(year!="NA") %>% 
  ggplot()+
  geom_line(aes(x=group_dir, y=percent, color=year), size=1)+
  geom_line(data=subset(high_percent, year=="2019"), 
            aes(x=group_dir, y=percent, color=year),linetype="solid", size=2)+
  theme_minimal()+
  annotate(geom = "text",
           x=c(135,135,135),
           y=c(20,30,40),
           label = c("20%","30%","40%"))+
  scale_color_manual(values=ypal2019)+
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())+
  ggtitle("High Winds at Cove Point Marsh")+
  scale_x_continuous(breaks = c(0, 45, 90, 135,180,225,270,315), limits = c(0,360), 
                     labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), minor_breaks = NULL)+
  coord_polar(theta="x", start = )+
  guides(color=guide_legend(title="Year"))
h_plot

s_plot<-s_percent %>% 
  filter(year!="NA") %>%
  filter(year!="2010") %>% 
  ggplot()+
  geom_line(aes(x=group_dir, y=percent, color=year), size=1)+
  geom_line(data=subset(s_percent, year=="2019"), 
            aes(x=group_dir, y=percent, color=year),linetype="solid", size=2)+
  theme_minimal()+
  # geom_text(data=data.frame(x=c(270),
  #                           y=c(30),
  #                           label = c("30%")), 
  #           aes(x,y,label=label), inherit.aes=FALSE)+
  # annotate(geom = "text",
  #          x=c(240,240,240),
  #          y=c(20,30,40),
  #          label = c("20%","30%","40%"))+
  scale_color_manual(values=ypal2019)+
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())+
  ggtitle("Seasonal Winds at Cove Point Marsh")+
  scale_x_continuous(breaks = c(0, 45, 90, 135,180,225,270,315), limits = c(0,360), 
                     labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), minor_breaks = NULL)+
  coord_polar(theta="x", start = )+
  guides(color=guide_legend(title="Year"))+
  facet_grid(.~season)
s_plot  

#####Storms#####
storms<-read.csv("H://0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/ysi_noaa_wq_weather_tides/data/nws_storms/storm_data_search_results.csv") %>% 
  select(1:6, EVENT_NARRATIVE,EPISODE_NARRATIVE)
storms<-storms %>% 
  mutate(BEGIN_TIME=sprintf('%04d', BEGIN_TIME)) %>% 
  mutate(datetime=mdy_hm(paste0(BEGIN_DATE," ",BEGIN_TIME)))
storms2<-storms %>% 
  filter(EVENT_TYPE != "Heat") %>%
  filter(EVENT_TYPE != "Excessive Heat") %>% 
  filter(EVENT_TYPE != "Dense Fog") %>% 
  filter(EVENT_TYPE != "Tornado")
storms3<-storms %>% mutate(begin_datetime=mdy(as.character(BEGIN_DATE))) %>% 
  mutate(year=year(begin_datetime)) %>% 
  group_by(year) %>% 
  count(.,EVENT_TYPE) 
storms_4<-storms3%>% 
  summarise(all_events=sum(n)) %>% 
  left_join(storms3,.)
storms_4<-storms_4 %>% 
  mutate(EVENT_TYPE=factor(EVENT_TYPE, levels = c("Coastal Flood",
                                                  "Flash Flood",
                                                  "Flood",
                                                  "Hail",
                                                  "High Wind",
                                                  "Strong Wind",
                                                  "Thunderstorm Wind",
                                                  "Tropical Storm",
                                                  "Tornado",
                                                  "Blizzard",
                                                  "Frost/Freeze",
                                                  "Winter Storm",
                                                  "Winter Weather",
                                                  "Ice Storm",
                                                  "Cold/Wind Chill",
                                                  "Excessive Heat",
                                                  "Heat",
                                                  "Dense Fog")))

storms5<-storms %>% 
  mutate(begin_datetime=mdy(as.character(BEGIN_DATE))) %>% 
  mutate(year=year(begin_datetime))


storm_plot<-storms_4 %>% 
  ggplot(.,aes(x=factor(year), y=n, fill=EVENT_TYPE))+
  geom_col(position="stack", color="black")+
  theme(axis.line = element_line(color="black"),
        panel.backgroun=element_blank())+
  scale_y_continuous(breaks=seq(0,60,10), expand = c(0,0))+
  scale_fill_manual(values = stormpal)+
  ggtitle("Weather Events for Calvert County recorded by the National Weather Service")+
  xlab("Year")+ylab("Number of Events")+labs(fill="Event Type")
storm_plot


# tides is in GMT so need to convert to line up times

tides<-readRDS("tides.rds")
tides<-tides %>%
  mutate(datetime=`time (GMT)`-(5*60*60))

tides2<-tides %>% 
  filter()

a<-ggplot()+
  geom_line(data=tides,color="grey84",aes(x=datetime,
                           y= `verified water level at Solomons Island, MD (meters rel. to NAVD)`))+
  geom_point(data=filter(storms5,year==2019), aes(x=datetime, y=0.55, 
                              color=EVENT_TYPE), size=2)+
  geom_hline(aes(yintercept=0.19), linetype="dashed", color="blue")+
  geom_hline(aes(yintercept=-0.26), linetype="dashed", color="blue")+
  
  theme_classic()+
  scale_x_datetime(limits = c(as.POSIXct("2019-01-01 00:00:00"),as.POSIXct("2019-12-31 23:59:59")))
a
