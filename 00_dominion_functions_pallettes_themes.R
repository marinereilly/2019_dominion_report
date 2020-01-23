#####themes#####

#####functions#####
season<-function(df){
  mutate(df,season=case_when(
    month==1     ~ "Winter",
    month==2     ~ "Winter",
    month==3     ~ "Spring",
    month==4     ~ "Spring",
    month==5     ~ "Spring",
    month==6     ~ "Summer",
    month==7     ~ "Summer",
    month==8     ~ "Summer",
    month==9     ~ "Autumn",
    month==10    ~ "Autumn",
    month==11    ~ "Autumn",
    month==12    ~ "Winter"
  ))
}

date_cols<-function(df){
  mutate(df,
         year=year(datetime),
         doy=yday(datetime),
         month=month(datetime),
         day=day(datetime)) %>% 
    mutate(plotdate=ymd(paste0("1996-",month,"-",day)))
}
#####palettes#####

cp_pal<-c("north"="#FF5765", 
          "mid" = "#FFDB15",
          "south"="#8A6FDF",
          "weir"="#A8E10C",
          "hobo"="#A8E10C",
          "kayak survey"="steelblue2")

year_color_highlight<-c("2019"="red3","2018"="grey75","2017"="grey75", "2016"="grey75",
                        "2015"="grey75", "2014"="grey75", "2013"="grey75",
                        "2012"="grey75", "2011"="grey75","2010"="grey75",
                        "2020"="darkblue")
year_size_highlight<-c("2018"=1, "2017"=1, "2016"=1, "2015"=1, "2014"=1, "2013"=1,
                       "2012"=1, "2011"=1, "2010"=1, "2019"=1.5, "2020"=1)

pal7<-c("north"="#E69F00", "mid"="#F0E442", "south"="#009E73", "hobo"="#56B4E9")
pal8<-c("rainfall"="darkorchid3", "depth_ft"="navy", "depth_m"="navy")
pal9<-c("north"="#E69F00", "mid"="#F0E442", "south"="#009E73","depth_ft"="navy", 
        "depth_m"="navy","rainfall"="darkorchid3")

ypal2019 <- c("2012"="#fde0dd", "2013"="#fcc5c0", "2014"="#fa9fb5", "2015"="#f768a1", 
              "2016"="#dd3497", "2017"="#ae017e", "2018"="#7a0177", "2019"="#49006a", 
              "2010"="grey80", "2011"="grey80")

pos_neg_colors<-c("Positive"="green3", "Negative"="red2")

colo3<-c("Plot A"="#69B5B3", "Plot B"="#E87770", "Plot C"="blue4")
colo4<-c("Plot A"="#d94801", "Plot B"="#2171b5", "Plot C"="#238b45")
