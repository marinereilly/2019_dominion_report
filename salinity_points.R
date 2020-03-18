library(sf)
library(dplyr)
library(lubridate)

# filenames <- list.files("H://0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/01_GPS_waypoints/2019/salinity",
#                         pattern="*.gpx", full.names = FALSE)
# file_names<-tools::file_path_sans_ext(filenames)

# salinity<-readxl::read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/salinity_kayak/data/2019/2019_SalinityProfiles_DOMINION.xlsx",
# sheet = "Salinities", col_types = c("text",
# "text", "numeric", "numeric", "numeric",
# "numeric", "text", "date", "text",
# "text", "numeric"))
# salinity<-salinity %>% 
#   mutate(time=paste0(hour(Time),":",minute(Time)),
#          date=mdy(Date))
my_files<-list.files("H://0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/01_GPS_waypoints/2019/salinity",
                    pattern="*.gpx", full.names = TRUE)
my_stuff<-list.files("H://0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/01_GPS_waypoints/2019/salinity",
                     pattern="*.gpx", full.names = FALSE)
my_filenames<-tools::file_path_sans_ext(my_stuff)

my_data <- list()
for (i in seq_along(my_files)) {
  my_data[[i]] <- st_read(my_files[i])
  my_data[[i]]<-my_data[[i]] %>% 
    select(time,geometry) %>% 
    mutate(datetime=lubridate::ymd_hms(time),
           file_name=my_filenames[i]) %>% 
    mutate(date=lubridate::date(time),
           time=paste0(hour(datetime),":",minute(datetime)),
           salinity=999) %>% 
    tidyr::separate(file_name, into=c("filename", "delete"), sep=".") %>% 
    select(filename, datetime, date, time, salinity,geometry)
  st_write(my_data[[i]], 
           paste0("H://0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/01_GPS_waypoints/2019/salinity/shapefiles/",
                  my_filenames[i], ".shp"))
    
}

View(my_data[[1]])




for(i in file_names){
  filepath <- file.path("H://0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/01_GPS_waypoints/2019/salinity",paste(i,".gpx",sep=""))
assign(i, select(st_read(filepath),time,geometry))
}

dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

for(i in dfs){
  i<-i %>% 
    select(time, geometry) %>% 
    mutate(datetime=lubridate::ymd_hms(time)) %>% 
    mutate(date=lubridate::date(time),
           time=paste0(hour(datetime),":",minute(datetime)))
}




####
filepath <- file.path("H://0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/01_new_dominion/surveys/01_GPS_waypoints/2019/",
                      paste(file_names[1],".gpx",sep=""))
j<-assign(file_names[1], st_read(filepath)) %>% 
  select(time,geometry) %>% 
  mutate(datetime=lubridate::ymd_hms(time)) %>% 
  mutate(date=lubridate::date(time),
         time=paste0(hour(datetime),":",minute(datetime)))
k<-left_join(j,salinity, by= c("date","time"))  

View(j)
d<-filter(salinity,date==as.Date("2019-05-24"))
