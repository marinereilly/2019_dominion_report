devtools::install_github("troyhill/VulnToolkit")

library(VulnToolkit)

test<-noaa(begindate = "20100101", enddate = "20191231", station = "8577330", 
     units = "meters", datum = "NAVD", interval = "hourly", time = "GMT", 
     continuous = "TRUE")
saveRDS(test,"tides.rds")
library(ggplot2)
a<-ggplot(test,aes(x=`time (GMT)`, y=`verified water level at Solomons Island, MD (meters rel. to NAVD)`))+
  geom_line()+geom_hline(aes(yintercept=0.75), color="red",linetype="dashed")+
  theme_bw()+scale_x_datetime(breaks = "years", date_labels = "%Y")
a
