# prepare the libraries
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(leafletR)) install.packages("leafletR", repos = "http://cran.us.r-project.org")



setwd("F:/study/POST GSD/Self study/machine learning/ncov transportation/leaflet map/ncov-train-and-flight-map/csv")

# read in files
edge <- read.csv(file = '20200219 train update - edges.csv',sep = ",",fileEncoding = "UTF-8")
station <- read.csv(file = '20200219 train update - station full.csv',encoding = "UTF-8")

# get the full expanded data for daily new and daily accumulate of source (the out number)
source_short<-edge%>%
  group_by(source,timestamp)%>%
  count()%>%
  dcast(source ~ timestamp,value.var="n")
source_short[is.na(source_short)] <- 0
source_long<-gather(source_short,key=timestamp,value,-source)%>%
  group_by(source) %>%
  mutate("accumulate" = cumsum(value)) %>%
  mutate(out_or_in="out")
names(source_long)[names(source_long) == "source"] <- "new_id"
source_long<-data.frame(source_long)

# get the full expanded data for daily new and daily accumulate of target (the in number)
target_short<-edge%>%
  group_by(target,timestamp)%>%
  count()%>%
  dcast(target ~ timestamp,value.var="n")
target_short[is.na(target_short)] <- 0
target_long<-gather(target_short,key=timestamp,value,-target)%>%
  group_by(target) %>%
  mutate("accumulate" = cumsum(value)) %>%
  mutate(out_or_in="in")
names(target_long)[names(target_long) == "target"] <- "new_id"
target_long<-data.frame(target_long)

# bind in and out together
station_daily<-rbind(source_long,target_long)

# match lat and long for each station
station_in_use<-station[c(3,5,8,9,11,12)]
station_daily<-station_daily%>%
  left_join(station_in_use,by="new_id")
station_daily[is.na(station_daily)] <- 0

# convert to geojson
# toGeoJSON(station_daily,name="station_daily", dest=getwd(), overwrite=TRUE)

write.csv(station_daily,"station_daily.csv",fileEncoding = "UTF-8")




