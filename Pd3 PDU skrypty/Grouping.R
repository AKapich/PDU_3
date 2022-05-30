library(data.table)
library(dplyr)
library(tidyverse)
library(stringi)

setwd("C:/Users/wojte/OneDrive/Pulpit/Dane_PD3")
options(stringsAsFactors = FALSE)

r_2017 <- read.csv("201709-citibike-tripdata.csv")
r_2018 <- read.csv("201809-citibike-tripdata.csv")
r_2019 <- read.csv("201909-citibike-tripdata.csv")
r_2020 <- read.csv("202009-citibike-tripdata.csv")
r_2021 <- read.csv("202109-citibike-tripdata.csv")

r_2017 <- as.data.table(r_2017)
r_2018 <- as.data.table(r_2018)
r_2019 <- as.data.table(r_2019)
r_2020 <- as.data.table(r_2020)
r_2021 <- as.data.table(r_2021)

r_2017 <- r_2017[, .(starttime, stoptime, start.station.name, end.station.name)]
r_2018 <- r_2018[, .(starttime, stoptime, start.station.name, end.station.name)]
r_2019 <- r_2019[, .(starttime, stoptime, start.station.name, end.station.name)]
r_2020 <- r_2020[, .(starttime, stoptime, start.station.name, end.station.name)]
r_2021<-r_2021[, .(starttime = started_at,
                   stoptime = ended_at,
                   start.station.name=start_station_name,
                   end.station.name=end_station_name)]

files_grouping <- list(r_2017,r_2018,r_2019,r_2020,r_2021)
rm(r_2017,r_2018,r_2019,r_2020,r_2021)

grouping <- function(df){
  temp <- df[, .(Data = stri_sub(starttime, 1, 10),
                            Godzina_start = stri_sub(starttime, 12, 16),
                            Godzina_stop = stri_sub(stoptime, 12, 16),
                            start.station.name,
                            end.station.name)]
  
  
  temp <- temp[, .(Grupa_osob=.N), .(Data, Godzina_start, Godzina_stop, start.station.name, end.station.name)][Grupa_osob>1, ]
  
  ans <- temp[, .N, Grupa_osob][order(-N),]
  
  ans <- ans[, .(Grupa_osob, Ilosc=N)]
  return(ans)
  
}

results <- lapply(files_grouping, grouping)
