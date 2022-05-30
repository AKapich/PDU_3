library(data.table)
library(dplyr)
library(tidyverse)
library(stringi)
library(lubridate)

setwd("C:/Users/wojte/OneDrive/Pulpit/Dane_PD3")
options(stringsAsFactors = FALSE)

m_01 <- as.data.table(read.csv("202001-citibike-tripdata.csv"))
m_01 <- m_01[, .(starttime, stoptime, start.station.name, end.station.name,
                   week=lubridate::week(ymd(stri_sub(starttime,1,10))))]
m_02 <- as.data.table(read.csv("202002-citibike-tripdata.csv"))
m_02 <- m_02[, .(starttime, stoptime, start.station.name, end.station.name,
                 week=lubridate::week(ymd(stri_sub(starttime,1,10))))]
m_03 <- as.data.table(read.csv("202003-citibike-tripdata.csv"))
m_03 <- m_03[, .(starttime, stoptime, start.station.name, end.station.name,
                 week=lubridate::week(ymd(stri_sub(starttime,1,10))))]
m_04 <- as.data.table(read.csv("202004-citibike-tripdata.csv"))
m_04 <- m_04[, .(starttime, stoptime, start.station.name, end.station.name,
                 week=lubridate::week(ymd(stri_sub(starttime,1,10))))]
m_05 <- as.data.table(read.csv("202005-citibike-tripdata.csv"))
m_05 <- m_05[, .(starttime, stoptime, start.station.name, end.station.name,
                 week=lubridate::week(ymd(stri_sub(starttime,1,10))))]
m_06 <- as.data.table(read.csv("202006-citibike-tripdata.csv"))
m_06 <- m_06[, .(starttime, stoptime, start.station.name, end.station.name,
                 week=lubridate::week(ymd(stri_sub(starttime,1,10))))]
m_07 <- as.data.table(read.csv("202007-citibike-tripdata.csv"))
m_07 <- m_07[, .(starttime, stoptime, start.station.name, end.station.name,
                 week=lubridate::week(ymd(stri_sub(starttime,1,10))))]
m_08 <- as.data.table(read.csv("202008-citibike-tripdata.csv"))
m_08 <- m_08[, .(starttime, stoptime, start.station.name, end.station.name,
                 week=lubridate::week(ymd(stri_sub(starttime,1,10))))]
m_09 <- as.data.table(read.csv("202009-citibike-tripdata.csv"))
m_09 <- m_09[, .(starttime, stoptime, start.station.name, end.station.name,
                 week=lubridate::week(ymd(stri_sub(starttime,1,10))))]
m_10 <- as.data.table(read.csv("202010-citibike-tripdata.csv"))
m_10 <- m_10[, .(starttime, stoptime, start.station.name, end.station.name,
                 week=lubridate::week(ymd(stri_sub(starttime,1,10))))]
m_11 <- as.data.table(read.csv("202011-citibike-tripdata.csv"))
m_11 <- m_11[, .(starttime, stoptime, start.station.name, end.station.name,
                 week=lubridate::week(ymd(stri_sub(starttime,1,10))))]
m_12 <- as.data.table(read.csv("202012-citibike-tripdata.csv"))
m_12 <- m_12[, .(starttime, stoptime, start.station.name, end.station.name,
                 week=lubridate::week(ymd(stri_sub(starttime,1,10))))]


is_weekday <- function(string){
  
  string <- stri_sub(string,1,10)
  
  lubridate::wday(string, week_start = 1) < 5
}

files_grouping <- list(m_01,m_02,m_03,m_04,m_05,m_06,m_07,m_08,m_09,m_10,m_11,m_12)
rm(m_01,m_02,m_03,m_04,m_05,m_06,m_07,m_08,m_09,m_10,m_11,m_12)

grouping_weekends <- function(df){
  temp <- df[is_weekday(starttime), .(Data = stri_sub(starttime, 1, 10),
                 Godzina_start = stri_sub(starttime, 12, 16),
                 Godzina_stop = stri_sub(stoptime, 12, 16),
                 start.station.name,
                 end.station.name,
                 week)]
  
  temp_weekend <- df[!is_weekday(starttime), .(Data = stri_sub(starttime, 1, 10),
                 Godzina_start = stri_sub(starttime, 12, 16),
                 Godzina_stop = stri_sub(stoptime, 12, 16),
                 start.station.name,
                 end.station.name,
                 week)]
  
  temp <- temp[, .(Grupa_osob=.N),
               .(Data, Godzina_start, Godzina_stop, start.station.name, end.station.name,week)][Grupa_osob==2, ]
  
  temp_weekend <- temp_weekend[, .(Grupa_osob=.N),
               .(Data, Godzina_start, Godzina_stop, start.station.name, end.station.name,week)][Grupa_osob==2, ]
  
  ans_1 <- temp[, .(Liczba=.N), week][,.(Tydzien=week, Liczba, Typ="Dzieñ powszedni")]
  ans_2 <- temp_weekend[, .(Liczba=.N), week][,.(Tydzien=week, Liczba, Typ="Weekend")]
  ans <- bind_rows(ans_1,ans_2)
  ans[order(Tydzien),]
  return(ans)
  
}

results_weekends <- lapply(files_grouping, grouping_weekends)

results_weekends <- bind_rows(results_weekends[[1]],results_weekends[[2]],results_weekends[[3]],
                              results_weekends[[4]],results_weekends[[5]],results_weekends[[6]],
                              results_weekends[[7]],results_weekends[[8]],results_weekends[[9]],
                              results_weekends[[10]],results_weekends[[11]],results_weekends[[12]])

results_weekends <- results_weekends[,.(Liczba=sum(Liczba)),.(Tydzien,Typ)]
results_weekends <- results_weekends[order(Tydzien, -Typ),]
