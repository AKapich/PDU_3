library(data.table)
library(dplyr)
library(tidyverse)
library(stringi)
library(lubridate)

setwd("C:/Users/wojte/OneDrive/Pulpit/Dane_PD3")
options(stringsAsFactors = FALSE)

m_06 <- read.csv("202006-citibike-tripdata.csv")
m_07 <- read.csv("202007-citibike-tripdata.csv")
m_08 <- read.csv("202008-citibike-tripdata.csv")
m_09 <- read.csv("202009-citibike-tripdata.csv")

m_06 <- as.data.table(m_06)
m_07 <- as.data.table(m_07)
m_08 <- as.data.table(m_08)
m_09 <- as.data.table(m_09)

m_06 <- m_06[, .(starttime, stoptime, start.station.name, end.station.name, tripduration)]
m_07 <- m_07[, .(starttime, stoptime, start.station.name, end.station.name, tripduration)]
m_08 <- m_08[, .(starttime, stoptime, start.station.name, end.station.name, tripduration)]
m_09 <- m_09[, .(starttime, stoptime, start.station.name, end.station.name, tripduration)]

files_average <- list(m_06,m_07,m_08,m_09)
rm(m_06,m_07,m_08,m_09)

grouping_average <- function(df){
  temp <- df[, .(Data = stri_sub(starttime, 1, 10),
                 Godzina_start = stri_sub(starttime, 12, 16),
                 Godzina_stop = stri_sub(stoptime, 12, 16),
                 start.station.name,
                 end.station.name)]
  
  temp_speed <- df[, .(Data = stri_sub(starttime, 1, 10),
                       Godzina_start = stri_sub(starttime, 12, 16),
                       Godzina_stop = stri_sub(stoptime, 12, 16),
                       start.station.name,
                       end.station.name,
                       tripduration)]
  
  
  temp <- temp[, .(Grupa_osob=.N),
               .(Data, Godzina_start, Godzina_stop, start.station.name, end.station.name)][Grupa_osob==2, ]
  
  ans <- setkey(setDT(temp_speed), Data, Godzina_start, Godzina_stop, start.station.name, end.station.name)[temp]
  
  ans <- ans[Grupa_osob==2, .(tripduration, Data, Godzina=stri_sub(Godzina_start,1,2))][,.(sredni_czas=mean(tripduration)),Godzina]
  
  return(ans)
}

overall_average <- function(df){
  df <- df[ ,.(Godzina=stri_sub(starttime,12,13),tripduration)]
  ans <- df[,.(sredni_czas=mean(tripduration)),Godzina]
  return(ans)
}

ans_average <- lapply(files_average, grouping_average)
ans_overall <- lapply(files_average, overall_average)
