library(ggplot2)
library(viridis)
library(hrbrthemes)
library(tidyverse)
library(wesanderson)

r_2017 <-cbind(results_sub[[1]], Rok="2017")
r_2018 <-cbind(results_sub[[2]], Rok="2018")
r_2019 <-cbind(results_sub[[3]], Rok="2019")
r_2020 <-cbind(results_sub[[4]], Rok="2020")
r_2021 <-cbind(results_sub[[5]], Rok="2021")

df <- bind_rows(r_2017,r_2018,r_2019,r_2020,r_2021)

ggplot(df, aes(fill=`Liczba subskrypcji`, y=Ilosc, x=Rok)) +
  geom_bar(position="stack", stat="identity") +
  ggtitle("Zmiana udzia³u subskrybentów w przejazdach 2-osobowych") +
  scale_fill_viridis(alpha = 1, begin = 0.65, end=1,discrete = T) +
  scale_color_gradient(low="blue", high="red")+
  theme_ipsum() +
  xlab("") +
  ylab("")

