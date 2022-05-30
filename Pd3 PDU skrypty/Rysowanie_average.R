library(gifski)
library(png) 
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(tidyverse)
library(wesanderson)
library(gganimate)
library(hrbrthemes)


options(stringsAsFactors = T)
head(babynames)
df <- bind_rows(ans_average[[1]],ans_average[[2]],ans_average[[3]],ans_average[[4]])

df2 <- bind_rows(ans_overall[[1]],ans_overall[[2]],ans_overall[[3]],ans_overall[[4]])

Dwojki <- df[, .(`Sredni czas`=mean(sredni_czas)), Godzina]
Dwojki <- Dwojki[,.(Godzina,`Sredni czas`=`Sredni czas`/60, Typ="Grupa 2-osobowa")]

Wszyscy <- df2[, .(`Sredni czas`=mean(sredni_czas)), Godzina]
Wszyscy <- Wszyscy[,.(Godzina,`Sredni czas`=`Sredni czas`/60, Typ="Dowolny przejazd")]

colnames(Dwojki) <- c("Godzina", "Avg", "Typ")
colnames(Wszyscy) <- c("Godzina", "Avg", "Typ")

dane <- bind_rows(Dwojki, Wszyscy)
dane<-dane[order(Godzina),][,.(Godzina=as.numeric(Godzina),Avg,Typ)]
cat("00",":00")

ggplot(dane, aes(x=Godzina, y=Avg, group=Typ,color=Typ)) +
  geom_line() +
  geom_point()

Wykres <- dane %>% ggplot(aes(x=Godzina, y=Avg, group=Typ, color=Typ)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(alpha = 1, begin = 0.2, end=0.5, discrete = TRUE,) +
  ggtitle("Porównanie œrednich d³ugoœci przejazdów (lato 2020)") +
  theme_ipsum() +
  ylab("Œrednia d³ugoœæ przejazdu (w minutach)") +
  xlab("Godzina dnia")+
  transition_reveal(Godzina)

animate(Wykres, duration = 5, fps = 20, width = 600, height = 400, renderer = gifski_renderer())
anim_save("output.gif")
