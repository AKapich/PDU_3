library(ggplot2)
library(dplyr)
library(babynames)
library(viridis)
library(hrbrthemes)
library(plotly)

results_weekends <- results_weekends %>%
                      mutate( mytext=paste(
                      "Numer tygodnia: ", Tydzien, "\n",
                      "Weekend/dowolny dzien: ", Typ, "\n", 
                      "Iloœæ przejazdów: ", Liczba, sep=""))
  
p <- results_weekends %>% 
  ggplot( aes(x=Tydzien, y=Liczba, fill=Typ, text=Typ)) +
  geom_area( ) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Liczba przejazdów dwuosobowych w roku 2020") +
  coord_cartesian(xlim = c(1, 52), ylim = c(0, 21000)) +
  theme_ipsum() +
  theme(legend.position="none")

p <- ggplotly(p, tooltip=c("x","y","fill"))
p

