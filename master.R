library(tidyverse)
library(readr)

#DENSIDAD
datos <- read_csv("../R-homework2/dataset/googleplaystore.csv")
datos %>%  filter(is.na(Rating) == FALSE)%>% 
  ggplot(aes(x=Rating),!is.na(datos$Rating)) + 
  geom_density(color="darkblue", fill="lightblue")

#CAJITAS
datos %>% filter(is.na(`Content Rating`) == FALSE)%>% 
  ggplot(aes(x=`Content Rating`, y= Rating)) + 
  geom_boxplot(fill = "#4271AE", colour = "#1F3552")+scale_x_discrete(name = "Rango Etario") +
  scale_y_continuous(name = "Ranting")

#TORTA
