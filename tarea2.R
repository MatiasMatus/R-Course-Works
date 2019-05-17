library(tidyverse)
library(readr)


datos <- read_csv("../Curso R/googleplaystore.csv")
datos %>% 
 ggplot(aes(x=Rating),!is.na(datos$Rating)) + 
  geom_density(color="darkblue", fill="lightblue")
