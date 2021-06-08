#packages required

library(tidyverse)
library(dplyr)

#which creoles are the most stable?

creole_stability <-  
  select(database, language, place_stability, manner_stability) %>% 
  transform(creole_stability$place_stability, creole_stability$manner_stability 
            == as.numeric(place_stability, manner_stability)) %>% 
  mutate(creole_stability, global_stability = (place_stability + manner_stability)/2)
  
  #in progress
