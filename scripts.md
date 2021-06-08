#packages required

library(tidyverse)
library(dplyr)

#which creoles are the most stable?

creole_stability <-  
  select(database, language, place_stability, manner_stability) %>% 
  transform(creole_stability$place_stability, creole_stability$manner_stability 
            == as.numeric(place_stability, manner_stability)) %>% 
  mutate(creole_stability, global_stability = (place_stability + manner_stability)/2) %>% 
  group_by(language) %>%
  summarize(mean_creole_stability = mean(global_stability))
  
  #in progress
  ## I am having problems with the missing values
