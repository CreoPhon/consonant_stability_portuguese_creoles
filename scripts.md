#packages required

library(tidyverse)
library(dplyr)

#which creoles are the most stable?

creole_stability <-  
  select(database, language, place_stability, manner_stability)


creole_stability$place_stability = as.numeric(creole_stability$place_stability)
               
creole_stability$manner_stability = as.numeric(creole_stability$manner_stability)


global_creole_stability <- mutate(creole_stability, global_stability = 
                                   (place_stability + manner_stability)/2) 
  

final_results <- global_creole_stability %>%  group_by(language) %>%
  summarize(m = mean(global_stability, na.rm = TRUE))

ggplot(final_results) + 
  geom_bar(aes(x = language, y = m, fill = language), stat = "identity")
