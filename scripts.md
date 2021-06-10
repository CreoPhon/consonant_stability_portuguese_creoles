# Which creoles are the most stable?

## Packages required

library(tidyverse)
library(dplyr)

## Preparing the data

creole_stability <-
  select(database, language, place_stability, manner_stability)

creole_stability$place_stability = as.numeric(creole_stability$place_stability)

creole_stability$manner_stability = as.numeric(creole_stability$manner_stability)

## Calculating stability for each creole

global_creole_stability <- mutate(creole_stability, global_stability = (place_stability + manner_stability)/2)

final_results <- global_creole_stability %>% group_by(language) %>% summarize(m = mean(global_stability, na.rm = TRUE))

## Ploting the results

region <- c("GG", "UG", "UG", "UG", "UG", "UG", "NI", "NI", "GG", "UG", "SI", "NI", "SA", "GG", "GG", "SI")

final_results_region <- cbind(final_results, region)

ggplot(final_results_region) + geom_bar(aes(x = m, y = reorder(language, m), fill = region), stat = "identity", show.legend = FALSE)


# Which segments are the most stable?
