# ----------------------------------------------------
# ----------------------------------------------------
# Rough grazing
# ----------------------------------------------------
# ----------------------------------------------------

library(tidyverse)

ag_census_2011 = read_csv("~/data/ag_census/AgCensusData/ag_census_2011.csv") %>% 
   rename_all(toupper) %>% 
   select(PARISH, ITEM47) %>% 
   group_by(PARISH) %>% 
   summarise_all(sum)

write_csv(ag_census_2011, "projects/hill_farming/data/rough_grazing.csv")
