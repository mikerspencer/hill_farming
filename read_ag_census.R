# ----------------------------------------------------
# ----------------------------------------------------
# Read ag census data
# ----------------------------------------------------
# ----------------------------------------------------

library(tidyverse)

ag_schema = read_csv("~/data/ag_census/AgCensusData/ag_census_schema.csv")

# 2000 ag census used to avoid foot and mouth
ag_census_2000 = read_csv("~/data/ag_census/AgCensusData/ag_census_2000.csv") %>% 
   select(PARISH, ITEM50, ITEM7, ITEM11,
          ITEM40, ITEM68, ITEM76, ITEM84,
          ITEM122, ITEM145,
          ITEM177, ITEM178, ITEM179, ITEM182, ITEM183, ITEM184, ITEM1714, ITEM1715, ITEM1716, ITEM1717, ITEM1718, ITEM1719) %>% 
   mutate(ITEM50=replace(ITEM50, ITEM50==".", "0"),
          ITEM50=as.numeric(ITEM50)) %>% 
   group_by(PARISH) %>% 
   summarise_all(sum)

ag_schema %>% 
   select(`Item name`, Description) %>% 
   filter(`Item name` %in% colnames(ag_census_2000)) %>% 
   data.frame()


ag_census_2011 = read_csv("~/data/ag_census/AgCensusData/ag_census_2011.csv") %>% 
   rename_all(toupper) %>% 
   select(PARISH, ITEM50, ITEM7, ITEM11,
          ITEM40, ITEM68, ITEM76, ITEM84,
          ITEM122, ITEM145,
          ITEM177, ITEM178, ITEM179, ITEM182, ITEM183, ITEM184, ITEM1714, ITEM1715, ITEM1716, ITEM1717, ITEM1718, ITEM1719) %>% 
   mutate(ITEM50=replace(ITEM50, ITEM50==".", "0"),
          ITEM50=as.numeric(ITEM50)) %>% 
   group_by(PARISH) %>% 
   summarise_all(sum)

ag_schema %>% 
   select(`Item name`, Description) %>% 
   filter(`Item name` %in% colnames(ag_census_2011)) %>% 
   data.frame()

write_csv(ag_census_2000, "~/projects/hill_farming/data/ag_census_2000.csv")
write_csv(ag_census_2011, "~/projects/hill_farming/data/ag_census_2011.csv")
