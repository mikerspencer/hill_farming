# ----------------------------------------------------
# ----------------------------------------------------
# Read ag census data
# ----------------------------------------------------
# ----------------------------------------------------

library(tidyverse)

ag_schema = read_csv("~/data/ag_census/AgCensusData/ag_census_schema.csv")

# 2000 ag census used to avoid foot and mouth
ag_census_2000 = read_csv("~/data/ag_census/AgCensusData/ag_census_2000.csv") %>% 
   select(PARISH, ITEM50,
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
   select(PARISH, ITEM50,
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

# PARISH	Parish Number
# TOTAREA	Total Area
# 
# ITEM40	Total Crops and Fallow
# ITEM68	Total Vegetables
# ITEM76	Total Soft Fruit
# ITEM84	Total bulbs, flowers and nursery stock
# 
# ITEM122	Total cattle
# ITEM145	Total sheep, rams and lambs
# 
# ITEM177	Occupier working full-time on holding
# ITEM178	Occupier working half-time or more on holding
# ITEM179	Occupier working less than half time on holding
# ITEM181	Wife/husband of occupier (if doing farm work0
# ITEM182	Spouse working full-time on holding
# ITEM183	Spouse working half-time or more on holding
# ITEM184	Spouse working less than half time on holding
# ITEM177	Occupier working full-time on holding
# ITEM178	Occupier working half-time or more on holding
# ITEM179	Occupier working less than half time on holding
# ITEM182	Spouse working full-time on holding
# ITEM183	Spouse working half-time or more on holding
# ITEM184	Spouse working less than half time on holding
# ITEM188	Full-time Hired Males 20 yrs and over
# ITEM189	Full-time Family Males 20 yrs and over
# ITEM190	Full-time Hired Males under 20 yrs
# ITEM191	Full-time Family Males under 20 yrs
# ITEM192	Full-time hired Females
# ITEM193	Full-time Family Females
# ITEM194	Part-time Hired Males
# ITEM195	Part-time Family Males
# ITEM196	Part-time Hired Females
# ITEM197	Part-time Family Females
# ITEM198	Casual and Seasonal Males
# ITEM199	Casual and Seasonal Females
# ITEM1714	Full-time regular staff - male - partners
# ITEM1715	Full-time regular staff - male - hired
# ITEM1716	Full-time regular staff - male - member of occupiers family
# ITEM1717	Full-time regular staff - female - partners
# ITEM1718	Part-time regular staff - male - partners
# ITEM1719	Part-time regular staff - female - partners
# ITEM2066 	Person Working Days - contract labour
# ITEM2712	Non-family labour employed on non-regular basis
