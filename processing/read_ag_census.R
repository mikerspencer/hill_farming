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
          ITEM177, ITEM178, ITEM179, ITEM182, ITEM183, ITEM184,
          ITEM192, ITEM193, ITEM194, ITEM195, ITEM196, ITEM197, ITEM198, ITEM199,
          ITEM1714, ITEM1715, ITEM1716, ITEM1717, ITEM1718, ITEM1719) %>% 
   mutate(ITEM50=replace(ITEM50, ITEM50==".", "0"),
          ITEM50=as.numeric(ITEM50)) %>% 
   mutate(ITEM177=replace(ITEM177, ITEM177>1, 1),
          ITEM178=replace(ITEM178, ITEM178>1, 1),
          ITEM179=replace(ITEM179, ITEM179>1, 1),
          ITEM182=replace(ITEM182, ITEM182>1, 1),
          ITEM183=replace(ITEM183, ITEM183>1, 1),
          ITEM184=replace(ITEM184, ITEM184>1, 1)) %>%
   group_by(PARISH) %>% 
   summarise_all(sum, na.rm=T) %>% 
   mutate(full_time=ITEM192 + ITEM193 + ITEM1714 + ITEM1715 + ITEM1716 + ITEM1717,
          part_time=ITEM194 + ITEM195 + ITEM196 + ITEM197 + ITEM1718 + ITEM1719,
          casual=ITEM198 + ITEM199) %>% 
   select(-ITEM192, -ITEM193, -ITEM1714, -ITEM1715, -ITEM1716, -ITEM1717, -ITEM194, -ITEM195, -ITEM196, -ITEM197, -ITEM1718, -ITEM1719, -ITEM198, -ITEM199)

# ITEM192 & Full-time hired females \\
# ITEM193 & Full-time family females \\
# ITEM194 & Part-time hired males \\
# ITEM195 & Part-time family males \\
# ITEM196 & Part-time hired females \\
# ITEM197 & Part-time family females \\
# ITEM198 & Casual and seasonal males \\
# ITEM199 & Casual and seasonal females \\
# ITEM1714 & Full-time regular staff - male - partners \\
# ITEM1715 & Full-time regular staff - male - hired \\
# ITEM1716 & Full-time regular staff - male - member of occupiers family \\
# ITEM1717 & Full-time regular staff - female - partners \\
# ITEM1718 & Part-time regular staff - male - partners \\
# ITEM1719 & Part-time regular staff - female - partners \\

ag_schema %>% 
   select(`Item name`, Description) %>% 
   filter(`Item name` %in% colnames(ag_census_2000)) %>% 
   data.frame()


ag_census_2011 = read_csv("~/data/ag_census/AgCensusData/ag_census_2011.csv") %>% 
   rename_all(toupper) %>% 
   select(PARISH, ITEM50, ITEM7, ITEM11,
          ITEM40, ITEM68, ITEM76, ITEM84,
          ITEM122, ITEM145,
          ITEM177, ITEM178, ITEM179, ITEM182, ITEM183, ITEM184,
          ITEM192, ITEM193, ITEM194, ITEM195, ITEM196, ITEM197, ITEM198, ITEM199,
          ITEM1714, ITEM1715, ITEM1716, ITEM1717, ITEM1718, ITEM1719) %>% 
   mutate(ITEM50=replace(ITEM50, ITEM50==".", "0"),
          ITEM50=as.numeric(ITEM50)) %>% 
   mutate(ITEM177=replace(ITEM177, ITEM177>1, 1),
          ITEM178=replace(ITEM178, ITEM178>1, 1),
          ITEM179=replace(ITEM179, ITEM179>1, 1),
          ITEM182=replace(ITEM182, ITEM182>1, 1),
          ITEM183=replace(ITEM183, ITEM183>1, 1),
          ITEM184=replace(ITEM184, ITEM184>1, 1)) %>% 
   group_by(PARISH) %>% 
   summarise_all(sum, na.rm=T) %>% 
   mutate(full_time=ITEM192 + ITEM193 + ITEM1714 + ITEM1715 + ITEM1716 + ITEM1717,
          part_time=ITEM194 + ITEM195 + ITEM196 + ITEM197 + ITEM1718 + ITEM1719,
          casual=ITEM198 + ITEM199) %>% 
   select(-ITEM192, -ITEM193, -ITEM1714, -ITEM1715, -ITEM1716, -ITEM1717, -ITEM194, -ITEM195, -ITEM196, -ITEM197, -ITEM1718, -ITEM1719, -ITEM198, -ITEM199)

ag_schema %>% 
   select(`Item name`, Description) %>% 
   filter(`Item name` %in% colnames(ag_census_2011)) %>% 
   data.frame()

write_csv(ag_census_2000, "~/projects/hill_farming/data/ag_census_2000.csv")
write_csv(ag_census_2011, "~/projects/hill_farming/data/ag_census_2011.csv")
