# ----------------------------------------------------
# ----------------------------------------------------
# Read census data
# ----------------------------------------------------
# ----------------------------------------------------

library(tidyverse)

# 2001
# population
KS01 = read_csv("projects/hill_farming/data/KS01.csv", skip=4)
# age
KS02 = read_csv("projects/hill_farming/data/KS02.csv", skip=5)
# employment
KS11a = read_csv("projects/hill_farming/data/KS11a.csv", skip=5)

# 2011
# population
KS101SC = read_csv("projects/hill_farming/data/KS101SC.csv", skip=4)
KS102SC = read_csv("projects/hill_farming/data/KS101SC.csv", skip=4)

KS605SC = read_csv("projects/hill_farming/data/KS605SC.csv", skip=4)


# Which columns are we interested in?
census_2001 = KS01 %>% 
   inner_join(KS11a) %>% 
   select(TAG=X1,
          pop_all_2001=`2001 Population: All people`,
          pop_male_2001=`2001 Population: Males`,
          pop_female_2001=`2001 Population: Females`,
          pop_working_all_2001=`All people aged 16 to 74 in employment`,
          pop_working_ag_2001=`Percentage of people aged 16 to 74 working in: Agriculture, hunting and forestry`,
          pop_working_fish_2001= `Percentage of people aged 16 to 74 working in: Fishing`) %>% 
   mutate(pop_working_ag_2001=as.numeric(str_replace(pop_working_ag_2001, "-", "0")),
          pop_working_fish_2001=as.numeric(str_replace(pop_working_fish_2001, "-", "0"))) %>% 
   mutate(pop_working_land_2001=pop_working_ag_2001 + pop_working_fish_2001) %>% 
   select(-pop_working_ag_2001, -pop_working_fish_2001)

census_2011 = KS101SC %>% 
   inner_join(KS605SC) %>% 
   select(census_id=X1,
          pop_all_2011=`All people`,
          pop_male_2011=Males,
          pop_female_2011=Females,
          pop_working_all_2011=`All people aged 16 to 74 in employment`,
          pop_working_land_2011=`A. Agriculture, forestry and fishing`) %>% 
   mutate(pop_working_all_2011=gsub(",", "", pop_working_all_2011),
          pop_working_land_2011=gsub(",", "", pop_working_land_2011)) %>% 
   mutate(pop_working_all_2011=as.numeric(str_replace(pop_working_all_2011, "-", "0")),
          pop_working_land_2011=as.numeric(str_replace(pop_working_land_2011, "-", "0")))


# Get proportion according to parishes
areas_2001 = read_csv("projects/hill_farming/data/OutputAreas2001_parishes.csv") %>% 
   select(TAG=a_TAG, PARCode=b_PARCode, PARName=b_PARName, OutputAreas2001_area, a_OutputAreas2001_t_area) %>% 
   mutate(area_prop=OutputAreas2001_area / a_OutputAreas2001_t_area) %>% 
   left_join(census_2001) %>% 
   mutate(pop_all_2001=ceiling(pop_all_2001 * area_prop),
          pop_male_2001=ceiling(pop_male_2001 * area_prop),
          pop_female_2001=ceiling(pop_female_2001 * area_prop),
          pop_working_all_2001=ceiling(pop_working_all_2001 * area_prop),
          pop_working_land_2001=ceiling(pop_working_land_2001 * area_prop)) %>% 
   select(-OutputAreas2001_area, -a_OutputAreas2001_t_area, -area_prop, -TAG) %>% 
   group_by(PARCode, PARName) %>% 
   summarise_all(sum)

# Joining to polygons removes Scotland row from census
areas_2011 = read_csv("projects/hill_farming/data/OutputArea2011_MHW_parishes.csv") %>% 
   select(census_id=a_code, PARCode=b_PARCode, PARName=b_PARName, OutputArea2011_MHW_area, a_OutputArea2011_MHW_t_area) %>% 
   mutate(area_prop=OutputArea2011_MHW_area / a_OutputArea2011_MHW_t_area) %>% 
   left_join(census_2011) %>% 
   mutate(pop_all_2011=ceiling(pop_all_2011 * area_prop),
          pop_male_2011=ceiling(pop_male_2011 * area_prop),
          pop_female_2011=ceiling(pop_female_2011 * area_prop),
          pop_working_all_2011=ceiling(pop_working_all_2011 * area_prop),
          pop_working_land_2011=ceiling(pop_working_land_2011 * area_prop)) %>% 
   select(-OutputArea2011_MHW_area, -a_OutputArea2011_MHW_t_area, -area_prop, -census_id) %>% 
   group_by(PARCode, PARName) %>% 
   summarise_all(sum)

pop_census = areas_2001 %>% 
   inner_join(areas_2011)

write_csv(pop_census, "projects/hill_farming/pop_census.csv")
