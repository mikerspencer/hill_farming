# ----------------------------------------------------
# ----------------------------------------------------
# Read census data
# ----------------------------------------------------
# ----------------------------------------------------

library(tidyverse)

# 2001
# population
KS01 = read_csv("hill_farming/data/KS01.csv", skip=4)
# age
KS02 = read_csv("hill_farming/data/KS02.csv", skip=5)
# employment
KS11a = read_csv("hill_farming/data/KS11a.csv", skip=5)

# 2011
# population
KS101SC = read_csv("hill_farming/data/KS101SC.csv", skip=4)
KS102SC = read_csv("hill_farming/data/KS101SC.csv", skip=4)

KS605SC = read_csv("hill_farming/data/KS605SC.csv", skip=4)


# Which columns are we interested in?
census_2001 = KS01 %>% 
   inner_join(KS11a) %>% 
   select(census_id=X1,
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
          pop_working_land_2011=`A. Agriculture, forestry and fishing`)


# Get proportion according to parishes
areas_total_2001 = read_csv("hill_farming/data/OutputAreas2001_temp.csv")
areas_2001 = read_csv("hill_farming/data/OutputAreas2001_parishes.csv") %>% 
   select(-cat) %>% 
   left_join(areas_total_2001, by=c("a_TAG"="TAG"))

areas_2001 = areas_2001[!duplicated(areas_2001), ]




areas_2011 = read_csv("hill_farming/data/OutputArea2011_MHW_parishes.csv")
