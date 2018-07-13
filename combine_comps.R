# ----------------------------------------------------
# ----------------------------------------------------
# Combine comparisons
# ----------------------------------------------------
# ----------------------------------------------------

library(tidyverse)

# ----------------------------------------------------
# Census

pop_census = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/census/pop_census.csv")

ag_census_2000 = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/census/ag_census_2000.csv")

ag_census_2011 = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/census/ag_census_2011.csv")


# ----------------------------------------------------
# Landscapes


NNR = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/NNR_SCOTLAND_parishes.csv") %>% 
   mutate(NNR_prop = NNR_SCOTLAND_area / b_Shape_Area) %>% 
   select(PARCode=b_PARCode, NNR_prop) %>% 
   group_by(PARCode) %>% 
   summarise(NNR_prop=sum(NNR_prop))

sparse = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/RD341_PARCODE_SPAFLAG_SUBREGION_6_LOOKUP_SPAonly.csv") %>% 
   select(PARCode=AG_PARCODE, sparse_prop=PCT_PARISH_AREA)

SAC = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/SAC_SCOTLAND_parishes.csv") %>% 
   mutate(SAC_prop = SAC_SCOTLAND_area / b_Shape_Area) %>% 
   select(PARCode=b_PARCode, SAC_prop) %>% 
   group_by(PARCode) %>% 
   summarise(SAC_prop=sum(SAC_prop))

rural = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/SG_UrbanRural_2016_parishes.csv") %>% 
   filter(a_UR2FOLD==2) %>% 
   mutate(rural_prop = SG_UrbanRural_2016_area / b_Shape_Area) %>% 
   select(PARCode=b_PARCode, rural_prop) %>% 
   group_by(PARCode) %>% 
   summarise(rural_prop=sum(rural_prop))

SPA = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/SPA_SCOTLAND_parishes.csv") %>% 
   mutate(SPA_prop = SPA_SCOTLAND_area / b_Shape_Area) %>% 
   select(PARCode=b_PARCode, SPA_prop) %>% 
   group_by(PARCode) %>% 
   summarise(SPA_prop=sum(SPA_prop))

SSSI = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/SSSI_SCOTLAND_parishes.csv") %>% 
   mutate(SSSI_prop = SSSI_SCOTLAND_area / b_Shape_Area) %>% 
   select(PARCode=b_PARCode, SSSI_prop) %>% 
   group_by(PARCode) %>% 
   summarise(SSSI_prop=sum(SSSI_prop))

wildland = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/WILDLAND_SCOTLAND_parishes.csv") %>% 
   mutate(wildland_prop = WILDLAND_SCOTLAND_area / b_Shape_Area) %>% 
   select(PARCode=b_PARCode, wildland_prop) %>% 
   group_by(PARCode) %>% 
   summarise(wildland_prop=sum(wildland_prop))

landscapes = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/ag_parishes.csv") %>% 
   left_join(NNR) %>% 
   left_join(sparse) %>% 
   left_join(SAC) %>% 
   left_join(rural) %>% 
   left_join(SPA) %>% 
   left_join(SSSI) %>% 
   left_join(wildland)

landscapes[is.na(landscapes)] = 0

write_csv(landscapes, "~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/landscapes.csv")
