# ----------------------------------------------------
# ----------------------------------------------------
# Read hill classification data
# ----------------------------------------------------
# ----------------------------------------------------

library(tidyverse)


# Urban rural
urban_rural = read_csv("~/hill_farming/data/SG_UrbanRural_2016_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName, UR8FOLD=a_UR8FOLD) %>% 
   group_by(PARCode, PARName, UR8FOLD) %>% 
   summarise(urban_rural_prop=sum(SG_UrbanRural_2016_area / b_Shape_Area))

# Wild land
wild_land = read_csv("~/hill_farming/data/WILDLAND_SCOTLAND_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(wildland_prop=sum(WILDLAND_SCOTLAND_area / b_Shape_Area))
   
# Land capability agriculture
# to remove urban values mutate(a_lcacode=replace(a_lcacode, which(a_lcacode > 7), NA))
land_capability_ag = read_csv("~/hill_farming/data/lca_250k_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName, lcacode=a_lcacode) %>% 
   group_by(PARCode, PARName, lcacode) %>% 
   summarise(land_cap_ag_prop=sum(lca_250k_area / b_Shape_Area))

# Land capability forestry
land_capability_forest = read_csv("~/hill_farming/data/lcf250k_dleas_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName, LANDCAP=a_LANDCAP) %>% 
   group_by(PARCode, PARName, LANDCAP) %>% 
   summarise(land_cap_forest_prop=sum(lcf250k_dleas_area / b_Shape_Area))

# SNH land classification
land_classes = read_csv("~/hill_farming/data/LCA_SCOTLAND_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName, Landscape_class_3=a_LEVEL_3) %>% 
   group_by(PARCode, PARName, Landscape_class_3) %>% 
   summarise(landscape_class_prop=sum(LCA_SCOTLAND_area / b_Shape_Area))

# Terrain
terrain = read_csv("~/hill_farming/data/terrain_parishes.csv") %>% 
   select(-cat, -Shape_Leng, -Shape_Area) %>% 
   drop_na() %>% 
   group_by(PARCode, PARName) %>% 
   summarise_all(mean)


# Join them all together
parish_restrictions = read_csv("hill_farming/data/ag_parishes.csv") %>% 
   left_join(terrain) %>% 
   left_join(land_classes) %>% 
   left_join(land_capability_ag) %>% 
   left_join(land_capability_forest) %>% 
   left_join(wild_land) %>% 
   left_join(urban_rural)
