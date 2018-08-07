# ----------------------------------------------------
# ----------------------------------------------------
# Grouping landscape designations
# ----------------------------------------------------
# ----------------------------------------------------

library(tidyverse)

# For all variables higher proportions are worse/more of them

   
# Urban rural
urban_rural = read_csv("~/projects/hill_farming/data/SG_UrbanRural_2016_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName, UR2FOLD=a_UR2FOLD) %>% 
   filter(UR2FOLD==2) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(rural_prop=sum(SG_UrbanRural_2016_area / b_Shape_Area))

# Wild land
wild_land = read_csv("~/projects/hill_farming/data/WILDLAND_SCOTLAND_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(wildland_prop=sum(WILDLAND_SCOTLAND_area / b_Shape_Area))

# Land capability forestry
land_capability_forest = read_csv("~/projects/hill_farming/data/lcf250k_dleas_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName, LANDCAP=a_LANDCAP) %>% 
   filter(LANDCAP=="F6" | LANDCAP=="F7") %>% 
   group_by(PARCode, PARName) %>% 
   summarise(land_cap_forest_prop=sum(lcf250k_dleas_area / b_Shape_Area))

# National nature reserves
NNR = read_csv("~/projects/hill_farming/data/NNR_SCOTLAND_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(NNR_prop=sum(NNR_SCOTLAND_area / b_Shape_Area))

# Special area of conservation
SAC = read_csv("~/projects/hill_farming/data/SAC_SCOTLAND_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(SAC_prop=sum(SAC_SCOTLAND_area / b_Shape_Area))
# Some parishes exceed 1, likely due to overlapping polygons

# Special protection area
SPA = read_csv("~/projects/hill_farming/data/SPA_SCOTLAND_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(SPA_prop=sum(SPA_SCOTLAND_area / b_Shape_Area))

# SSSI
SSSI = read_csv("~/projects/hill_farming/data/SSSI_SCOTLAND_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(SSSI_prop=sum(SSSI_SCOTLAND_area / b_Shape_Area))
# Some parishes exceed 1, likely due to overlapping polygons



# Join them all together
lanscape_des = read_csv("~/projects/hill_farming/data/ag_parishes.csv") %>% 
   distinct() %>% 
   left_join(land_capability_forest) %>% 
   left_join(wild_land) %>% 
   left_join(urban_rural) %>% 
   left_join(NNR) %>% 
   left_join(SPA) %>% 
   write_csv("~/projects/hill_farming/designations.csv")
