# ----------------------------------------------------
# ----------------------------------------------------
# Read hill classification data
# ----------------------------------------------------
# ----------------------------------------------------

library(tidyverse)

# For all variables higher proportions are worse

# Urban rural
urban_rural = read_csv("~/hill_farming/data/SG_UrbanRural_2016_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName, UR2FOLD=a_UR2FOLD) %>% 
   filter(UR2FOLD==2) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(rural_prop=sum(SG_UrbanRural_2016_area / b_Shape_Area))

# Wild land
wild_land = read_csv("~/hill_farming/data/WILDLAND_SCOTLAND_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(wildland_prop=sum(WILDLAND_SCOTLAND_area / b_Shape_Area))
   
# Land capability agriculture
# to remove urban values mutate(a_lcacode=replace(a_lcacode, which(a_lcacode > 7), NA))
land_capability_ag = read_csv("~/hill_farming/data/lca_250k_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName, lcacode=a_lcacode) %>% 
   filter(lcacode>6 & lcacode<10) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(land_cap_ag_prop=sum(lca_250k_area / b_Shape_Area))

# Land capability forestry
land_capability_forest = read_csv("~/hill_farming/data/lcf250k_dleas_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName, LANDCAP=a_LANDCAP) %>% 
   filter(LANDCAP=="F6" | LANDCAP=="F7") %>% 
   group_by(PARCode, PARName) %>% 
   summarise(land_cap_forest_prop=sum(lcf250k_dleas_area / b_Shape_Area))

# SNH land classification
land_classes = read_csv("~/hill_farming/data/LCA_SCOTLAND_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName) %>% 
   filter(a_LEVEL_3 %in% c("Flat or Rolling, Smooth or Sweeping, Extensive, High Moorlands of the Highlands and Islands",
                           "Inland Loch",
                           "Highland Straths",
                           "Moorland Transitional Landscapes of the Highlands and Islands",
                           "High, Massive, Rolling, Rounded Mountains of the Highlands and Islands",
                           "High Massive Mountain Plateau of the Cairngorms",
                           "Smooth Upland Moorland Hills",
                           "Highland Foothills",
                           "Upland Igneous and Volcanic Hills The Ochil, Sidlaw, Cleish and Lomond Hills",
                           "High, Massive, Rugged, Steep-Sided Mountains of the Highlands and Islands",
                           "Sea Lochs of the Highlands and Islands",
                           "Highland and Island Rocky Coastal Landscapes",
                           "Peatland Landscapes  of the Highlands and Islands",
                           "Rocky Moorlands of the Highlands and Islands",
                           "Rugged, Craggy Upland Hills and Moorlands of the Highlands, including the Trossachs",
                           "Low Coastal Hills of the Highlands and Islands",
                           "Coastal Hills Headlands Plateaux and Moorlands",
                           "Lowland Hills",
                           "Foothills and Pronounced Hills",
                           "Upland Hills, The Southern Uplands and Cheviots",
                           "High Plateau Moorlands",
                           "Rugged Granite Uplands",
                           "Rocky Volcanic Islands",
                           "Rugged Moorland Hills",
                           "Upland Fringe Moorland",
                           "Upland Hills, The Lammemuir, Pentland and Moorfoot Hills",
                           "Rocky Coasts Cliffs and Braes of the Lowlands",
                           "Knock or Rock and Lochan of the Islands",
                           "Highland Cnocan")) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(landscape_class_prop=sum(LCA_SCOTLAND_area / b_Shape_Area))

# National nature reserves
NNR = read_csv("~/hill_farming/data/NNR_SCOTLAND_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(NNR_prop=sum(NNR_SCOTLAND_area / b_Shape_Area))

# Peat/carbon map
Peat = read_csv("~/hill_farming/data/PEAT_SCOTLAND_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName) %>% 
   filter(a_PRIMARY_LA %in% c("Blanket bog/peat. veg.", "Water", "Montane veg.", "Cliffs", "Dubh lochans", "Undiff. heather moor", "Other peat", "Wet heather moor", "Dry heather moor", "Quarries", "Wetlands", "Dune lands", "Bings (area)", "Smooth grass/rushes", "Undiff. Nardus/Molinia", "Snow cover", "Industrial peat", "Ski tows", "Rhododendron")) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(peat_prop=sum(PEAT_SCOTLAND_area / b_Shape_Area))

# Special area of conservation
SAC = read_csv("~/hill_farming/data/SAC_SCOTLAND_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(SAC_prop=sum(SAC_SCOTLAND_area / b_Shape_Area))
# Some parishes exceed 1, likely due to overlapping polygons

# Special protection area
SPA = read_csv("~/hill_farming/data/SPA_SCOTLAND_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(SPA_prop=sum(SPA_SCOTLAND_area / b_Shape_Area))

# SSSI
SSSI = read_csv("~/hill_farming/data/SSSI_SCOTLAND_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(SSSI_prop=sum(SSSI_SCOTLAND_area / b_Shape_Area))
# Some parishes exceed 1, likely due to overlapping polygons

# Terrain
terrain = read_csv("~/hill_farming/data/terrain_parishes.csv") %>% 
   select(-cat, -Shape_Leng, -Shape_Area) %>% 
   drop_na() %>% 
   group_by(PARCode, PARName) %>% 
   summarise_all(mean)


# Join them all together
parish_restrictions = read_csv("hill_farming/data/ag_parishes.csv") %>% 
   distinct() %>% 
   left_join(terrain) %>% 
   left_join(land_classes) %>% 
   left_join(land_capability_ag) %>% 
   left_join(land_capability_forest) %>% 
   left_join(wild_land) %>% 
   left_join(urban_rural) %>% 
   left_join(NNR) %>% 
   left_join(Peat) %>% 
   left_join(SPA) %>% 
   write_csv("hill_farming/parish_restrictions.csv")
