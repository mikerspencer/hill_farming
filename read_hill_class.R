# ----------------------------------------------------
# ----------------------------------------------------
# Read hill classification data
# ----------------------------------------------------
# ----------------------------------------------------

library(tidyverse)

# For all variables higher proportions are worse

   
# Land capability agriculture
# to remove urban values mutate(a_lcacode=replace(a_lcacode, which(a_lcacode > 7), NA))
land_capability_ag = read_csv("~/projects/hill_farming/data/lca_250k_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName, lcacode=a_lcacode) %>% 
   filter(lcacode>6 & lcacode<10) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(land_cap_ag_prop=sum(lca_250k_area / b_Shape_Area))

# SNH land classification
land_classes = read_csv("~/projects/hill_farming/data/LCA_SCOTLAND_parishes.csv") %>% 
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

# Peat/carbon map
Peat = read_csv("~/projects/hill_farming/data/PEAT_SCOTLAND_parishes.csv") %>% 
   rename(PARCode=b_PARCode, PARName=b_PARName) %>% 
   filter(a_IMPORTANCE %in% c(1, 2, 3, 5)) %>% 
   group_by(PARCode, PARName) %>% 
   summarise(peat_prop=sum(PEAT_SCOTLAND_area / b_Shape_Area))

# Terrain
terrain = read_csv("~/projects/hill_farming/data/terrain_parishes.csv") %>% 
   select(cat, PARCode, PARName, Shape_Area, elev__average, slope__average, area_part) %>% 
   group_by(PARCode, PARName) %>% 
   mutate(area_part = area_part/Shape_Area,
          elev_mean = elev__average * area_part,
          slope_mean = slope__average * area_part) %>% 
   summarise(elev_mean = sum(elev_mean, na.rm=T),
             slope_mean = sum(slope_mean, na.rm=T))

# Common grazing
# Note there is likely an issue with the reported area of common grazing as 1 parish is greater than 100 %.
c_grazing = read_csv("~/projects/hill_farming/data/common_grazing.csv") %>% 
   left_join(read_csv("~/projects/hill_farming/data/LCA_SCOTLAND_parishes.csv"), by=c("PARCode"="b_PARCode")) %>% 
   group_by(PARCode) %>% 
   summarise(c_grazing_prop=unique(common_grazing * 10000) / unique(b_Shape_Area)) %>% 
   mutate(c_grazing_prop=replace(c_grazing_prop, c_grazing_prop>1, 1))

# Rough grazing
# Note there is likely an issue with the reported area of common grazing as 1 parish is greater than 100 %.
r_grazing = read_csv("~/projects/hill_farming/data/rough_grazing.csv") %>% 
   left_join(read_csv("~/projects/hill_farming/data/LCA_SCOTLAND_parishes.csv"), by=c("PARISH"="b_PARCode")) %>% 
   group_by(PARISH) %>% 
   summarise(r_grazing_prop=unique(ITEM47 * 10000) / unique(b_Shape_Area)) %>% 
   mutate(r_grazing_prop=replace(r_grazing_prop, r_grazing_prop>1, 1)) %>% 
   rename(PARCode=PARISH)

# Join them all together
parish_restrictions = read_csv("~/projects/hill_farming/data/ag_parishes.csv") %>% 
   distinct() %>% 
   left_join(terrain) %>% 
   left_join(land_classes) %>% 
   left_join(land_capability_ag) %>% 
   left_join(Peat) %>% 
   left_join(c_grazing) %>% 
   left_join(r_grazing) %>% 
   write_csv("~/projects/hill_farming/parish_restrictions.csv")
