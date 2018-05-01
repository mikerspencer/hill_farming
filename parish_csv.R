# ------------------------------
# ------------------------------
# Parish as csv
# ------------------------------
# ------------------------------

library(rgdal)
library(tidyverse)

parishes = readOGR(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial/ag_parishes_2016.gpkg"), "simplified_parishes")

parishes@data %>% 
   select(PARCode, PARName) %>% 
   write_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/ag_parishes.csv")
