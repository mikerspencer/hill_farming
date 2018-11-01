# ---------------------------------------------
# ---------------------------------------------
# Figures for Chapter: define hill farming
# ---------------------------------------------
# ---------------------------------------------

# Maps of raw data
# Histograms?

library(tidyverse)
library(rgdal)
library(broom)


# ---------------------------------------------
# Data

parishes = readOGR(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial/ag_parishes_2016.gpkg"), "simplified_parishes")

Scotland = readOGR(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial/ag_parishes_2016.gpkg"), "Scotland") %>% 
   tidy()

parishes$id = row.names(parishes)
parishes = tidy(parishes) %>% 
   left_join(parishes@data)
   
restrictions = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/parish_restrictions.csv") %>% 
   mutate_all(funs(replace(., is.na(.), 0))) %>% 
   mutate(peat_prop=as.numeric(peat_prop))

parishes = parishes %>% 
   left_join(restrictions, by=c("PARCode"="PARCode", "PARName"="PARName")) %>% 
   mutate_all(funs(replace(., is.na(.), 0)))


# ---------------------------------------------
# Functions

plot.map = function(i, tit){
   ggplot(parishes, aes(long, lat, group=group)) +
      geom_polygon(aes_string(fill=i)) +
      geom_polygon(data=Scotland, aes(long, lat, group=group),
                   colour="grey30", fill=NA, size=0.1) +
      coord_equal() +
      scale_fill_distiller(palette="Greens", direction=1) +
      labs(fill=tit) +
      theme_minimal() +
      theme(axis.text=element_blank(),
            axis.title=element_blank(),
            line=element_blank())
}


# ---------------------------------------------
# Terrain50

# Elevation


# Slope


# ---------------------------------------------
# Common grazing

plot.map("c_grazing_prop", "Common\ngrazing\nproportion")



# ---------------------------------------------
# Rough grazing

plot.map("r_grazing_prop", "Rough\ngrazing\nproportion")


# ---------------------------------------------
# Landscape character (SNH)


# ---------------------------------------------
# Carbon and peat (SNH)


# ---------------------------------------------
# Land capability (JHI)


# ---------------------------------------------
# Parish elevation and slope


# ---------------------------------------------
# Parish landscape, carbon and land capability

