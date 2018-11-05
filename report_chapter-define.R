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
            line=element_blank(),
            text=element_text(size=25))
}


# ---------------------------------------------
# Terrain50

elev = readGDAL(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/elev.tif")) %>% 
   tidy()

slope = readGDAL(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/slope.tif")) %>% 
   as.data.frame()

# Elevation

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/elev.png",
    height=1080, width=800)
ggplot(elev, aes(x, y, fill=band1)) +
   geom_raster() +
   geom_polygon(data=Scotland, aes(long, lat, group=group),
                colour="grey30", fill=NA, size=0.1) +
   coord_equal() +
   scale_fill_distiller(palette="Greens", direction=1) +
   labs(fill="Elevation\n(metres)") +
   theme_minimal() +
   theme(axis.text=element_blank(),
         axis.title=element_blank(),
         line=element_blank(),
         text=element_text(size=25))
dev.off()

# Slope
png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/slope.png",
    height=1080, width=800)
ggplot(slope, aes(x, y, fill=band1)) +
   geom_raster() +
   geom_polygon(data=Scotland, aes(long, lat, group=group),
                colour="grey30", fill=NA, size=0.1) +
   coord_equal() +
   scale_fill_distiller(palette="Greens", direction=1) +
   labs(fill="Slope\n(degrees)") +
   theme_minimal() +
   theme(axis.text=element_blank(),
         axis.title=element_blank(),
         line=element_blank(),
         text=element_text(size=25))
dev.off()


# ---------------------------------------------
# Common grazing

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/common_grazing.png",
    height=1080, width=800)
plot.map("c_grazing_prop", "Common\ngrazing\nproportion")
dev.off()


# ---------------------------------------------
# Rough grazing

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/rough_grazing.png",
    height=1080, width=800)
plot.map("r_grazing_prop", "Rough\ngrazing\nproportion")
dev.off()

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

