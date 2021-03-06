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
library(patchwork)
library(RColorBrewer)


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

plot.map = function(i, tit, pal){
   ggplot(parishes, aes(long, lat, group=group)) +
      geom_polygon(aes_string(fill=i)) +
      geom_polygon(data=Scotland, aes(long, lat, group=group),
                   colour="grey30", fill=NA, size=0.1) +
      coord_equal() +
      scale_fill_distiller(palette=pal, direction=1) +
      labs(fill=tit) +
      theme_minimal() +
      theme(axis.text=element_blank(),
            axis.title=element_blank(),
            line=element_blank(),
            text=element_text(size=25))
}


# ---------------------------------------------
# Terrain50

# Elevation

elev = readGDAL(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/elev.tif")) %>% 
   as.data.frame()

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/elev.png",
    height=1080, width=1600)
ggplot(elev, aes(x, y, fill=band1)) +
   geom_raster() +
   geom_polygon(data=Scotland, aes(long, lat, group=group),
                colour="grey30", fill=NA, size=0.1) +
   coord_equal() +
   scale_fill_distiller(palette="Oranges", direction=1) +
   labs(fill="Elevation\n(metres)") +
   theme_minimal() +
   theme(axis.text=element_blank(),
         axis.title=element_blank(),
         line=element_blank(),
         text=element_text(size=25)) +
   plot.map("elev_mean", "Elevation\nmean (m)", "Oranges")
dev.off()

# Slope

slope = readGDAL(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/slope.tif")) %>% 
   as.data.frame()

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/slope.png",
    height=1080, width=1600)
ggplot(slope, aes(x, y, fill=band1)) +
   geom_raster() +
   geom_polygon(data=Scotland, aes(long, lat, group=group),
                colour="grey30", fill=NA, size=0.1) +
   coord_equal() +
   scale_fill_distiller(palette="RdPu", direction=1) +
   labs(fill="Slope\n(degrees)") +
   theme_minimal() +
   theme(axis.text=element_blank(),
         axis.title=element_blank(),
         line=element_blank(),
         text=element_text(size=25)) +
   plot.map("slope_mean", "Slope\nmean\n(degrees)", "RdPu")
dev.off()


# ---------------------------------------------
# Common grazing

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/grazing.png",
    height=1080, width=1600)
plot.map("c_grazing_prop", "Common\ngrazing\nproportion", "Greens") +
   plot.map("r_grazing_prop", "Rough\ngrazing\nproportion", "Greens")
dev.off()


# ---------------------------------------------
# Landscape character (SNH)

lca = readOGR(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial"), "LCA_SCOTLAND")

y = c("Flat or Rolling, Smooth or Sweeping, Extensive, High Moorlands of the Highlands and Islands",
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
      "Highland Cnocan")

lca$hill[lca$LEVEL_3 %in% y] = "Hill"
lca$hill[!lca$LEVEL_3 %in% y] = "Other"

lca$id = row.names(lca)
lca = tidy(lca) %>% 
   left_join(lca@data)

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/landscape.png",
    height=1080, width=1600)
ggplot(lca, aes(long, lat, group=group)) +
   geom_polygon(aes(fill=hill)) +
   geom_polygon(data=Scotland, aes(long, lat, group=group),
                colour="grey30", fill=NA, size=0.1) +
   coord_equal() +
   scale_fill_manual(values=brewer.pal(9, "Purples")[c(8, 3)]) +
   labs(fill="Landscape\nclass") +
   theme_minimal() +
   theme(axis.text=element_blank(),
         axis.title=element_blank(),
         line=element_blank(),
         text=element_text(size=25)) +
   plot.map("landscape_class_prop",
            "Landscape\nclass\nproportion",
            "Purples")
dev.off()


# ---------------------------------------------
# Carbon and peat (SNH)

lca$soil[lca$LEVEL_3 %in% y] = "Peat"
lca$soil[!lca$LEVEL_3 %in% y] = "Other"

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/peat_frame.png",
    height=1080, width=1600)
ggplot(lca, aes(long, lat, group=group)) +
   geom_polygon(aes(fill=soil)) +
   geom_polygon(data=Scotland, aes(long, lat, group=group),
                colour="grey30", fill=NA, size=0.1) +
   coord_equal() +
   scale_fill_manual(values=brewer.pal(9, "Reds")[c(3, 8)]) +
   labs(fill="Soil") +
   theme_minimal() +
   theme(axis.text=element_blank(),
         axis.title=element_blank(),
         line=element_blank(),
         text=element_text(size=25)) +
   plot.map("peat_prop",
            "Peatland\nproportion",
            "Reds")
dev.off()


# ---------------------------------------------
# Land capability (JHI)

lca = readOGR(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial"), "lca_250k")

lca$id = row.names(lca)
lca = tidy(lca) %>% 
   left_join(lca@data)

lca$hill[lca$lcacode %in% c(6.3, 7.0, 6.2, 6.1)] = "Upland"
lca$hill[!lca$lcacode %in% c(6.3, 7.0, 6.2, 6.1)] = "Other"

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/lcagri.png",
    height=1080, width=1600)
ggplot(lca, aes(long, lat, group=group)) +
   geom_polygon(aes(fill=hill)) +
   geom_polygon(data=Scotland, aes(long, lat, group=group),
                colour="grey30", fill=NA, size=0.1) +
   coord_equal() +
   scale_fill_manual(values=brewer.pal(9, "Blues")[c(8, 3)]) +
   labs(fill="Agricultural\ncapability") +
   theme_minimal() +
   theme(axis.text=element_blank(),
         axis.title=element_blank(),
         line=element_blank(),
         text=element_text(size=25)) +
   plot.map("land_cap_ag_prop",
            "Upland\nagricultural\ncapability\nproportion",
            "Blues")
dev.off()

