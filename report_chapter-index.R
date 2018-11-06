# ---------------------------------------------
# ---------------------------------------------
# Figures Chapter: Index
# ---------------------------------------------
# ---------------------------------------------

library(tidyverse)
library(rgdal)
library(broom)
library(patchwork)
library(RColorBrewer)
library(viridis)


# ---------------------------------------------
# Data

hilliness = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/hilliness.csv")

parishes = readOGR(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial/ag_parishes_2016.gpkg"), "simplified_parishes")

Scotland = readOGR(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial/ag_parishes_2016.gpkg"), "Scotland") %>% 
   tidy()

parishes$id = row.names(parishes)
parishes = tidy(parishes) %>% 
   left_join(parishes@data)

parishes = parishes %>% 
   left_join(hilliness, by=c("PARCode"="PARCode"))


# ---------------------------------------------
# Score and rank

parishes$rank = cut(parishes$score,
                    breaks=5,
                    labels=c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"))

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/score-rank.png",
    height=1080, width=1600)
ggplot(parishes, aes(long, lat, fill=score, group=group)) +
   geom_polygon() +
   geom_polygon(data=Scotland, aes(long, lat, group=group),
                colour="grey30", fill=NA, size=0.1) +
   coord_equal() +
   scale_fill_distiller(palette="YlOrBr", direction=1) +
   labs(fill="Hilliness\nscore") +
   theme_minimal() +
   theme(axis.text=element_blank(),
         axis.title=element_blank(),
         line=element_blank(),
         text=element_text(size=25)) +
   ggplot(parishes, aes(long, lat, fill=rank, group=group)) +
   geom_polygon() +
   geom_polygon(data=Scotland, aes(long, lat, group=group),
                colour="grey30", fill=NA, size=0.1) +
   coord_equal() +
   scale_fill_brewer(palette="YlOrBr") +
   labs(fill="Hilliness\nquintiles") +
   theme_minimal() +
   theme(axis.text=element_blank(),
         axis.title=element_blank(),
         line=element_blank(),
         text=element_text(size=25))
dev.off()
