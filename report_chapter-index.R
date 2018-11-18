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

restrictions = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/parish_restrictions.csv")
hilliness = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/hilliness.csv") %>% 
   mutate(rank = cut_number(score, n=5,
                            labels=c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")))

designations = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/designations.csv")

parishes = readOGR(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial/ag_parishes_2016.gpkg"), "simplified_parishes")

Scotland = readOGR(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial/ag_parishes_2016.gpkg"), "Scotland") %>% 
   tidy()

parishes$id = row.names(parishes)
parishes = tidy(parishes) %>% 
   left_join(parishes@data)

parishes = parishes %>% 
   left_join(hilliness, by=c("PARCode"="PARCode")) %>% 
   left_join(designations, by=c("PARCode"="PARCode"))


# ---------------------------------------------
# Score and rank

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


# ---------------------------------------------
# Top 10

hilliness %>% 
   left_join(restrictions) %>% 
   select(PARName, score) %>% 
   arrange(desc(score)) %>% 
   top_n(10, wt=score) %>% 
   xtable()


# ---------------------------------------------
# Designation comparison

score.designation = hilliness %>% 
   left_join(designations, by=c("PARCode"="PARCode"))

plot.designation = function(i, tit, tit.x, pal){
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
            text=element_text(size=25)) +
      ggplot(score.designation, aes_string("score", i)) +
      geom_point(size=5, alpha=0.5) +
      labs(x="Hilliness score",
           y=tit.x) +
      theme_bw() +
      theme(text=element_text(size=25))
}

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/desig_forest.png",
    height=1080, width=1600)
plot.designation("land_cap_forest_prop", "Constrained\nforestry\nproportion", "Constrained forestry proportion", "BuGn")
dev.off()

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/desig_wildland.png",
    height=1080, width=1600)
plot.designation("wildland_prop", "Wildland\nproportion", "Wildland proportion", "GnBu")
dev.off()

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/desig_rural.png",
    height=1080, width=1600)
plot.designation("rural_prop", "Rural\nproportion", "Rural proportion", "PuBuGn")
dev.off()

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/desig_NNR.png",
    height=1080, width=1600)
plot.designation("NNR_prop", "National\nNature\nReserve\nproportion", "National Nature Reserve proportion", "YlOrRd")
dev.off()

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/desig_SPA.png",
    height=1080, width=1600)
plot.designation("SPA_prop", "Special\nProtection\nArea\nproportion", "Special Protection Area proportion", "YlGnBu")
dev.off()
