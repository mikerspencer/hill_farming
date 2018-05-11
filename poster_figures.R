# --------------------------
# --------------------------
# Hill farms
# Poster figures
# --------------------------
# --------------------------

library(rgdal)
library(broom)
library(tidyverse)
library(viridis)

parishes = readOGR(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial/ag_parishes_2016.gpkg"), "simplified_parishes")

parishes$id = row.names(parishes)
parishes = tidy(parishes) %>% 
   left_join(parishes@data)

wild_land = readOGR(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial/"), "WILDLAND_SCOTLAND")

restrictions = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/parish_restrictions.csv") %>% 
   mutate_all(funs(replace(., is.na(.), 0)))

parishes = parishes %>% 
   left_join(restrictions, by=c("PARCode"="PARCode", "PARName"="PARName")) %>% 
   mutate_all(funs(replace(., is.na(.), 0)))


# --------------------------
# Clipping

pdf("~/Cloud/Michael/SRUC/hill_farms/poster/images/explanation_parish.pdf", width=5)
ggplot(parishes, aes(long, lat, group=group)) +
   geom_polygon(colour="grey10", fill=rgb(0/255, 156/255, 222/255), size=0.1) +
   coord_equal() +
   theme_minimal() +
   theme(axis.text=element_blank(),
         axis.title=element_blank(),
         line=element_blank())
dev.off()

wild_land = tidy(wild_land)

pdf("~/Cloud/Michael/SRUC/hill_farms/poster/images/explanation_wild.pdf", width=5)
ggplot(parishes, aes(long, lat, group=group)) +
   geom_polygon(colour="grey10", fill=rgb(0, 156/255, 222/255), size=0.1) +
   geom_polygon(data=wild_land,
                aes(long, lat, group=group),
                colour=rgb(150/255, 0, 81/255),
                fill=rgb(150/255, 0, 81/255, 0.75)) +
   coord_equal() +
   theme_minimal() +
   theme(axis.text=element_blank(),
         axis.title=element_blank(),
         line=element_blank())
dev.off()

pdf("~/Cloud/Michael/SRUC/hill_farms/poster/images/explanation_parish-wild.pdf", width=5)
parishes %>% 
   ggplot(aes(long, lat, fill=wildland_prop, group=group)) +
   geom_polygon() +
   coord_equal() +
   scale_fill_viridis() +
   labs(fill="Wild land\nProportion") +
   theme_minimal() +
   theme(axis.text=element_blank(),
         axis.title=element_blank(),
         line=element_blank())
dev.off()


# --------------------------
# Combining

x = data.frame(cols=colnames(restrictions)[c(6, 10:17)],
               names=c("Elevation", "Slope", "SNH\nLandscape",
                       "JHI\nagri\ncapability", "JHI\nforest\ncapability",
                       "SNH\nwildland", "Rural-urban", "NNR", "SNH peat"), stringsAsFactors=F)

lapply(1:nrow(x), function(i){
   pdf(paste0("~/Cloud/Michael/SRUC/hill_farms/poster/images/", x[i, 1], ".pdf"), width=5)
   print(
      ggplot(parishes, aes_string("long", "lat", fill=x[i, 1], group="group")) +
      geom_polygon() +
      coord_equal() +
      scale_fill_viridis() +
      labs(fill=x[i, 2]) +
      theme_minimal() +
      theme(axis.text=element_blank(),
            axis.title=element_blank(),
            line=element_blank())
   )
   dev.off()
})


# --------------------------
# Score

score = restrictions %>% 
   select(-PARName,
          -elev__minimum, -elev__average, -elev__maximum,
          -slope__minimum, -slope__average, -slope__maximum,
          -SPA_prop, -common_grazing) %>% 
   mutate(elev__median=elev__median/max(elev__median),
          slope__median=slope__median/max(slope__median)) %>% 
   gather(variable, value, -PARCode) %>% 
   group_by(PARCode) %>% 
   summarise(score=sum(value))

parishes = parishes %>% 
   left_join(score)

pdf("~/Cloud/Michael/SRUC/hill_farms/poster/images/score.pdf", width=5)
parishes %>% 
   ggplot(aes(long, lat, fill=score, group=group)) +
   geom_polygon() +
   coord_equal() +
   scale_fill_viridis(option="magma") +
   labs(title="Combined constraints score",
        fill="Hill\nscore") +
   theme_minimal() +
   theme(axis.text=element_blank(),
         axis.title=element_blank(),
         line=element_blank())
dev.off()


# --------------------------
# Population

pdf("~/Cloud/Michael/SRUC/hill_farms/poster/images/pop_census.pdf")
read_csv("~/Cloud/Michael/SRUC/hill_farms/data/census/pop_census.csv") %>% 
   mutate(prop_2011_land=pop_working_land_2011 / pop_working_all_2011) %>% 
   select(PARCode, prop_2011_land) %>% 
   left_join(score) %>% 
   ggplot(aes(score, prop_2011_land)) +
   geom_point() +
   theme_bw() +
   theme(line=element_blank(),
         text=element_text(size=20)) +
   labs(title="Population census and hill farming",
        subtitle="Each point represents an agricultural parish",
        x="Hill farming score",
        y="Proportion of people employed\nin land based industry")
dev.off()
