# --------------------------
# --------------------------
# Hill farms
# D&G and Borders plot
# --------------------------
# --------------------------

library(rgdal)
library(broom)
library(tidyverse)
library(RColorBrewer)

parishes = readOGR(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial/ag_parishes_2016.gpkg"), "simplified_parishes")

parishes$id = row.names(parishes)
parishes = tidy(parishes) %>% 
   left_join(parishes@data)

restrictions = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/parish_restrictions.csv") %>% 
   mutate_all(funs(replace(., is.na(.), 0)))

parishes = parishes %>% 
   left_join(restrictions, by=c("PARCode"="PARCode", "PARName"="PARName")) %>% 
   mutate_all(funs(replace(., is.na(.), 0)))


# --------------------------
# Score

score = restrictions %>% 
   select(-PARName,
          -elev__minimum, -elev__average, -elev__maximum,
          -slope__minimum, -slope__average, -slope__maximum,
          -SPA_prop, -grazing_prop) %>% 
   mutate(elev__median=elev__median/max(elev__median),
          slope__median=slope__median/max(slope__median)) %>% 
   gather(variable, value, -PARCode) %>% 
   group_by(PARCode) %>% 
   summarise(score=sum(value))

parishes = parishes %>% 
   left_join(score)


# --------------------------
# Population


x = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/census/pop_census.csv") %>% 
   mutate(prop_2011_land=pop_working_land_2011 / pop_working_all_2011) %>% 
   select(PARCode, prop_2011_land) %>% 
   left_join(score)

y = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial/LA_lookup.csv") %>% 
   filter(AUTHORITY=="Dumfries and Galloway" | AUTHORITY=="Scottish Borders") %>% 
   left_join(x)

png("~/Cloud/Michael/SRUC/hill_farms/figures/pop_census_southern-uplands.png", width=1700, height=900, res=150)
ggplot(y, aes(score, prop_2011_land, colour=AUTHORITY)) +
   geom_point(size=3) +
   scale_color_brewer(palette="Set2") +
   theme_bw() +
   theme(line=element_blank(),
         text=element_text(size=20)) +
   labs(title="Population census and hill farming",
        subtitle="Each point represents an agricultural parish",
        x="Hill farming score",
        y="Proportion of people employed\nin land based industry",
        colour="")
dev.off()

png("~/Cloud/Michael/SRUC/hill_farms/figures/pop_census_southern-uplands_national-context.png", width=1700, height=900, res=150)
ggplot(x, aes(score, prop_2011_land)) +
   geom_point(size=3, colour="grey70") +
   geom_point(data=y, aes(score, prop_2011_land, colour=AUTHORITY), size=3) +
   scale_color_brewer(palette="Set2") +
   theme_bw() +
   theme(line=element_blank(),
         text=element_text(size=20)) +
   labs(title="Population census and hill farming",
        subtitle="Each point represents an agricultural parish",
        x="Hill farming score",
        y="Proportion of people employed\nin land based industry",
        colour="")
dev.off()
