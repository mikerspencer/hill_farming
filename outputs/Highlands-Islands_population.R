# --------------------------
# --------------------------
# Hill farms
# D&G and Borders plot
# --------------------------
# --------------------------

library(tidyverse)
library(RColorBrewer)

restrictions = read_csv("~/projects/hill_farming/data/parish_restrictions.csv") %>% 
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


# --------------------------
# Population


x = read_csv("~/projects/hill_farming/pop_census.csv") %>% 
   mutate(prop_2011_land=pop_working_land_2011 / pop_working_all_2011) %>% 
   select(PARCode, prop_2011_land) %>% 
   left_join(score)

y = read_csv("~/projects/hill_farming/LA_lookup.csv") %>% 
   filter(NAME=="Highland" | NAME=="Orkney Islands" | NAME=="Shetland Islands" | NAME=="Argyll and Bute" | NAME=="Na h-Eileanan an Iar") %>% 
   left_join(x)

png("~/projects/hill_farming/pop_census_Highlands-Islands.png", width=1700, height=900, res=150)
ggplot(y, aes(score, prop_2011_land, colour=NAME)) +
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

png("~/projects/hill_farming/pop_census_Highlands-Islands_national-context.png", width=1700, height=900, res=150)
ggplot(x, aes(score, prop_2011_land)) +
   geom_point(size=3, colour="grey70", shape=1) +
   geom_point(data=y, aes(score, prop_2011_land, colour=NAME), size=3, shape=1, stroke=1.5) +
   scale_shape(solid=F) +
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
