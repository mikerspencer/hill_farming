# --------------------------
# --------------------------
# Hill farms
# employment plot
# --------------------------
# --------------------------

library(tidyverse)
library(RColorBrewer)

restrictions = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/parish_restrictions.csv") %>% 
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

x = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/census/pop_census.csv") %>% 
   mutate(prop_2011_land=pop_working_land_2011 / pop_working_all_2011) %>% 
   select(PARCode, prop_2011_land) %>% 
   left_join(score)

png("~/Cloud/Michael/SRUC/hill_farms/figures/pop_census_all.png", width=1700, height=900, res=150)
ggplot(x, aes(score, prop_2011_land)) +
   geom_point(size=3, alpha=0.3) +
   theme_bw() +
   theme(line=element_blank(),
         text=element_text(size=20)) +
   labs(title="Population census and hill farming",
        subtitle="Each point represents an agricultural parish",
        x="Hill farming score",
        y="Proportion of people employed\nin agriculture, forestry and fishing",
        colour="")
dev.off()
