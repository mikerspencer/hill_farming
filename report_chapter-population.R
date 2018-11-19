# ---------------------------------------------
# ---------------------------------------------
# Figures Chapter: Population
# ---------------------------------------------
# ---------------------------------------------

library(tidyverse)
library(rgdal)
library(broom)
library(patchwork)
library(RColorBrewer)
library(viridis)
library(scales)


# ---------------------------------------------
# Data

hilliness = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/hilliness.csv")
hilliness$quintiles=cut_number(hilliness$score, n=5,
                               labels=c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"))

parishes = readOGR(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial/ag_parishes_2016.gpkg"), "simplified_parishes")

Scotland = readOGR(paste0(normalizePath("~"), "/Cloud/Michael/SRUC/hill_farms/data/spatial/ag_parishes_2016.gpkg"), "Scotland") %>% 
   tidy()

ag_census_2000 = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/census/ag_census_2000.csv") %>% 
   rename(PARCode=PARISH)
ag_census_2011 = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/census/ag_census_2011.csv") %>% 
   rename(PARCode=PARISH)
census = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/census/pop_census.csv")

parishes$id = row.names(parishes)
parishes = tidy(parishes) %>% 
   left_join(parishes@data)

hilliness_pop = hilliness %>% 
   left_join(census)


# ---------------------------------------------
# Functions

log_with_neg = function(i){
   i.na = which(is.na(i))
   i[is.na(i)] = 0
   i = i * 100
   i[i>=1] = log10(i[i>=1])
   i[i>-1 & i<1] = round(i[i>-1 & i<1], 1)
   i[i<=-1] = -log10(abs(i[i<=-1]))
   i[i.na] = NA
   i
}

exp_with_neg = function(i){
   i.na = which(is.na(i))
   i[is.na(i)] = 0
   i = i * 100
   i[i>=1] = exp(i[i>=1])
   i[i>-1 & i<1] = round(i[i>-1 & i<1], 1)
   i[i<=-1] = -exp(abs(i[i<=-1]))
   i[i.na] = NA
   i
}

trans_neg = trans_new(name="trans_neg",
                      transform=log_with_neg,
                      inverse=exp_with_neg)

plot.census = function(dat, i01, i11, tit, tit.x, pal, down=-0.5, up=0.5){
   x = dat %>% 
      rename_(i01=i01, i11=i11) %>% 
      mutate(perc_diff = ((i11 +1) / (i01 +1)) -1,
             perc_diff_map = perc_diff,
             perc_diff_map = replace(perc_diff_map, perc_diff_map<down, down),
             perc_diff_map = replace(perc_diff_map, perc_diff_map>up, up)) %>% 
      select(PARCode, score, i01, i11, perc_diff, perc_diff_map)
   
   if (max(x$perc_diff, na.rm=T)<3){
      parishes %>% 
         left_join(x) %>% 
         ggplot(aes(long, lat, group=group)) +
         geom_polygon(aes(fill=perc_diff_map)) +
         geom_polygon(data=Scotland, aes(long, lat, group=group),
                      colour="grey30", fill=NA, size=0.1) +
         coord_equal() +
         scale_fill_distiller(palette=pal, direction=1,
                              breaks=scales::pretty_breaks(n=5),
                              labels=scales::percent) +
         labs(fill=tit) +
         theme_minimal() +
         theme(axis.text=element_blank(),
               axis.title=element_blank(),
               line=element_blank(),
               text=element_text(size=30),
               legend.key.height=unit(3.5, "line")) +
         ggplot(x, aes(score, perc_diff)) +
         geom_point(size=5, alpha=0.3) +
         scale_y_continuous(labels=scales::percent) +
         labs(x="Hilliness score",
              y=tit.x) +
         theme_bw() +
         theme(text=element_text(size=30))
   } else {
      parishes %>% 
         left_join(x) %>% 
         ggplot(aes(long, lat, group=group)) +
         geom_polygon(aes(fill=perc_diff_map)) +
         geom_polygon(data=Scotland, aes(long, lat, group=group),
                      colour="grey30", fill=NA, size=0.1) +
         coord_equal() +
         scale_fill_distiller(palette=pal, direction=1,
                              breaks=scales::pretty_breaks(n=5),
                              labels=scales::percent,
                              limits=c(down, up)) +
         labs(fill=tit) +
         theme_minimal() +
         theme(axis.text=element_blank(),
               axis.title=element_blank(),
               line=element_blank(),
               text=element_text(size=30),
               legend.key.height=unit(3.5, "line")) +
         ggplot(x, aes(score, perc_diff)) +
         geom_point(size=5, alpha=0.3) +
         scale_y_continuous(labels=scales::percent) +
         labs(x="Hilliness score",
              y=tit.x) +
         theme_bw() +
         theme(text=element_text(size=30))
   }
}

plot.census.map = function(dat, i01, i11, tit, pal, down=-0.5, up=0.5){
   x = dat %>% 
      rename_(i01=i01, i11=i11) %>% 
      mutate(perc_diff = ((i11 +1) / (i01 +1)) -1,
             perc_diff_map = perc_diff,
             perc_diff_map = replace(perc_diff_map, perc_diff_map<down, down),
             perc_diff_map = replace(perc_diff_map, perc_diff_map>up, up)) %>% 
      select(PARCode, score, i01, i11, perc_diff, perc_diff_map)
   
   parishes %>% 
      left_join(x) %>% 
      ggplot(aes(long, lat, group=group)) +
      geom_polygon(aes(fill=perc_diff_map)) +
      geom_polygon(data=Scotland, aes(long, lat, group=group),
                   colour="grey30", fill=NA, size=0.1) +
      coord_equal() +
      scale_fill_distiller(palette=pal, direction=1,
                           breaks=scales::pretty_breaks(n=5),
                           labels=scales::percent) +
      labs(fill=tit) +
      theme_minimal() +
      theme(axis.text=element_blank(),
            axis.title=element_blank(),
            line=element_blank(),
            text=element_text(size=30),
            legend.key.height=unit(3.5, "line"))
}

plot.census.cum = function(dat.cum, ag, tit.cum){
   dat.cum %>% 
      filter(question==ag) %>% 
      drop_na() %>%
      group_by(year) %>% 
      arrange(score) %>% 
      mutate(cum = cumsum(value)) %>% 
      ggplot(aes(score, cum, colour=year)) +
      geom_line(size=2) +
      labs(x="Hilliness score",
           y=tit.cum,
           colour="") +
      theme_bw() +
      theme(text=element_text(size=30))
}


# ---------------------------------------------
# Population

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/population.png",
    height=1080, width=1600)
plot.census(hilliness_pop, "pop_all_2001", "pop_all_2011",
            "Population\nchange", "Population change", "PiYG")
dev.off()

sum(census$pop_all_2001)
sum(census$pop_all_2011)
100 * sum(census$pop_all_2011) / sum(census$pop_all_2001)



png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/population_male.png",
    height=1080, width=1600)
plot.census(hilliness_pop, "pop_male_2001", "pop_male_2011",
            "Male\npopulation\nchange", "Male population change", "PiYG")
dev.off()

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/population_female.png",
    height=1080, width=1600)
plot.census(hilliness_pop, "pop_female_2001", "pop_female_2011",
            "Female\npopulation\nchange", "Female population change", "PiYG")
dev.off()


# ---------------------------------------------
# Employment

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/employment.png",
    height=1080, width=1600)
plot.census(hilliness_pop, "pop_working_all_2001", "pop_working_all_2011",
            "Working\npopulation\nchange", "Working population change", "PRGn")
dev.off()

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/employment_land.png",
    height=1080, width=1600)
plot.census(hilliness_pop, "pop_working_land_2001", "pop_working_land_2011",
            "Working\nland\npopulation\nchange", "Working land population change", "PRGn")
dev.off()

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/employment_service.png",
    height=1080, width=1600)
plot.census(hilliness_pop, "pop_working_service_2001", "pop_working_service_2011",
            "Working\nservice\npopulation\nchange", "Working service population change", "PRGn")
dev.off()

temp = hilliness_pop %>% 
   transmute(PARCode, score,
             pop2001 = pop_working_land_2001 / (pop_working_all_2001 - pop_working_service_2001),
          pop2011 = pop_working_land_2011 / (pop_working_all_2011 - pop_working_service_2011))

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/employment_land_prop.png",
    height=1080, width=1600)
plot.census(temp, "pop2001", "pop2011",
            "Proportion\nworking\non land\nchange",
            "Proportion working on land change", "PRGn",
            down=-0.15, up=0.15)
dev.off()


# ---------------------------------------------
# Ag census employee type

temp = hilliness %>% 
   left_join(ag_census_2000, by=c(PARCode="PARCode")) %>% 
   left_join(ag_census_2011, by=c(PARCode="PARCode"))


lapply(colnames(ag_census_2000)[11:22], function(i){
   png(paste0("~/Cloud/Michael/SRUC/hill_farms/report/Figures/ag_census", i, ".png"),
       height=1080, width=1600)
   print(
      plot.census(temp, paste0(i, ".x"), paste0(i, ".y"),
                  paste0(i, "\nchange"), paste0(i, " change"), "RdBu")
   )
   dev.off()
})

lapply(colnames(ag_census_2000)[2:10], function(i){
   png(paste0("~/Cloud/Michael/SRUC/hill_farms/report/Figures/ag_census", i, ".png"),
       height=1080, width=1600)
   print(
      plot.census(temp, paste0(i, ".x"), paste0(i, ".y"),
                  paste0(i, "\nchange"), paste0(i, " change"), "RdYlGn")
   )
   dev.off()
})


# ---------------------------------------------
# Ag census cumsum

temp.cum = hilliness %>% 
   left_join(ag_census_2000, by=c(PARCode="PARCode")) %>% 
   left_join(ag_census_2011, by=c(PARCode="PARCode")) %>% 
   gather(key, value, -PARCode, -cluster2, -score) %>%
   separate(key, into=c("question", "year"), sep="\\.") %>% 
   mutate(year=replace(year, year=="x", 2000),
          year=replace(year, year=="y", 2011))

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/worker_owner.png",
    height=800, width=1600)
plot.census.map(temp, "ITEM177.x", "ITEM177.y", "ITEM177", "RdYlGn") +
   plot.census.map(temp, "ITEM178.x", "ITEM178.y", "ITEM178", "RdYlGn") +
   plot.census.map(temp, "ITEM179.x", "ITEM179.y", "ITEM179", "RdYlGn") +
   plot.census.cum(temp.cum, "ITEM177", "Farms") +
   plot.census.cum(temp.cum, "ITEM178", "Farms") +
   plot.census.cum(temp.cum, "ITEM179", "Farms") +
   plot_layout(ncol=3, nrow=2, heights = c(3, 1))
dev.off()

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/worker_spouse.png",
    height=800, width=1600)
plot.census.map(temp, "ITEM182.x", "ITEM182.y", "ITEM182", "PuOr") +
   plot.census.map(temp, "ITEM183.x", "ITEM183.y", "ITEM183", "PuOr") +
   plot.census.map(temp, "ITEM184.x", "ITEM184.y", "ITEM184", "PuOr") +
   plot.census.cum(temp.cum, "ITEM182", "Farms") +
   plot.census.cum(temp.cum, "ITEM183", "Farms") +
   plot.census.cum(temp.cum, "ITEM184", "Farms") +
   plot_layout(ncol=3, nrow=2, heights = c(3, 1))
dev.off()

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/worker_spouse.png",
    height=800, width=1600)
plot.census.map(temp, "ITEM182.x", "ITEM182.y", "ITEM182", "PuOr") +
   plot.census.map(temp, "ITEM183.x", "ITEM183.y", "ITEM183", "PuOr") +
   plot.census.map(temp, "ITEM184.x", "ITEM184.y", "ITEM184", "PuOr") +
   plot.census.cum(temp.cum, "ITEM182", "Farms") +
   plot.census.cum(temp.cum, "ITEM183", "Farms") +
   plot.census.cum(temp.cum, "ITEM184", "Farms") +
   plot_layout(ncol=3, nrow=2, heights = c(3, 1))
dev.off()

# employees?


png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/output_cattle_sheep.png",
    height=1080, width=1600)
plot.census.map(temp, "ITEM122.x", "ITEM122.y", "ITEM122", "BrBG") +
   plot.census.map(temp, "ITEM145.x", "ITEM145.y", "ITEM145", "BrBG") +
   plot.census.cum(temp.cum, "ITEM122", "Cattle") +
   plot.census.cum(temp.cum, "ITEM145", "Sheep") +
   plot_layout(ncol=2, nrow=2, heights = c(3, 1))
dev.off()

# ---------------------------------------------
# Distance to work

dist2001 = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/census/CAS225.csv",
                    skip=4, na="-") %>% 
   filter(X2=="ALL CLASSES OF NS-SeC") %>% 
   select(-X2) %>% 
   mutate_all(funs(replace(., is.na(.), 0)))

dist2001 = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/OutputAreas2001_parishes.csv") %>% 
   select(TAG=a_TAG, PARCode=b_PARCode, PARName=b_PARName, OutputAreas2001_area, a_OutputAreas2001_t_area) %>% 
   mutate(area_prop=OutputAreas2001_area / a_OutputAreas2001_t_area) %>% 
   left_join(dist2001, by=c(TAG="X1")) %>% 
   mutate(total.2001 = TOTAL * area_prop,
          home.2001=`Works mainly at or from home` * area_prop,
          less_2km.2001=`Less than 2km` * area_prop,
          between_2_5km.2001=`2km to less than 5km` * area_prop,
          between_5_10km.2001=`5km to less than 10km` * area_prop,
          between_10_20km.2001=`10km to less than 20km` * area_prop,
          over_20km.2001=`20km and over` * area_prop,
          offshore.2001=`Works on an offshore installation` * area_prop,
          other.2001=Other * area_prop) %>% 
   mutate(other.2001=offshore.2001 + other.2001) %>% 
   select(-OutputAreas2001_area, -a_OutputAreas2001_t_area, -TAG, -area_prop, -TOTAL, -`Works mainly at or from home`, -`Less than 2km`, -`2km to less than 5km`, -`5km to less than 10km`, -`10km to less than 20km`, -`20km and over`, -`Works on an offshore installation`, -Other, -offshore.2001) %>% 
   group_by(PARCode, PARName) %>% 
   summarise_all(sum, na.rm=T) %>%
   group_by(PARCode, PARName) %>% 
   mutate_all(round, 0) %>% 
   mutate(home.2001=home.2001 / total.2001,
          less_2km.2001=less_2km.2001 / total.2001,
          between_2_5km.2001=between_2_5km.2001 / total.2001,
          between_5_10km.2001=between_5_10km.2001 / total.2001,
          between_10_20km.2001=between_10_20km.2001 / total.2001,
          over_20km.2001=over_20km.2001 / total.2001,
          other.2001=other.2001 / total.2001)

dist2011 = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/census/LC7102SC.csv",
                    skip=4, na="-") %>% 
   filter(X2=="All people aged 16 to 74 in employment") %>% 
   select(-X2) %>% 
   mutate_all(funs(replace(., is.na(.), 0)))

dist2011 = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/OutputArea2011_MHW_parishes.csv") %>% 
   select(census_id=a_code, PARCode=b_PARCode, PARName=b_PARName, OutputArea2011_MHW_area, a_OutputArea2011_MHW_t_area) %>% 
   mutate(area_prop=OutputArea2011_MHW_area / a_OutputArea2011_MHW_t_area) %>% 
   left_join(dist2011, by=c(census_id="X1")) %>% 
   mutate(total.2011 = `All people aged 16 to 74 in employment` * area_prop,
          home.2011=`Work mainly at or from home` * area_prop,
          less_2km.2011=`Less than 2km` * area_prop,
          between_2_5km.2011=`2km to less than 5km` * area_prop,
          between_5_10km.2011=`5km to less than 10km` * area_prop,
          between_10_20km.2011=`10km to less than 20km` * area_prop,
          between_20_30km.2011=`20km to less than 30km` * area_prop,
          over_30km.2011=`30km and over` * area_prop,
          other.2011=`Other (2)` * area_prop) %>% 
   mutate(over_20km.2011=between_20_30km.2011 + over_30km.2011) %>% 
   select(-OutputArea2011_MHW_area, -a_OutputArea2011_MHW_t_area, -census_id, -area_prop, -`All people aged 16 to 74 in employment`, -`Work mainly at or from home`, -`Less than 2km`, -`2km to less than 5km`, -`5km to less than 10km`, -`10km to less than 20km`, -`20km to less than 30km`, -`30km and over`, -`Other (2)`, -between_20_30km.2011, -over_30km.2011) %>% 
   group_by(PARCode, PARName) %>% 
   summarise_all(sum, na.rm=T) %>%
   group_by(PARCode, PARName) %>% 
   mutate_all(round, 0) %>% 
   mutate(home.2011=home.2011 / total.2011,
          less_2km.2011=less_2km.2011 / total.2011,
          between_2_5km.2011=between_2_5km.2011 / total.2011,
          between_5_10km.2011=between_5_10km.2011 / total.2011,
          between_10_20km.2011=between_10_20km.2011 / total.2011,
          over_20km.2011=over_20km.2011 / total.2011,
          other.2011=other.2011 / total.2011)

parishes %>% 
   left_join(dist2001) %>% 
   ggplot(aes(long, lat, group=group)) +
   geom_polygon(aes(fill=over_20km_2001)) +
   geom_polygon(data=Scotland, aes(long, lat, group=group),
                colour="grey30", fill=NA, size=0.1) +
   coord_equal() +
   scale_fill_distiller(palette="Greens", direction=1,
                        breaks=scales::pretty_breaks(n=5),
                        labels=scales::percent) +
   labs(fill="People") +
   theme_minimal() +
   theme(axis.text=element_blank(),
         axis.title=element_blank(),
         line=element_blank(),
         text=element_text(size=15)) +
   parishes %>% 
   left_join(dist2011) %>% 
   ggplot(aes(long, lat, group=group)) +
   geom_polygon(aes(fill=over_20km_2011)) +
   geom_polygon(data=Scotland, aes(long, lat, group=group),
                colour="grey30", fill=NA, size=0.1) +
   coord_equal() +
   scale_fill_distiller(palette="Greens", direction=1,
                        breaks=scales::pretty_breaks(n=5),
                        labels=scales::percent) +
   labs(fill="People") +
   theme_minimal() +
   theme(axis.text=element_blank(),
         axis.title=element_blank(),
         line=element_blank(),
         text=element_text(size=15))

x = dist2001 %>% 
   left_join(dist2011) %>% 
   mutate(middle.2001 = less_2km.2001 + between_2_5km.2001 + between_5_10km.2001 + between_10_20km.2001,
          middle.2011 = less_2km.2011 + between_2_5km.2011 + between_5_10km.2011 + between_10_20km.2011) %>% 
   left_join(hilliness) %>% 
   select(-cluster2)
   
x %>% 
   ungroup() %>% 
   select(PARCode,
          home.2001, home.2011,
          middle.2001, middle.2011,
          over_20km.2001, over_20km.2011,
          other.2001, other.2011,
          quintiles) %>% 
   gather(distance, proportion, -PARCode, -quintiles) %>% 
   separate(distance, c("distance", "year"), sep="\\.") %>% 
   spread(year, proportion) %>% 
   drop_na() %>% 
   rename(yr_2001=`2001`, yr_2011=`2011`) %>% 
   ggplot(aes(yr_2001, yr_2011)) +
   geom_point() +
   geom_abline() +
   stat_smooth() +
   facet_grid(distance ~ quintiles)

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/distance_to_work.png",
    height=1080, width=1600)
x %>% 
   ungroup() %>% 
   select(PARCode,
          home.2001, home.2011,
          middle.2001, middle.2011,
          over_20km.2001, over_20km.2011,
          other.2001, other.2011,
          score) %>% 
   gather(distance, proportion, -PARCode, -score) %>% 
   separate(distance, c("distance", "year"), sep="\\.") %>% 
   drop_na() %>% 
   ggplot(aes(score, proportion)) +
   geom_point(size=5, alpha=0.3) +
   stat_smooth(size=3) +
   facet_grid(year ~ distance) +
   scale_y_continuous(labels=scales::percent) +
   labs(x="Hilliness score",
        y="Percent of workers") +
   theme_bw() +
   theme(text=element_text(size=30))
dev.off()
