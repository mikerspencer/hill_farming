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
      mutate(perc_diff = ((i11 +1) / (i01 +1)) -1) %>% 
      select(PARCode, score, i01, i11, perc_diff)
   
   if (max(x$perc_diff, na.rm=T)<3){
      parishes %>% 
         left_join(x) %>% 
         ggplot(aes(long, lat, group=group)) +
         geom_polygon(aes(fill=perc_diff)) +
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
               text=element_text(size=25)) +
         ggplot(x, aes(score, perc_diff)) +
         geom_point(size=5, alpha=0.3) +
         scale_y_continuous(labels=scales::percent) +
         labs(x="Hilliness score",
              y=tit.x) +
         theme_bw() +
         theme(text=element_text(size=25))
   } else {
      parishes %>% 
         left_join(x) %>% 
         ggplot(aes(long, lat, group=group)) +
         geom_polygon(aes(fill=perc_diff)) +
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
               text=element_text(size=25)) +
         ggplot(x, aes(score, perc_diff)) +
         geom_point(size=5, alpha=0.3) +
         scale_y_continuous(labels=scales::percent) +
         labs(x="Hilliness score",
              y=tit.x) +
         theme_bw() +
         theme(text=element_text(size=25))
   }
}


# ---------------------------------------------
# Population

png("~/Cloud/Michael/SRUC/hill_farms/report/Figures/population.png",
    height=1080, width=1600)
plot.census(hilliness_pop, "pop_all_2001", "pop_all_2011",
            "Population\nchange", "Population change", "PiYG")
dev.off()

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

temp = hilliness %>% 
   left_join(ag_census_2000, by=c(PARCode="PARCode")) %>% 
   left_join(ag_census_2011, by=c(PARCode="PARCode")) %>% 
   gather(key, value, -PARCode, -cluster2, -score) %>%
   separate(key, into=c("question", "year"), sep="\\.") %>% 
   mutate(year=replace(year, year=="x", 2001),
          year=replace(year, year=="y", 2011))
   
#cum sum
temp %>% 
   filter(question=="ITEM177") %>% 
   drop_na() %>%
   group_by(year) %>% 
   arrange(score) %>% 
   mutate(cum = cumsum(value)) %>% 
ggplot(aes(score, cum, colour=year)) +
   geom_line()

#scatter
temp %>% 
   filter(question=="ITEM177") %>% 
   spread(year, value) %>% 
   ggplot(aes(`2001`, `2011`, colour=score)) + 
   geom_point() + 
   geom_abline()


# ---------------------------------------------
# Distance to work

dist2001 = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/census/CAS225.csv",
                    skip=4, na="-") %>% 
   filter(X2=="ALL CLASSES OF NS-SeC") %>% 
   select(-X2) %>% 
   mutate_all(funs(replace(., is.na(.), 0)))

areas2001 = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/spatial-processed/OutputAreas2001_parishes.csv") %>% 
   select(TAG=a_TAG, PARCode=b_PARCode, PARName=b_PARName, OutputAreas2001_area, a_OutputAreas2001_t_area) %>% 
   mutate(area_prop=OutputAreas2001_area / a_OutputAreas2001_t_area) %>% 
   left_join(dist2001, by=c(TAG="X1")) %>% 
   mutate(total = TOTAL * area_prop,
          home_2001=`Works mainly at or from home` * area_prop,
          less_2km_2001=`Less than 2km` * area_prop,
          between_2_5km_2001=`2km to less than 5km` * area_prop,
          between_5_10km_2001=`5km to less than 10km` * area_prop,
          between_10_20km_2001=`10km to less than 20km` * area_prop,
          over_20km_2001=`20km and over` * area_prop,
          offshore_2001=`Works on an offshore installation` * area_prop,
          other_2001=Other * area_prop) %>% 
   select(-OutputAreas2001_area, -a_OutputAreas2001_t_area, -TAG, -area_prop, -TOTAL, -`Works mainly at or from home`, -`Less than 2km`, -`2km to less than 5km`, -`5km to less than 10km`, -`10km to less than 20km`, -`20km and over`, -`Works on an offshore installation`, -Other) %>% 
   group_by(PARCode, PARName) %>% 
   summarise_all(sum, na.rm=T) %>%
   group_by(PARCode, PARName) %>% 
   mutate_all(round, 0)

dist2011 = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/census/LC7102SC.csv",
                    skip=4)

