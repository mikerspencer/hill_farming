x = census %>% 
   left_join(civ_parish_2001, by=c("PARName"="parish")) %>% 
   left_join(civ_parish_2011, by=c("PARName"="parish")) %>% 
   mutate(A_agri_2001=A_agri.x/100 * pop_working_all_2001,
          A_agri_2011=A_agri.y/100 * pop_working_all_2011) %>% 
   select(PARName, ag_2001=pop_working_land_2001, civ_2001=A_agri_2001, ag_2011=pop_working_land_2011, civ_2011=A_agri_2011)

sum(x$ag_2001[is.na(x$civ_2001)])
sum(x$ag_2011[is.na(x$civ_2011)])

x = x[complete.cases(x), ]

sum(x$civ_2001==0)
sum(x$ag_2001[x$civ_2001==0])

sum(x$civ_2011==0)

x %>% 
   filter(civ_2001==0) %>% 
   arrange(desc(ag_2001))
