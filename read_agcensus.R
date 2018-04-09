# ---------------------
# ---------------------
# Read ag census data
# ---------------------
# ---------------------

library(tidyverse)

agcensus2000 = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/agcensus/Scotland_2000_2k.csv")
agcensus2011 = read_csv("~/Cloud/Michael/SRUC/hill_farms/data/agcensus/Scotland_2011_2k.csv")

agcensus2000$ID = 1:nrow(agcensus2000)
agcensus2011$ID = 1:nrow(agcensus2011)


# Lifestock unit ------
# https://en.wikipedia.org/wiki/Livestock_grazing_comparison

# Dairy cow	1
# Dry medium beef cow	0.75
# Medium beef cow suckling	0.8
# Calf under 6 months	0.34
# Cow and unweaned calf	1.14
# Heifer or steer under 1 year	0.34
# Heifer or steer under 2 years	0.65
# Heifer or steer over 2 years	0.8
# Bull	0.65
# Horse	0.8
# Medium sheep	0.08
# Goat	0.16
# Llama	0.45
# Pig	0.2

lsu2000 = agcensus2000 %>% 
   select(ID, `Dairy cows and heifers (c100)`, `Beef cows and heifers (c101)`, `Bulls for service >=2yrs (c108)`, `Bulls for service >1<2yrs (c109)`, `Total pigs (c157)`, `Total sheep (c145)`) %>% 
   transmute(ID, x, y,
             LSU=`Dairy cows and heifers (c100)` + 
                (`Beef cows and heifers (c101)` * 0.75) +
                ((`Bulls for service >=2yrs (c108)` + `Bulls for service >1<2yrs (c109)`) * 0.65) +
                (`Total pigs (c157)` * 0.2) +
                `Total sheep (c145)` * 0.08)

lsu2011 = agcensus2011 %>% 
   mutate(ID,
             dairy_cow=`Dairy cows in milk (c81)`, 
             beef_cow_suckling=0.8 * `Beef cows in milk (c82)`,
             calf_under_6_mth=0.34 * `Other male cattle <6mths (c101)` + `Other female cattle <6mths (c102)`,
             cow_calf=1.14 * (`Dairy cows in calf (c83)` + `Beef cows in calf (c84)`),
             cow_under_1yr=0.34 * (`Other female cattle 6mths-<1yr (c100)` + `Other male cattle 6mths-<1yr (c99)`),
             cow_under_2yr=0.65 * (`Beef heifers <2yrs (c88)` + `Dairy heifers <2yrs (c87)` + `Other male cattle 1-<2yrs (c95)` + `Other female dairy cattle 1-<2yrs (c96)` + `Other female beef cattle 1-<2yrs (c97)` + `Other female non-breeding cattle 1-<2yrs (c98)`),
             cow_over_2yr=0.8 * (`Dairy heifers 2yrs & over (c85)` + `Beef heifers 2yrs & over (c86)` + `Other male cattle 2yrs & over (c91)` + `Other female dairy cattle 2yrs & over (c92)` + `Other female beef cattle 2yrs & over (c93)` + `Other female non-breeding cattle 2yrs & over (c94)`),
             bull=0.65 * (`Bulls for service 2yrs & over (c89)` + `Bulls for service <2yrs (c90)`)) %>% 
   transmute(ID, LSU=dairy_cow + beef_cow_suckling + calf_under_6_mth + cow_calf + cow_under_1yr + cow_over_2yr + bull + (`Total pigs (c129)` * 0.2) + `Total sheep (c117)` * 0.08)

# Crop area ------

crop2000 = agcensus2000 %>% 
   select(ID, `Total Crops (c40)`, `Total vegetables (c68)`, `Total soft fruit (c76)`) %>% 
   rename(crops=`Total Crops (c40)`, vegetables=`Total vegetables (c68)`, fruit=`Total soft fruit (c76)`)


# Staff ------

staff2000 =  agcensus2000 %>% 
   select(ID, `Total reg and casual staff (c200)`) %>% 
   rename(staff=`Total reg and casual staff (c200)`)


# Land ------

land2000 = agcensus2000 %>% 
   select(ID, `Total area of land (c12)`, `LFA - area of severely disadvantaged land (c125)`, `LFA - area of disadvantaged land (c126)`) %>% 
   rename(land_total=`Total area of land (c12)`, land_severe=`LFA - area of severely disadvantaged land (c125)`, land_disadvantaged=`LFA - area of disadvantaged land (c126)`)


# Combine ------

agcensus2000 = agcensus2000 %>% 
   select(ID, x, y) %>% 
   inner_join(land2000) %>% 
   inner_join(staff2000) %>% 
   inner_join(lsu2000) %>% 
   inner_join(crop2000)
   
