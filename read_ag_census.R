# ----------------------------------------------------
# ----------------------------------------------------
# Read ag census data
# ----------------------------------------------------
# ----------------------------------------------------

library(tidyverse)

ag_schema = read_csv("AgCensusData/ag_census_schema.csv")

# 2000 ag census used to avoid foot and mouth
ag_census_2000 = read_csv("AgCensusData/ag_census_2000.csv") %>% 
   select(PARISH, TOTAREA, ITEM40, ITEM47, ITEM68, ITEM76, ITEM84, ITEM100, ITEM101, ITEM102, ITEM103, ITEM104, ITEM105, ITEM106, ITEM107, ITEM108, ITEM109, ITEM110, ITEM111, ITEM112, ITEM113, ITEM114, ITEM115, ITEM116, ITEM117, ITEM118, ITEM119, ITEM120, ITEM121, ITEM122, ITEM133, ITEM134, ITEM135, ITEM136, ITEM1809, ITEM1810, ITEM1894, ITEM1895, ITEM139, ITEM140, ITEM141, ITEM143, ITEM144, ITEM145, ITEM146, ITEM147, ITEM148, ITEM149, ITEM150, ITEM151, ITEM152, ITEM153, ITEM154, ITEM155, ITEM156, ITEM157, ITEM177, ITEM178, ITEM179, ITEM182, ITEM183, ITEM184, ITEM177, ITEM178, ITEM179, ITEM182, ITEM183, ITEM184, ITEM192, ITEM193, ITEM194, ITEM195, ITEM196, ITEM197, ITEM198, ITEM199, ITEM1714, ITEM1715, ITEM1716, ITEM1717, ITEM1718, ITEM1719)

ag_schema %>% 
   select(`Item name`, Description) %>% 
   filter(`Item name` %in% colnames(ag_census_2000)) %>% 
   data.frame()

lsu2000 = ag_census_2000 %>% 
   mutate(PARISH,
          dairy_cow = ITEM100 + ITEM102,
          beef_cow_dry = 0.75 * ITEM103,
          beef_cow_suckling = 0.8 * ITEM101,
          calf_under_6_months = 0.34 * (ITEM120 + ITEM121),
          Cow and unweaned calf	1.14
          calf_under_1_year = 0.34 * (ITEM118 + ITEM119)
          cattle_under_2_years = 0.65 * (ITEM114 + ITEM115 + ITEM116 + ITEM117)
          cattle_over_2_years = 0.8 * (ITEM110 + ITEM111 + ITEM112 + ITEM113)
          bull = 0.65 * (ITEM108 + ITEM109)
          sheep = 0.08 * ITEM145
          pig = 0.2 * ITEM157) %>%
   transmute(ID, LSU=dairy_cow + beef_cow_suckling + calf_under_6_mth + cow_calf + cow_under_1yr + cow_over_2yr + bull + (`Total pigs (c129)` * 0.2) + `Total sheep (c117)` * 0.08)

ag_census_2011 = read_csv("AgCensusData/ag_census_2011.csv")

# PARISH	Parish Number
# TOTAREA	Total Area
# 
# ITEM40	Total Crops and Fallow
# ITEM47	Rough Grazing
# ITEM68	Total Vegetables
# ITEM76	Total Soft Fruit
# ITEM84	Total bulbs, flowers and nursery stock
# 
# ITEM100	Dairy cows and heifers in milk
# ITEM101	Beef cows and heifers in milk
# ITEM102	Dairy cows in calf but not in milk
# ITEM103	Beef cows in calf but not in milk
# ITEM104	Dairy heifers 2 years old and over in calf for the first time
# ITEM105	Beef heifers 2 years old and over in calf for the first time
# ITEM106	Dairy heifers under 2 years old in calf for the first time
# ITEM107	Beef heifers under 2 years old in calf for the first time
# ITEM108	Bulls for service aged two years old and over
# ITEM109	Bulls for service aged between 1 and 2 years old
# ITEM110	Male cattle aged 2 years and over
# ITEM111	Female dairy cattle aged 2 years and over for breeding
# ITEM112	Female beef cattle aged 2 years and over for breeding
# ITEM113	Female cattle aged 2 years and over not for breeding
# ITEM114	Male cattle aged between 1 and 2 years
# ITEM115	Female dairy cattle for breeding aged between 1 and 2 years
# ITEM116	Female beef cattle for breeding aged between 1 and 2 years
# ITEM117	Female cattle not for breeding aged between 1 and 2 years
# ITEM118	Male cattle aged between 6 months and 1year
# ITEM119	Female cattle aged between 6 months and 1 year
# ITEM120	Male cattle aged under 6 months old
# ITEM121	Female cattle aged under 6 months old
# ITEM122	Total cattle
# ITEM133	Calves under 6 months old sold between 1 June and 1 June
# ITEM134	Calves between 6 months and 1 year old sold between 1 June and 1 June
# ITEM135	Calves under 6 months old bought between 1 June and 1 June
# ITEM136	Calves between 6 months and 1 year old bought between 1 June and 1 June
# ITEM137	Irish cattle bought for breeding
# ITEM138	Irish cattle bought for feeding
# ITEM1809	Calves and store cattle sold - 1 year old and under 2 years
# ITEM1810	Calves and store cattle bought - 1 year old and under 2 years
# ITEM1894	Calves and Store Cattle sold - 2 Years old and over
# ITEM1895	Calves and Store Cattle bought - 2 Years old and over
# 
# ITEM139	Ewes used for breeding in last season
# ITEM140	Rams to be used/expected to be used for service
# ITEM141	Other sheep aged 1 year and over for breeding
# ITEM143	Other sheep aged 1 year and over, other
# ITEM144	Lambs
# ITEM145	Total sheep, rams and lambs
# 
# ITEM146	Sows in pig
# ITEM147	Gilts in pig
# ITEM148	Other sows for breeding
# ITEM149	Barren sows for fattening
# ITEM150	Gilts 50kg and over to be used for breeding
# ITEM151	Boars being used for service
# ITEM152	Other pigs 110kg and over
# ITEM153	Other pigs 80-110kg
# ITEM154	Other pigs 50-80kg
# ITEM155	Other pigs 20-50kg
# ITEM156	Other pigs under 20kg
# ITEM157	Total pigs
# 
# ITEM158	Hens in first laying season
# ITEM159	Moulted hens
# ITEM161	Pullets being reared for laying
# ITEM160	Females laying eggs to hatch layer chicks
# ITEM162	Females laying eggs to hatch table chicks
# ITEM163	Cocks
# ITEM164	Broilers and other table fowls
# ITEM168	Turkeys for Breeding
# ITEM169	Turkeys for Fattening
# ITEM167	Other poultry
# ITEM170	Total Poultry
# 
# ITEM171	Other Livestock
# 
# ITEM177	Occupier working full-time on holding
# ITEM178	Occupier working half-time or more on holding
# ITEM179	Occupier working less than half time on holding
# ITEM181	Wife/husband of occupier (if doing farm work0
# ITEM182	Spouse working full-time on holding
# ITEM183	Spouse working half-time or more on holding
# ITEM184	Spouse working less than half time on holding
# ITEM177	Occupier working full-time on holding
# ITEM178	Occupier working half-time or more on holding
# ITEM179	Occupier working less than half time on holding
# ITEM182	Spouse working full-time on holding
# ITEM183	Spouse working half-time or more on holding
# ITEM184	Spouse working less than half time on holding
# ITEM188	Full-time Hired Males 20 yrs and over
# ITEM189	Full-time Family Males 20 yrs and over
# ITEM190	Full-time Hired Males under 20 yrs
# ITEM191	Full-time Family Males under 20 yrs
# ITEM192	Full-time hired Females
# ITEM193	Full-time Family Females
# ITEM194	Part-time Hired Males
# ITEM195	Part-time Family Males
# ITEM196	Part-time Hired Females
# ITEM197	Part-time Family Females
# ITEM198	Casual and Seasonal Males
# ITEM199	Casual and Seasonal Females
# ITEM1714	Full-time regular staff - male - partners
# ITEM1715	Full-time regular staff - male - hired
# ITEM1716	Full-time regular staff - male - member of occupiers family
# ITEM1717	Full-time regular staff - female - partners
# ITEM1718	Part-time regular staff - male - partners
# ITEM1719	Part-time regular staff - female - partners
# ITEM2066 	Person Working Days - contract labour
# ITEM2712	Non-family labour employed on non-regular basis
# 
# ITEM1869	Diversification - Tourism, accommodation and other leisure activities
# ITEM1870	Diversification - Handicraft
# ITEM1871	Diversification - Processing of Farm Products
# ITEM1872	Diversification - Wood Processing
# ITEM1873	Diversification - Contracting/Haulage
# ITEM1874	Diversification - Aquaculture
# ITEM1875	Diversification - Renewable Energy Production
# ITEM1876	Diversification - Other Commercial activities
# ITEM1877	Diversification - Other Commercial activities - Details
# ITEM2706	Christmas trees
