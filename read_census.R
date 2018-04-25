# ----------------------------------------------------
# ----------------------------------------------------
# Read census data
# ----------------------------------------------------
# ----------------------------------------------------

library(tidyverse)


KS01 = read_csv("hill_farming/data/KS01.csv", skip=4)
KS02 = read_csv("hill_farming/data/KS02.csv", skip=4)
KS101SC = read_csv("hill_farming/data/KS101SC.csv", skip=4)
KS102SC = read_csv("hill_farming/data/KS101SC.csv", skip=4)
KS11a = read_csv("hill_farming/data/KS11a.csv", skip=4)
KS605SC = read_csv("hill_farming/data/KS605SC.csv", skip=4)


# Which columns are we interested in?


# Get proportion according to parishes

