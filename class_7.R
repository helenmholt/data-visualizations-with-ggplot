#class 7 
#09/26/2025
#missing class on 10/2 so doing this early 
#all final solutions are in the .Rmd file for this project, this document is to tryout things and then copy and paste what works to the .Rmd

#load libraries
library(tidyverse)

#import data
biomass_fish <- read_csv("data/sttstj_fish_biomass.csv")

taxo <- read_csv("data/taxonomic.csv")

#1) create a line graph of Red Hind ("EPI GUTT") inside the national park 

#add species name to biomass_fish, don't necessarily need to do this now but will be helpful and needed for future questions
biomass_species <- left_join(biomass_fish, taxo, by = "SPECIES_CD")

#filter by species and protected status, keep needed columns
redhind_biomass_NP <- biomass_species %>% 
  filter(COMNAME.x == "red hind") %>% 
  filter(protected_status == "1") %>%
  select(YEAR, biomass, SE, protected_status, COMNAME.x)

#graph bby
ggplot (data = redhind_biomass_NP, 
        aes(x = YEAR, y = biomass)) + 
  geom_line(color = "red", linetype = 1)

#2) create a point and line graph of Yellowtail Snapper ("OCY CHRY") outside the national park

#use biomass_species and filter accordingly
yts_biomass_ONP <- biomass_species %>% 
  filter(COMNAME.x == "yellowtail snapper") %>% 
  filter(protected_status == "0") %>%
  select(YEAR, biomass, SE, protected_status, COMNAME.x)

#graph that data!
######come back to later

#3) create a bar graph of biomass over time for each species grouped by protected status

#4) create bar graph of trophic-level mean biomass inside and outside the park for each species 

#5) create a point and line plot of 'grouper' biomass (+/- SE) over time, inside and outside the park together 