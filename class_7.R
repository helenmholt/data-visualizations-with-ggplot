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
ggplot (data = yts_biomass_ONP, 
  aes(x = YEAR, y = biomass)) + 
  geom_line(color = "#dcc516", linetype = 1) +
  geom_point(color = "#dcc516")

#3) create a bar graph of biomass over time for each species grouped by protected status

#note: I think it would be better to have two graphs one in the park and one out to compare but here is a graph with both
ggplot (data = biomass_species, 
  aes(x = interaction(YEAR, protected_status), y = biomass, fill = SCINAME )) +
  geom_col(position = position_dodge()) + 
  scale_y_continuous(expand = c(0,0))

#problem bc protected status is being treated like a continuous variable, fix by making a factor
biomass_species$protected_status <- factor(biomass_species$protected_status)

#now graph, used Gemini to help format 
ggplot (data = biomass_species, 
        aes(x = interaction(YEAR, protected_status), y = biomass, fill = SCINAME)) +
  geom_col(position = position_dodge()) + 
  scale_y_continuous(expand = c(0,0)) +
  
  # change view of x axis: labeling interaction values
  scale_x_discrete(
    name = "Year and Protected Status", # x axis title
    
    #use the labels argument to map the interaction (e.g., "2010.0") to a clearer label
    # This requires knowing all unique YEAR.protected_status combinations.
    # The following function dynamically creates them:
    labels = function(x) {
      parts <- strsplit(x, "\\.")
      sapply(parts, function(p) {
        year <- p[1]
        status_code <- p[2]
        status_label <- switch(status_code, 
                               "0" = "Open", 
                               "1" = "VI National Park", 
                               status_code)
        paste(year, "\n", status_label)})}) +
  
  #edit axis and legend labels
  labs(
    y = "Biomass (kg/177m2)", 
    fill = "Scientific Name") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#4) create bar graph of trophic-level mean biomass inside and outside the park for each species 

#need to get mean biomass 
mean_biomass <- biomass_species %>%
  group_by(trophic, protected_status) %>%
  summarise(
    mean_biomass = mean(biomass, na.rm = TRUE),
    .groups = 'drop')

#now graph
ggplot (data = mean_biomass, 
  aes(x = trophic, y = mean_biomass, fill = protected_status)) +
  geom_col(position = position_dodge()) + 
  scale_fill_discrete(labels = c("Open", "VI National Park")) + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(x = "Trophic Level", y = "Mean Biomass", fill = "Management Area")

#5) create a point and line plot of 'grouper' biomass (+/- SE) over time, inside and outside the park together 

#mean grouper biomass, across all species 
grouper_biomass <- biomass_species %>%
          filter(trophic == "grouper") %>% 
  group_by(YEAR, protected_status) %>%
  summarise(
    mean_biomass = mean(biomass, na.rm = TRUE),
    mean_se = mean(SE, na.rm = TRUE),
    .groups = 'drop'
  )

#graph
ggplot(data = grouper_biomass,
         aes(x = YEAR, y = mean_biomass, color = protected_status)) +
  geom_line(linetype = 1) +
  geom_errorbar(aes(ymin = mean_biomass - mean_se,
                    ymax = mean_biomass + mean_se),
                width = 0.2) + 
  scale_color_discrete(labels = c("Open", "VI National Park")) +
  labs(
    title = "Mean Grouper Biomass Over Time Within and Outside of Protected Areas",
    x = "Year",
    y = "Mean Biomass (+/- SE)",
    color = "Management Area"
  ) +
  theme_minimal()
