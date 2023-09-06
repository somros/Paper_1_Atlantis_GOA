# what proportion of biomass is in BC for each group?
library(tidyverse)
# now read in s1-s4 for all species
# read S1-S4
dists_vert <- read.csv('../../Parametrization/build_init_prm_10COHORTS/data/seasonal_distribution_POP.csv')
dists_invert <- read.csv('../../Parametrization/build_init_prm_10COHORTS/data/seasonal_distribution_inverts_POP.csv')
dists <- cbind(dists_vert, dists_invert)

# reshape
dists_long <- dists %>%
  mutate(box_id = 0:108) %>%
  pivot_longer(-box_id) %>%
  mutate(seas = substr(name, (nchar(name)-1), (nchar(name))),
         stage = substr(name, (nchar(name)-3), (nchar(name)-3)),
         name = substr(name, 1, (nchar(name)-5))) %>%
  left_join(grps %>% select(Code, Name, LongName), by = c('name'='Name'))

# fix pollock from the get go, we now use adults distributions for juveniles so drop juveniles
dists_long <- dists_long %>%
  filter(!(name == 'Pollock' & stage == 'J'))

# fix FFS - to be set as halibut
dists_hal <- dists_long %>% filter(Code == 'HAL')
dists_ffs <- dists_long %>% filter(Code == 'FFS') %>% mutate(value = dists_hal$value)
dists_long <- dists_long %>% filter(Code != 'FFS') %>% rbind(dists_ffs)

# set group names 
vertnames <- intersect(vertebrate_groups %>% pull(Name), unique(dists_long$name))
invertnames <- intersect(setdiff((grps %>% pull(Name)), vertnames), unique(dists_long$name))

# apply function to drop duplicated distributions
vertsp <- bind_rows(lapply(vertnames, handle_dists, isvert = TRUE))
invertsp <- bind_rows(lapply(invertnames, handle_dists, isvert = FALSE))
allsp <- rbind(vertsp, invertsp)

# Change A and J with juvenile and adult and order J first
allsp <- allsp %>%
  mutate(stage = case_when(
    stage == 'J' ~ 'Juvenile',
    stage == 'A' ~ 'Adult',
    .default = stage
  ))

# focus on bottom trawl dists (i.e. no birds, mammals, pelagics)
todo <- c("Arrowtooth_flounder", "Benthic_carnivores", "Bivalves", "Capelin", "Cod", "Corals", 
          "Crab_king", "Crab_other", "Crab_tanner", "Deep_demersal", "Deposit_feeders", "Dogfish", 
          "Dolphins", "Epibenthic_carn", "Epibenthic_graze", "Eulachon", "Filter_feeders", 
          "Flatfish_deep", "Flatfish_shallow", "Flathead_sole", "Forage_slope", "Gelatinous_other", 
          "Halibut", "Jellyfish", "Meiobenthos", "Octopus", "Pacific_hake", "Pacific_ocean_perch", 
          "Pollock", "Rex_sole", "Rockfish_demersal_shelf", "Rockfish_pelagic_shelf", "Rockfish_slope", 
          "Sablefish", "Sculpins", "Shallow_demersal", "Shark_demersal", "Shrimp_other", "Shrimp_pandalid", 
          "Skate_big", "Skate_longnose", "Skate_other", "Sponges", "Squid", "Thornyhead"
)

allsp <- allsp %>% left_join(grps %>% select(Code, Name), by = 'Code')

# get prop in AK vs BC
sp_list <- list()
for(i in 1:length(todo)){
  this_name <- todo[i]
  this_dat <- allsp %>% 
    filter(Name == this_name) %>%
    rowwise() %>%
    mutate(area = ifelse(box_id < 92, 'AK', 'BC')) %>%
    ungroup() %>%
    group_by(LongName, seas, stage, area) %>%
    summarise(prop = sum(value)) %>%
    ungroup()
  sp_list[[i]] <- this_dat
}
sp_frame <- bind_rows(sp_list)

sp_frame %>% filter(area == 'AK') %>% pull(prop) %>% summary()
sp_frame %>% filter(area == 'BC') %>% pull(prop) %>% summary()

# make a dens plot
sp_frame %>%
  ggplot(aes(x = prop, color = area))+
  geom_density()

