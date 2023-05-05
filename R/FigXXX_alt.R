# Plot diet comps for a set of predators before and during the heatwave years
library(tidyverse)
library(viridis)

run_base <- 1044
run_hw <- 1062

# set paths to directories
dir_base <- paste0('../../GOA/Parametrization/output_files/data/out_', run_base, '/')
dir_hw <- paste0('../../GOA/Parametrization/output_files/data/out_', run_hw, '/')

# read fg
atlantis_fg <- read.csv('../data/GOA_Groups.csv', header = T)

# producers and detritus
prods_gtype <- c('PHYTOBEN', 'LG_PHY', 'SM_PHY', 'SED_BACT', 'PL_BACT', 'CARRION', 'LAB_DET', 'REF_DET')
# predators:
preds_gtype <- setdiff((atlantis_fg %>% pull(GroupType) %>% unique()), prods_gtype)

# and corresponding codes
pred_codes <- atlantis_fg %>% filter(GroupType %in% preds_gtype) %>% pull(Code)

# these are the predators of interest (Codes)
preds_to_keep <- c('CAP','SAN','HER','ATF','POL','COD','BDF','BSF')
prey_to_keep <- c('ZS','ZM','ZL','EUP')

diets <- read.table(paste0(dir_hw, 'outputGOA0', run_hw, '_testDietCheck.txt'), header = T)

# filter time, aggregate cohorts, subset to predators of interest
diets_to_plot <- diets %>%
  filter(Predator %in% preds_to_keep & Updated == 0) %>% # IDK what Updated means but it seems to pertain the last time step only
  group_by(Time, Predator) %>% # drop cohorts
  summarise(across(KWT:DR, mean)) %>%
  ungroup() %>%
  mutate(Time = Time / 365) %>%
  #filter(Time < 35) %>% # for now focus on pre- and during HW, post HW they are recovering
  rowwise() %>% # now take meanas before and during the heatwave
  mutate(Period = ifelse(Time < 30, 'Normal', 'Heat wave')) %>%
  group_by(Period, Predator)%>%
  summarise(across(KWT:DR, mean)) %>%
  ungroup() %>%
  pivot_longer(-c(Period, Predator), names_to = 'Prey', values_to = 'Prop') %>%
  left_join((atlantis_fg %>% select(Code, Name)), by = c('Predator'='Code')) %>%
  rename(Predator_Name = Name) %>%
  select(-Predator) %>%
  left_join((atlantis_fg %>% select(Code, Name)), by = c('Prey'='Code')) %>%
  rename(Prey_Name = Name) %>%
  select(-Prey) %>%
  filter(Prop > 0.01)
  
# view
p <- ggplot(data = diets_to_plot, aes(x = Period, y = Prop, fill = Prey_Name))+
  geom_bar(stat='identity')+
  scale_fill_viridis_d()+
  theme_bw()+
  facet_grid(~Predator_Name)
p

# it is difficult to see differences in diet composition necessarily before and during the heatwave
# 


  
