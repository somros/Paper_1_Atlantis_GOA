# Alberto Rovellini
# 3/21/2023
# Code to create Fig. 2 for ECCWO poster
# Fig. 2 biomass of groups that have the scalar, base run vs run with scalar (or relative, to each other, or something along those lines). Base this on the txt index files.

library(dplyr)
library(ggplot2)
library(tidyr)
library(rbgm)
library(ggsidekick)

select <- dplyr::select

run_base <- 1044
run_hw <- 1063

# set paths to directories
dir_base <- paste0('../../GOA/Parametrization/output_files/data/out_', run_base, '/')
dir_hw <- paste0('../../GOA/Parametrization/output_files/data/out_', run_hw, '/')

# read fg
atlantis_fg <- read.csv('../data/GOA_Groups.csv', header = T)

# read bgm

biom_base <- read.table(paste0(dir_base, 'outputGOA0', run_base, '_testBiomIndx.txt'), header = T)
biom_hw <- read.table(paste0(dir_hw, 'outputGOA0', run_hw, '_testBiomIndx.txt'), header = T)

# keep columns of interest, reshape data
biom_base_long <- biom_base %>%
  select(Time:DR) %>%
  mutate(Time = Time / 365) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'mt_base') %>%
  left_join((atlantis_fg %>% select(Code, Name)), by = 'Code') 

biom_hw_long <- biom_hw %>%
  select(Time:DR) %>%
  mutate(Time = Time / 365) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'mt_hw') %>%
  left_join((atlantis_fg %>% select(Code, Name)), by = 'Code') 

# join and get biomass relative to base run
biom_all <- biom_base_long %>%
  left_join(biom_hw_long, by = c('Time','Code','Name')) %>%
  mutate(Control = mt_base / mt_base,
         `Heat wave` = mt_hw / mt_base) %>%
  select(Time, Code, Name, Control, `Heat wave`) %>%
  pivot_longer(-c(Time, Code, Name), names_to = 'Run', values_to = 'rel_biomass')

# plot biomass over time, keep only plankton groups
p <- biom_all %>%
  filter(Code %in% c('ZS','ZM','ZL','EUP','PL','PS')) %>%
  ggplot(aes(x = Time, y = rel_biomass, group = Run))+
  geom_line(aes(color = Run, linetype = Run), linewidth = 1.5)+
  scale_color_manual(values = c('blue','orange'))+
  annotate("rect", xmin = 30, xmax = 35, ymin = -Inf, ymax = Inf,
           alpha = .2, fill = 'red')+
  theme_sleek()+
  labs(x = 'Year', y = 'Biomass relative to control')+
  facet_wrap(~Name)
p
ggsave('relative_plankton_biomass.png', p, width = 8, height = 5)
