# Alberto Rovellini
# 3/21/2023
# Code to create Fig. 2 for ICES paper
# Fig. 2 biomass of groups that have the scalar, base run vs run with scalar (or relative, to each other, or something along those lines). Base this on the txt index files.

# compare base to prod run
biom_base <- read.table(paste0(dir_base, 'outputGOA0', run_base, '_testBiomIndx.txt'), header = T)
biom_prod <- read.table(paste0(dir_prod, 'outputGOA0', run_prod, '_testBiomIndx.txt'), header = T)

# keep columns of interest, reshape data
biom_base_long <- biom_base %>%
  select(Time:DR) %>%
  mutate(Time = Time / 365) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'mt_base') %>%
  left_join((grps %>% select(Code, Name)), by = 'Code') 

biom_prod_long <- biom_prod %>%
  select(Time:DR) %>%
  mutate(Time = Time / 365) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'mt_prod') %>%
  left_join((grps %>% select(Code, Name)), by = 'Code') 

# join and get biomass relative to base run
biom_all <- biom_base_long %>%
  left_join(biom_prod_long, by = c('Time','Code','Name')) %>%
  mutate(Control = mt_base / mt_base,
         `Heat wave` = mt_prod / mt_base) %>%
  select(Time, Code, Name, Control, `Heat wave`) %>%
  pivot_longer(-c(Time, Code, Name), names_to = 'Run', values_to = 'rel_biomass')

# plot biomass over time, keep only plankton groups that we are changing
p_prod <- biom_all %>%
  filter(Code %in% c('ZM','PL')) %>%
  ggplot(aes(x = Time, y = rel_biomass, group = Run))+
  geom_line(aes(color = Run, linetype = Run), linewidth = 0.8)+
  scale_color_viridis_d(begin = 0.2, end = 0.8)+
  annotate("rect", xmin = 30, xmax = 35, ymin = -Inf, ymax = Inf,
           alpha = .2, fill = 'yellow')+
  theme_bw()+
  labs(x = 'Year', y = 'Biomass relative to control')+
  facet_wrap(~Name, ncol = 1)
p_prod

ggsave(paste0('output/', 'relative_plankton_biomass_', run_base, '_vs_', run_prod, '.png'), 
       p_prod, width = 5, height = 1.5, dpi = 600)
