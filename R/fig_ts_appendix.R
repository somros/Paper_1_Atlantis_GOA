# Alberto Rovellini
# 6/23/2023
# This script makes plots of biomass, weight at age, and numbers at age for all species and organize them in panels
# This is to be used for S1

# how long is the simulation period?
simyears <- 30

# how many groups have terminal biomass within some bounds of initial biomass?

total_biomass <- read.table(paste0(dir_base, 'outputGOA0', run_base, '_testBiomIndx.txt'), header = T)

init_biomass <- total_biomass %>%
  dplyr::select(Time:DC) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'biomass_init') %>%
  filter(Time == 0) %>%
  dplyr::select(-Time)

end_biomass <- total_biomass %>%
  dplyr::select(Time:DC) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'biomass_end') %>% # take last 5 years for terminal biomass
  mutate(Time = ceiling(Time / 365)) %>%
  filter(Time <= simyears) %>%
  filter(Time %in% (max(Time)-5:max(Time))) %>%
  group_by(Code) %>%
  summarize(biomass_end = mean(biomass_end)) %>%
  ungroup() 

# join
diff_biomass <- init_biomass %>%
  left_join(end_biomass) %>%
  left_join(grps %>% dplyr::select(Code, LongName), by = 'Code') %>%
  mutate(diff = biomass_end / biomass_init) %>%
  dplyr::select(LongName, diff) %>%
  mutate(in_bounds = case_when(
    diff > 0.5 & diff < 2 ~ 1,
    diff > 0.25 & diff < 4 ~ 2,
    .default = 0
  )) %>%
  group_by(in_bounds) %>%
  tally() %>%
  mutate(prop = n / sum(n))

# weight at age
wage_all <- extract_wage_all(out = out_base, this.nc = this_nc_base)

# plot
# Npages
npage <- 4

# organize names into pages
page_key <- wage_all %>% 
  dplyr::select(LongName) %>% 
  distinct() %>% 
  arrange(LongName) %>%
  mutate(page = rep(1:4, each = ceiling(nrow(.) / npage))[1:nrow(.)])

# add page numbers
wage_all <- wage_all %>%
  left_join(page_key, by = 'LongName')

for(i in 1:npage){
  
  p_waa <- wage_all %>%
    filter(page == i) %>%
    filter(year <= simyears) %>%
    ggplot(aes(x = year, y = weight, color = factor(age)))+
    geom_line()+
    scale_color_viridis_d(begin = 0.1, end = 0.9)+
    theme_bw()+
    labs(x = 'Year', y = 'Mean body weight (kg)', color = 'Age group')+
    facet_wrap(~LongName, scales = 'free_y', ncol = 3,  nrow = 4)
  
  ggsave(paste0('output/', 'WAA_ts_', i, '.png'), p_waa, width = 8, height = 10)
  
}

rm(wage_all)
gc()

# numbers at age
nage_all <- extract_nage_all(out = out_base, this.nc = this_nc_base)

# add page numbers
nage_all <- nage_all %>%
  left_join(page_key, by = 'LongName')

for(i in 1:npage){
  
  p_waa <- nage_all %>%
    filter(page == i) %>%
    filter(year <= simyears) %>%
    ggplot(aes(x = year, y = abun, color = factor(age)))+
    geom_line()+
    scale_color_viridis_d(begin = 0.1, end = 0.9)+
    theme_bw()+
    labs(x = 'Year', y = 'Number of individuals', color = 'Age group')+
    facet_wrap(~LongName, scales = 'free_y', ncol = 3,  nrow = 4)
  
  ggsave(paste0('output/', 'NAA_ts_', i, '.png'), p_waa, width = 8, height = 10)
  
}

rm(nage_all)
gc()



  