# Alberto Rovellini
# 6/23/2023
# This script makes plots of biomass, weight at age, and numbers at age for all species and organize them in panels
# it also helps calculate some metrics that are useful to evaluate model skill sensu Kaplan and Marshall 2016.
# This is to be used for S1

run_base <- 1317
dir_base <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_base, '/')

# how long is the simulation period?
simyears <- 30

# how many groups have terminal biomass within some bounds of initial biomass?

total_biomass <- read.table(paste0(dir_base, 'outputGOA0', run_base, '_testBiomIndx.txt'), header = T)

init_biomass <- total_biomass %>%
  dplyr::select(Time:DR) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'biomass_init') %>%
  filter(Time == 0) %>%
  dplyr::select(-Time)

end_biomass <- total_biomass %>%
  dplyr::select(Time:DR) %>%
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
  dplyr::select(LongName, diff) 

diff_biomass %>%
  mutate(in_bounds = case_when(
    diff > 0.5 & diff < 2 ~ 1,
    diff > 0.25 & diff < 4 ~ 2,
    .default = 0
  )) %>%
  group_by(in_bounds) %>%
  tally() %>%
  mutate(prop = n / sum(n))

# as validation tool, let's see how many times we are within the bounds of historical observations
# This is a best estimate of the biomass of those stocks for which we have transboundary information
# So it is in fact very limited

ak <- read.csv('../../Validation/data/biomass_expanded_ak.csv')
bc <- read.csv('../../Validation/data/biomass_expanded_bc.csv')

source('../../Validation/R/key_names_to_codes.R')

# clean
ak <- ak %>% 
  select(Year, Species, TB) %>% 
  left_join(key_ak, by = 'Species') %>% 
  select(-Species) %>%
  drop_na() %>%
  group_by(Year, Group) %>%
  summarize(TB = sum(TB, na.rm = T)) %>%
  ungroup() %>%
  rename(TB_ak = TB)

# What species do we have in the ak data frame?
# ak %>% pull(Group) %>% unique() %>% sort()
# 
# [1] "Arrowtooth_flounder"     "Cod"                     "Deep_demersal"           "Dogfish"                
# [5] "Flatfish_deep"           "Flatfish_shallow"        "Flathead_sole"           "Halibut"                
# [9] "Pacific_ocean_perch"     "Pollock"                 "Rex_sole"                "Rockfish_demersal_shelf"
# [13] "Rockfish_pelagic_shelf"  "Rockfish_slope"          "Sablefish"               "Sculpins"               
# [17] "Skate_big"               "Skate_longnose"          "Skate_other"             "Thornyhead"        

bc <- bc %>% 
  select(Year, Species, TB) %>% 
  left_join(key_bc, by = 'Species') %>% 
  select(-Species) %>%
  drop_na() %>%
  group_by(Year, Group) %>%
  summarize(TB = sum(TB, na.rm = T)) %>%
  ungroup() %>%
  rename(TB_bc = TB)

# What species do we have in the bc data frame?
# bc %>% pull(Group) %>% unique() %>% sort()
# 
# [1] "Arrowtooth_flounder" "Cod"                 "Flatfish_shallow"    "Halibut"             "Pacific_ocean_perch"
# [6] "Pollock"             "Rockfish_slope"      "Sablefish"           "Thornyhead"   

# differences?
# setdiff((ak %>% pull(Group) %>% unique() %>% sort()), (bc %>% pull(Group) %>% unique() %>% sort()))
# 
# [1] "Deep_demersal"           "Dogfish"                 "Flatfish_deep"           "Flathead_sole"          
# [5] "Rex_sole"                "Rockfish_demersal_shelf" "Rockfish_pelagic_shelf"  "Sculpins"               
# [9] "Skate_big"               "Skate_longnose"          "Skate_other"       

# join and sum - where BC is not available just use AK for now
# TODO: fix this - either figure out an expansion factor or get more data for BC for recent years

obs <- ak %>%
  full_join(bc, by = c('Year','Group')) %>%
  #filter(Year >= 1990) %>%
  rowwise() %>%
  mutate(both = ifelse(is.na(TB_ak) | is.na(TB_bc), 'n', 'y')) %>%
  mutate(across(TB_ak:TB_bc, ~replace_na(.,0))) %>%
  mutate(TB = TB_ak + TB_bc) %>%
  select(Year, Group, TB, both) %>%
  rename(Name = Group)

# for each code in the end_biomass dataframe, if the code exists in the obs object, see if the value if within max and min
end_biomass <- end_biomass %>% left_join(grps %>% dplyr::select(Code, Name), by = 'Code')
check_bounds <- function(fg){
  if(fg %in% obs$Name){
    # bounds from data
    this_obs <- obs %>% filter(Name == fg)
    this_min_obs <- min(this_obs$TB)
    this_max_obs <- max(this_obs$TB)
    # now from run (predicted)
    this_pred <- end_biomass %>% filter(Name == fg)
    this_min_run <- min(this_pred$biomass_end)
    this_max_run <- max(this_pred$biomass_end)
    # get ratios
    minbound <- this_min_run / this_min_obs
    maxbound <- this_max_run / this_max_obs
    # write out as data frame
    these_bounds <- data.frame(fg, minbound, maxbound)
  }
}
  
bounds <- bind_rows(lapply(grps$Name, check_bounds))

# Make plots for S1 -------------------------------------------------------

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
  
  ggsave(paste0('output/S1_ts/', this_run, '/WAA_ts_', i, '.png'), p_waa, width = 8, height = 10)
  
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
  
  ggsave(paste0('output/S1_ts/', this_run, '/NAA_ts_', i, '.png'), p_waa, width = 8, height = 10)
  
}

rm(nage_all)
gc()
