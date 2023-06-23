# Alberto Rovellini
# 6/23/2023
# This script makes plots of biomass, weight at age, and numbers at age for all species and organize them in panels
# This is to be used for S1

# how many groups have terminal biomass within some bounds of initial biomass?




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
    ggplot(aes(x = year, y = abun, color = factor(age)))+
    geom_line()+
    scale_color_viridis_d(begin = 0.1, end = 0.9)+
    theme_bw()+
    labs(x = 'Year', y = 'Number of individuals', color = 'Age group')+
    facet_wrap(~LongName, scales = 'free_y', ncol = 3,  nrow = 4)
  
  ggsave(paste0('output/', 'NAA_ts_', i, '.png'), p_waa, width = 8, height = 10)
  
}



  