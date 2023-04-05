# Alberto Rovellini
# 3/21/2023
# Code to create Fig. 3 for ECCWO poster
# Fig. 3 weight at age and biomass of forage fish and groups that feed on it
# all the extraction code is take from Owen and PY's code

# we need to compare all runs to the base

# functional group dimensions
# extract from base run
# fg_dimensions <- hyper_grids(out_base) %>% 
#   pluck("grid") %>% 
#   purrr::map_df(function(x){
#     out_base %>% activate(x) %>% hyper_vars() %>% 
#       mutate(grd=x)
#   })

# apply waa plotting function
fg_to_plot <- c('Capelin', 'Sandlance', 'Herring', 
                'Arrowtooth_flounder', 'Pollock', 'Cod',
                'Seabird_dive_fish', 'Seabird_surface_fish')

waa_base <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_base, this.nc = this_nc_base, run = 'base'))
waa_hw <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_hw, this.nc = this_nc_hw, run = 'hw'))
waa_prod <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_prod, this.nc = this_nc_prod, run = 'prod'))
waa_hw_prod <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_hw_prod, this.nc = this_nc_hw_prod, run = 'hw_prod'))

# # bind them and then plot them
# waa_all <- rbind(waa_base, waa_hw)
# waa_all$Name <- factor(waa_all$Name, levels = fg_to_plot) # reorder groups
# waa_all$age <- factor(waa_all$age, levels = as.character(1:10)) # treat age groups as a factor and order them 
# 
# # filter out the burn-in, or the worst of it at least
# waa_all <- waa_all %>% filter(year >= 20)
# 
# p <- ggplot()+
#   geom_line(data = waa_all, aes(year,weight,col=age), linewidth = .8)+
#   scale_color_viridis_d(begin = 0.1, end = 0.9)+
#   geom_rect(data = data.frame(xmin = c(NA, 30),
#                               xmax = c(NA, 35),
#                               ymin = c(NA, -Inf),
#                               ymax = c(NA, Inf),
#                               run = c('Control','Heat wave')),
#             aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = NA, fill = 'red', alpha = 0.2)+
#   theme_sleek()+
#   labs(col="Age Group",y="Wet Weight per Individual (kg)",x="Year")+
#   facet_grid2(run ~ Name, scales = 'free_y', independent = 'y')
# p
# ggsave(paste0('output/', 'waa_', run_base, '_vs_', run_hw, '.png'),
#        p,width = 12,height=5)

# heatmap for changes in condition
waa_heatmap_control <- waa_base

waa_heatmap <- waa_hw %>%
  left_join(waa_prod, by = c('year','age','age_group','Name')) %>%
  left_join(waa_hw_prod, by = c('year','age','age_group','Name')) %>%
  pivot_longer(-c(year, age_group, age, Name), names_to = 'run', values_to = 'weight_exp') %>%
  left_join(waa_heatmap_control, by = c('year','age','age_group','Name')) %>%
  mutate(percent_change = ((weight_exp-weight_base)/weight_base)*100) %>%
  filter(year >= 25)

# remove _ResN from age_group
waa_heatmap$age_group <- gsub('_ResN', '', waa_heatmap$age_group)

# order
waa_heatmap <- waa_heatmap %>%
  arrange(year, factor(Name, levels = fg_to_plot), age)

# fix order of age_group
waa_heatmap$age_group <- factor(waa_heatmap$age_group, 
                                levels = rev(unique(waa_heatmap$age_group)))

# fix order of run
waa_heatmap$run <- factor(waa_heatmap$run, 
                                levels = c('weight_hw_prod','weight_hw','weight_prod'))

# add guild for facets 
waa_heatmap <- waa_heatmap %>%
  rowwise() %>%
  mutate(Guild = case_when(
    Name %in% c('Capelin', 'Sandlance', 'Herring') ~ 'Forage fish',
    Name %in% c('Arrowtooth_flounder', 'Pollock', 'Cod') ~ 'Groundfish',
    Name %in% c('Seabird_dive_fish', 'Seabird_surface_fish') ~ 'Seabirds'
  )) %>%
  ungroup()

# make labels
run_labs <- c('Temperature + plankton', 'Temperature', 'Plankton')
names(run_labs) <- c('weight_hw_prod','weight_hw','weight_prod')

# plot
waa_hm <- ggplot()+
  geom_tile(data = waa_heatmap, aes(x = year, y = age_group, fill = percent_change))+
  scale_fill_viridis()+
  geom_vline(xintercept = 30, color = 'red', linetype = 'dashed')+
  geom_vline(xintercept = 35, color = 'red', linetype = 'dashed')+
  theme_bw()+
  labs(x = 'Year', y = '', fill = '%', title = 'Relative change in weight at age from control run')+
  facet_grid(Guild~run, scales = 'free_y', labeller = labeller(run = run_labs))
waa_hm

ggsave(paste0('output/', 'waa_relchange.png'),
       waa_hm,width = 7.5,height=8.5)
