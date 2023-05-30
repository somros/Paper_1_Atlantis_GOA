# Alberto Rovellini
# 5/8/2023
# Code to create heatmaps of relative change in weight at age over time for ICES paper
# Keeping older code for absolute dynamics too
# all the extraction code is take from Owen and PY's code
# 1. Temporal dynamics of weight at age
# 2. Relative differences in weight at age among runs

print('Doing fig_waa_hm_temporal.R')

# apply waa plotting function
fg_to_plot <- c('Capelin', 'Sandlance', 'Herring', 
                'Arrowtooth_flounder', 'Pollock', 'Cod',
                'Seabird_dive_fish', 'Seabird_surface_fish')

waa_base <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_base, this.nc = this_nc_base, run = 'base'))
waa_warm <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_warm, this.nc = this_nc_warm, run = 'warm'))
waa_prod <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_prod, this.nc = this_nc_prod, run = 'prod'))
waa_warm_prod <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_warm_prod, this.nc = this_nc_warm_prod, run = 'warm_prod'))


# Line plot ---------------------------------------------------------------

# These are probably less useful than the heatmap but they may be requested
# Leave commented out but they should wok OK
# # bind them and then plot them
# waa_base_lines <- waa_base %>% mutate(run = 'base') %>% rename(weight = weight_base)
# waa_warm_lines <- waa_warm %>% mutate(run = 'warm') %>% rename(weight = weight_warm)
# waa_prod_lines <- waa_prod %>% mutate(run = 'prod') %>% rename(weight = weight_prod)
# waa_warm_prod_lines <- waa_warm_prod %>% mutate(run = 'warm_prod') %>% rename(weight = weight_warm_prod)
# 
# waa_all <- rbind(waa_base_lines, waa_warm_lines, waa_prod_lines, waa_warm_prod_lines)
# waa_all$Name <- factor(waa_all$Name, levels = fg_to_plot) # reorder groups
# waa_all$age <- factor(waa_all$age, levels = as.character(1:10)) # treat age groups as a factor and order them
# 
# # filter out the burn-in, or the worst of it at least
# waa_all <- waa_all %>% filter(year >= 20)
# 
# # make labels
# run_labs <- c('Control', 'Temperature + Plankton', 'Temperature', 'Plankton')
# names(run_labs) <- c('base','warm_prod','warm','prod')
# 
# p <- ggplot()+
#   geom_line(data = waa_all, aes(year,weight,col=age), linewidth = .8)+
#   scale_color_viridis_d(begin = 0.1, end = 0.9)+
#   labs(col="Age Group",y="Wet Weight per Individual (kg)",x="Year")+
#   theme_bw()+
#   facet_grid2(run ~ LongName, scales = 'free_y', independent = 'y', labeller = labeller(run = run_labs))
# p
# ggsave(paste0('output/', now, '/waa_time_series.png'), p,width = 13,height=5)


# Heatmaps of relative difference in WAA over time ------------------------

waa_heatmap_control <- waa_base

waa_heatmap <- waa_warm %>%
  left_join(waa_prod, by = c('year','age','age_group','Name','LongName')) %>%
  left_join(waa_warm_prod, by = c('year','age','age_group','Name','LongName')) %>%
  pivot_longer(-c(year, age_group, age, Name, LongName), names_to = 'run', values_to = 'weight_exp') %>%
  left_join(waa_heatmap_control, by = c('year','age','age_group','Name','LongName')) %>%
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
                          levels = c('weight_warm_prod','weight_warm','weight_prod'))

# add guild for facets 
waa_heatmap <- waa_heatmap %>%
  left_join(guild_frame, by = c('Name' = 'fg')) 

# fix order of guilds
waa_heatmap$Guild <- factor(waa_heatmap$Guild,
                            levels = c('Forage fish', 'Flatfish', 'Gadids', 'Seabirds'))

# make labels
run_labs <- c('Temperature + plankton', 'Temperature', 'Plankton')
names(run_labs) <- c('weight_warm_prod','weight_warm','weight_prod')

# plot
waa_hm <- ggplot()+
  geom_tile(data = waa_heatmap, aes(x = year, y = age_group, fill = percent_change))+
  scale_fill_viridis()+
  geom_vline(xintercept = 30, color = 'red', linetype = 'dashed')+
  theme_bw()+
  labs(x = 'Year', y = '', fill = '%')+#, title = 'Relative change in weight at age from control run')+
  facet_grid(Guild~run, scales = 'free_y', space = 'free_y', labeller = labeller(run = run_labs))
waa_hm

ggsave(paste0('output/', now, '/waa_relchange.png'),
       waa_hm,width = 8, height=9, dpi = 600)
