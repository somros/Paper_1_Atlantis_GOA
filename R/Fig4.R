# Alberto Rovellini
# 5/8/2023
# Code to create heatmaps of relative change in numbers at age over time
# Keeping older code for absolute dynamics too
# all the extraction code is take from Owen and PY's code
# 1. Temporal dynamics of numbers at age
# 2. Relative differences in numbers at age among runs

# apply waa plotting function
fg_to_plot <- c('Capelin', 'Sandlance', 'Herring', 
                'Arrowtooth_flounder', 'Pollock', 'Cod',
                'Seabird_dive_fish', 'Seabird_surface_fish')

naa_base <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_base, this.nc = this_nc_base, run = 'base', spatial = FALSE))
naa_warm <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_warm, this.nc = this_nc_warm, run = 'warm', spatial = FALSE))
naa_prod <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_prod, this.nc = this_nc_prod, 'prod', spatial = FALSE))
naa_warm_prod <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_warm_prod, this.nc = this_nc_warm_prod, 'warm_prod', spatial = FALSE))

# Line plot ---------------------------------------------------------------

# These are probably less useful than the heatmap but they may be requested
# Leave commented out but they should wok OK
# bind them and then plot them
# naa_base_lines <- naa_base %>% mutate(run = 'base') %>% rename(abun = abun_base)
# naa_warm_lines <- naa_warm %>% mutate(run = 'warm') %>% rename(abun = abun_warm)
# naa_prod_lines <- naa_prod %>% mutate(run = 'prod') %>% rename(abun = abun_prod)
# naa_warm_prod_lines <- naa_warm_prod %>% mutate(run = 'warm_prod') %>% rename(abun = abun_warm_prod)
# 
# naa_all <- rbind(naa_base_lines, naa_warm_lines, naa_prod_lines, naa_warm_prod_lines)
# naa_all$Name <- factor(naa_all$Name, levels = fg_to_plot) # reorder groups
# naa_all$age <- factor(naa_all$age, levels = as.character(1:10)) # treat age groups as a factor and order them
# 
# # filter out the burn-in, or the worst of it at least
# naa_all <- naa_all %>% filter(year >= 20)
# 
# # make labels
# run_labs <- c('Control', 'Temperature + Plankton', 'Temperature', 'Plankton')
# names(run_labs) <- c('base','warm_prod','warm','prod')
# 
# p <- ggplot()+
#   geom_line(data = naa_all, aes(year,abun,col=age), linewidth = .8)+
#   scale_color_viridis_d(begin = 0.1, end = 0.9)+
#   labs(col="Age Group",y="Numbers (Millions)",x="Year")+
#   theme_bw()+
#   facet_grid2(run ~ LongName, scales = 'free_y', independent = 'y', labeller = labeller(run = run_labs))
# p
# ggsave(paste0('output/', 'naa_time_series.png'), p,width = 13,height=5)

# Heatmaps of relative difference in WAA over time ------------------------

naa_heatmap_control <- naa_base

naa_heatmap <- naa_warm %>%
  left_join(naa_prod, by = c('year','age','age_group','Name','LongName')) %>%
  left_join(naa_warm_prod, by = c('year','age','age_group','Name','LongName')) %>%
  pivot_longer(-c(year, age_group, age, Name,LongName), names_to = 'run', values_to = 'abun_exp') %>%
  left_join(naa_heatmap_control, by = c('year','age','age_group','Name','LongName')) %>%
  mutate(percent_change = ((abun_exp-abun_base)/abun_base)*100) %>%
  filter(year >= 25)

# remove _Nums from age_group
naa_heatmap$age_group <- gsub('_Nums', '', naa_heatmap$age_group)

# order
naa_heatmap <- naa_heatmap %>%
  arrange(year, factor(Name, levels = fg_to_plot), age)

# fix order of age_group
naa_heatmap$age_group <- factor(naa_heatmap$age_group, 
                                levels = rev(unique(naa_heatmap$age_group)))

# fix order of run
naa_heatmap$run <- factor(naa_heatmap$run, 
                          levels = c('abun_warm_prod','abun_warm','abun_prod'))

# add guild for facets 
naa_heatmap <- naa_heatmap %>%
  left_join(guild_frame, by = c('Name' = 'fg')) 

# fix order of guilds
naa_heatmap$Guild <- factor(naa_heatmap$Guild,
                            levels = c('Forage fish', 'Flatfish', 'Gadids', 'Seabirds'))

# make labels
run_labs <- c('Temperature + plankton', 'Temperature', 'Plankton')
names(run_labs) <- c('abun_warm_prod','abun_warm','abun_prod')

# plot
naa_hm <- ggplot()+
  geom_tile(data = naa_heatmap, aes(x = year, y = age_group, fill = percent_change))+
  scale_fill_viridis()+
  geom_vline(xintercept = 30, color = 'red', linetype = 'dashed')+
  theme_bw()+
  labs(x = 'Year', y = '', fill = '%')+#, title = 'Relative change in numbers at age from control run')+
  facet_grid(Guild~run, scales = 'free_y', space = 'free_y', labeller = labeller(run = run_labs))
naa_hm

ggsave(paste0('output/', 'naa_relchange.png'),
       naa_hm, width = 8, height=9, dpi=600)
