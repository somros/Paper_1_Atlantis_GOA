# Alberto Rovellini
# 5/5/2023
# Code to create Fig. 8 for ICES paper
# Fig. 8 percent difference in weight at age by age class (last 5 year average)

# apply waa plotting function
guild_frame <- read.csv('../data/fg_to_guild.csv')
guild_frame <- guild_frame %>% mutate(fg = str_remove(fg, '_N'))
fg_to_plot <- vertebrate_groups %>% pull(Name) # all vertebrates groups

waa_base <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_base, this.nc = this_nc_base, run = 'base'))
waa_hw <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_hw, this.nc = this_nc_hw, run = 'hw'))
waa_prod <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_prod, this.nc = this_nc_prod, run = 'prod'))
waa_hw_prod <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_hw_prod, this.nc = this_nc_hw_prod, run = 'hw_prod'))

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
  left_join(guild_frame, by = c('Name' = 'fg'))

# make labels
run_labs <- c('Temperature + plankton', 'Temperature', 'Plankton')
names(run_labs) <- c('weight_hw_prod','weight_hw','weight_prod')

# take last 5 years only
waa_heatmap_5y <- waa_heatmap %>%
  filter(year >= max(year) - 5) %>%
  group_by(run, Guild, Name, age) %>%
  summarize(weight_exp = mean(weight_exp), weight_base = mean(weight_base)) %>%
  ungroup() %>%
  mutate(percent_change = ((weight_exp-weight_base)/weight_base)*100)

# plot
waa_hm <- waa_heatmap_5y %>%
  filter(Name != 'Salmon_pink') %>%
  ggplot()+
  geom_tile(aes(x = age, y = Name, fill = percent_change), color = 'grey')+
  scale_fill_viridis()+
  theme_bw()+
  scale_x_continuous(breaks = 1:10)+
  labs(x = 'Age class', y = '', fill = '%', title = 'Relative change in weight at age from control run')+
  facet_grid(Guild~run, scales = 'free_y', labeller = labeller(run = run_labs))
waa_hm

ggsave(paste0('output/', 'waa_relchange_5y.png'),
       waa_hm, width = 8, height=9, dpi = 600)
