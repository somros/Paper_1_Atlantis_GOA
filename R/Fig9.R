# Alberto Rovellini
# 5/5/2023
# Code to create Fig. 9 for ICES paper
# Fig. 9 percent difference in numbers at age by age class (last 5 year average)

guild_frame <- read.csv('../data/fg_to_guild.csv')
guild_frame <- guild_frame %>% mutate(fg = str_remove(fg, '_N'))
fg_to_plot <- vertebrate_groups %>% pull(Name) # all vertebrates groups

naa_base <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_base, this.nc = this_nc_base, run = 'base', spatial = FALSE))
naa_hw <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_hw, this.nc = this_nc_hw, run = 'hw', spatial = FALSE))
naa_prod <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_prod, this.nc = this_nc_prod, 'prod', spatial = FALSE))
naa_hw_prod <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_hw_prod, this.nc = this_nc_hw_prod, 'hw_prod', spatial = FALSE))

# heatmap for changes in numbers at age
naa_heatmap_control <- naa_base

naa_heatmap <- naa_hw %>%
  left_join(naa_prod, by = c('year','age','age_group','Name')) %>%
  left_join(naa_hw_prod, by = c('year','age','age_group','Name')) %>%
  pivot_longer(-c(year, age_group, age, Name), names_to = 'run', values_to = 'abun_exp') %>%
  left_join(naa_heatmap_control, by = c('year','age','age_group','Name')) %>%
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
                          levels = c('abun_hw_prod','abun_hw','abun_prod'))

# add guild for facets 
naa_heatmap <- naa_heatmap %>%
  left_join(guild_frame, by = c('Name' = 'fg'))

# make labels
run_labs <- c('Temperature + plankton', 'Temperature', 'Plankton')
names(run_labs) <- c('abun_hw_prod','abun_hw','abun_prod')

# take last 5 years only
naa_heatmap_5y <- naa_heatmap %>%
  filter(year >= max(year) - 5) %>%
  group_by(run, Guild, Name, age) %>%
  summarize(abun_exp = mean(abun_exp), abun_base = mean(abun_base)) %>%
  ungroup() %>%
  mutate(percent_change = ((abun_exp-abun_base)/abun_base)*100)

# plot
naa_hm <- naa_heatmap_5y %>%
  #filter(Name != 'Salmon_pink') %>%
  ggplot()+
  geom_tile(aes(x = age, y = Name, fill = percent_change), color = 'grey')+
  scale_fill_viridis()+
  theme_bw()+
  scale_x_continuous(breaks = 1:10)+
  labs(x = 'Age class', y = '', fill = '%', title = 'Relative change in numbers at age from control run')+
  facet_grid(Guild~run, scales = 'free_y', labeller = labeller(run = run_labs))
naa_hm

ggsave(paste0('output/', 'naa_relchange_5y.png'),
       naa_hm, width = 8, height=9, dpi = 600)
