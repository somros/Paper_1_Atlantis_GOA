# Alberto Rovellini
# 5/8/2023
# Code to create heatmap of percent difference in weight at age by age class (last 5 year average) for ICES paper

print('Doing fig_waa_hm_static.R')

# pick groups to plot
fg_to_plot <- vertebrate_groups %>% pull(Name) # all vertebrates groups

fg_to_plot <- intersect(fg_to_plot, plot_these)

# apply waa plotting function
# rerun this because the fg to plot are different than from the previous heatmap
waa_base <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_base, this.nc = this_nc_base, run = 'base'))
waa_warm <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_warm, this.nc = this_nc_warm, run = 'warm'))
waa_prod <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_prod, this.nc = this_nc_prod, run = 'prod'))
waa_warm_prod <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_warm_prod, this.nc = this_nc_warm_prod, run = 'warm_prod'))

# heatmap for changes in condition
waa_heatmap_control <- waa_base

waa_heatmap <- waa_warm %>%
  left_join(waa_prod, by = c('year','age','age_group','Name','LongName')) %>%
  left_join(waa_warm_prod, by = c('year','age','age_group','Name','LongName')) %>%
  pivot_longer(-c(year, age_group, age, Name,LongName), names_to = 'run', values_to = 'weight_exp') %>% # exp for experiment
  left_join(waa_heatmap_control, by = c('year','age','age_group','Name','LongName')) 

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
                          levels = c('weight_warm','weight_prod','weight_warm_prod'))

# add guild for facets 
waa_heatmap <- waa_heatmap %>%
  left_join(guild_frame, by = c('Name' = 'fg'))

# fudge guild labels to have them horizontal in the figure
waa_heatmap$Guild <- gsub(' ',
                        '\n',
                        waa_heatmap$Guild)

# make labels
#run_labs <- c('Temperature + plankton', 'Temperature', 'Plankton')
#run_labs <- c('Scenario 4', 'Scenario 2', 'Scenario 3')
run_labs <- c('Scenario 4\n(Increased temperature,\nDecreased LTL productivity)', 
              'Scenario 2\n(Increased temperature)', 
              'Scenario 3\n(Decreased LTL productivity)') # addressing Joe's comment
names(run_labs) <- c('weight_warm_prod','weight_warm','weight_prod')

# take last 5 years only
waa_heatmap_5y <- waa_heatmap %>%
  filter(year >= max(year) - 5) %>% # only consider the last 5 years
  group_by(run, Guild, Name, LongName, age) %>%
  summarize(weight_exp = mean(weight_exp), weight_base = mean(weight_base)) %>%
  ungroup() %>%
  mutate(percent_change = ((weight_exp-weight_base)/weight_base)*100)

# arrange guilds for plot
# waa_heatmap_5y_1 <- waa_heatmap_5y %>%
#   filter(Name %in% fg_to_plot) %>%
#   mutate(Guild = factor(Guild, levels = c("Forage\nfish",
#                                           "Flatfish",
#                                           "Gadids",
#                                           "Sebastes\nand\nSebastolobus",
#                                           "Other\ndemersal\nfish",      
#                                           "Seabirds")))

waa_heatmap_5y_1 <- waa_heatmap_5y %>%
  filter(Name %in% plot_these) %>%
  mutate(Guild = factor(Guild, levels = c("Algae",
                                          "Phytoplankton",
                                          "Zooplankton",
                                          "Infauna",
                                          "Epibenthos",
                                          "Crustaceans",
                                          "Shrimps",
                                          "Cephalopod",
                                          "Forage\nfish",
                                          "Flatfish",
                                          "Gadids",
                                          "Sebastes\nand\nSebastolobus",
                                          "Other\ndemersal\nfish",
                                          "Salmon",
                                          "Cartilaginous\nfish",
                                          "Seabirds",
                                          "Marine\nmammals",
                                          "Detritus\nand\nbacteria")))

# plot
waa_hm <- waa_heatmap_5y_1 %>%
  #filter(Name != 'Salmon_pink') %>% # salmon pink died in those runs but bring it back in if it survives
  ggplot()+
  geom_tile(aes(x = age, y = LongName, fill = percent_change), color = 'darkgrey')+
  #scale_fill_viridis()+
  #scale_fill_distiller(palette = 'RdBu')+
  colorspace::scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0, rev = T) + 
  theme_bw()+
  scale_x_continuous(breaks = 1:10)+
  labs(x = 'Age class', y = '', fill = '%')+#, title = 'Relative change in weight at age from control run')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_grid(Guild~run, scales = 'free_y', space = 'free_y', labeller = labeller(run = run_labs))+
  theme(strip.text.y = element_text(angle = 0))
waa_hm

ggsave(paste0('output/', now, '/waa_relchange_5y1.png'),
       waa_hm, width = 9, height=11, dpi = 600)


# some numbers
tt <- waa_heatmap_5y_1 %>%
  group_by(run, Name) %>%
  summarise(meanchange = mean(percent_change)) %>%
  ungroup() %>%
  filter(run == 'weight_warm_prod')

