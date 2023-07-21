# Alberto Rovellini
# 5/8/2023
# Code to create heatmap of percent difference in numbers at age by age class (last 5 year average) for ICES paper

print('Doing fig_naa_hm_static.R')

# apply naa plotting function
# rerun this because the fg to plot are different than from the previous heatmap
naa_base <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_base, this.nc = this_nc_base, run = 'base', spatial = FALSE))
naa_warm <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_warm, this.nc = this_nc_warm, run = 'warm', spatial = FALSE))
naa_prod <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_prod, this.nc = this_nc_prod, 'prod', spatial = FALSE))
naa_warm_prod <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_warm_prod, this.nc = this_nc_warm_prod, 'warm_prod', spatial = FALSE))

# heatmap for changes in numbers at age
naa_heatmap_control <- naa_base

naa_heatmap <- naa_warm %>%
  left_join(naa_prod, by = c('year','age','age_group','Name','LongName')) %>%
  left_join(naa_warm_prod, by = c('year','age','age_group','Name','LongName')) %>%
  pivot_longer(-c(year, age_group, age, Name,LongName), names_to = 'run', values_to = 'abun_exp') %>%
  left_join(naa_heatmap_control, by = c('year','age','age_group','Name','LongName')) 

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
                          levels = c('abun_warm','abun_prod','abun_warm_prod'))

# add guild for facets 
naa_heatmap <- naa_heatmap %>%
  left_join(guild_frame, by = c('Name' = 'fg'))

# fudge guild labels to have them horizontal in the figure
naa_heatmap$Guild <- gsub(' ',
                          '\n',
                          naa_heatmap$Guild)

# make labels
#run_labs <- c('Temperature + plankton', 'Temperature', 'Plankton')
run_labs <- c('Scenario 4', 'Scenario 2', 'Scenario 3')
names(run_labs) <- c('abun_warm_prod','abun_warm','abun_prod')

# take last 5 years only
naa_heatmap_5y <- naa_heatmap %>%
  filter(year >= max(year) - 5) %>%
  group_by(run, Guild, Name, LongName, age) %>%
  summarize(abun_exp = mean(abun_exp), abun_base = mean(abun_base)) %>%
  ungroup() %>%
  mutate(percent_change = ((abun_exp-abun_base)/abun_base)*100)

# arrange guilds for plot
naa_heatmap_5y_1 <- naa_heatmap_5y %>%
  filter(Name %in% fg_to_plot) %>%
  mutate(Guild = factor(Guild, levels = c("Forage\nfish",
                                          "Flatfish",
                                          "Gadids",
                                          "Sebastes\nand\nSebastolobus",
                                          "Other\ndemersal\nfish",      
                                          "Seabirds")))

# naa_heatmap_5y_1 <- naa_heatmap_5y %>%
#   filter(Name %in% plot_these) %>%
#   mutate(Guild = factor(Guild, levels = c("Algae",
#                                           "Phytoplankton",
#                                           "Zooplankton",
#                                           "Infauna",
#                                           "Epibenthos",
#                                           "Crustaceans",
#                                           "Shrimps",
#                                           "Cephalopod",
#                                           "Forage\nfish",
#                                           "Flatfish",
#                                           "Gadids",
#                                           "Sebastes\nand\nSebastolobus",
#                                           "Other\ndemersal\nfish",
#                                           "Salmon",
#                                           "Cartilaginous\nfish",
#                                           "Seabirds",
#                                           "Marine\nmammals",
#                                           "Detritus\nand\nbacteria")))

# plot
naa_hm <- naa_heatmap_5y_1 %>%
  filter(Name %in% fg_to_plot) %>%
  #filter(Name != 'Salmon_pink', Name != 'Skate_big') %>%
  ggplot()+
  geom_tile(aes(x = age, y = LongName, fill = percent_change), color = 'lightgrey')+
  #scale_fill_viridis()+
  #scale_fill_distiller(palette = 'RdBu')+
  colorspace::scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0, rev = T) + 
  theme_bw()+
  scale_x_continuous(breaks = 1:10)+
  labs(x = 'Age class', y = '', fill = '%')+#, title = 'Relative change in numbers at age from control run')+
  facet_grid(Guild~run, scales = 'free_y', space = 'free_y', labeller = labeller(run = run_labs))+
  theme(strip.text.y = element_text(angle = 0))
naa_hm

ggsave(paste0('output/', now, '/naa_relchange_5y.png'),
       naa_hm, width = 7.7, height=4.5, dpi = 600)

# some numbers
tt <- naa_heatmap_5y_1 %>%
  group_by(run, Name) %>%
  summarise(meanchange = mean(percent_change)) %>%
  ungroup() %>%
  filter(run == 'abun_prod')
