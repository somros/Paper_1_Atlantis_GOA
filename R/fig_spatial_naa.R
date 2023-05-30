# Alberto Rovellini
# 5/8/2023
# Code to create spatial maps of changes in numbers at age between two runs for ICES paper
# all the extraction code is take from Owen and PY's code

print('Doing fig_spatial_naa_static.R')

# apply naa plotting function

# pick groups to plot
# as a note, be careful with what these are: pick forage and groundfish and you will be talking about heatwave stuff
fg_to_plot <- c('Capelin',  
                'Arrowtooth_flounder',
                'Sandlance', 
                'Pollock',
                'Herring', 
                'Cod')

# pick runs to compare
control <- 'base'
experiment <- 'warm_prod'

box_naa_control <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_base, this.nc = this_nc_base, run = control, spatial = TRUE))
box_naa_experiment <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_warm_prod, this.nc = this_nc_warm_prod, run = experiment, spatial = TRUE))

# name the columns with abundance the same
colnames(box_naa_control)[3] <- 'abun'
colnames(box_naa_experiment)[3] <- 'abun'

# join the two
box_naa <- box_naa_control %>%
  left_join(box_naa_experiment, by = c('t', 'box_id', 'Name', 'LongName')) %>%
  mutate(n_percent_change = (abun.y - abun.x) / abun.x*100) %>%
  filter(t %in% seq(seas,40,1)) %>% 
  filter(t >= spinup_length) %>%
  group_by(box_id, Name, LongName) %>%
  summarise(mean_change = mean(n_percent_change, na.rm = T)) %>%
  ungroup() %>%
  mutate(box_id = as.numeric(box_id))

# add space for plotting
box_naa_sf <- goa_sf %>%
  left_join(box_naa, by = 'box_id')

# reorder factors
box_naa_sf$Name <- factor(box_naa_sf$Name, 
                          levels = c('Capelin',  
                                     'Arrowtooth_flounder',
                                     'Sandlance', 
                                     'Pollock',
                                     'Herring', 
                                     'Cod'))

p_map <- box_naa_sf %>%
  split(.$Name) %>%
  purrr::map(~ ggplot()+
               geom_sf(data = ., aes(fill = mean_change, color = mean_change), color = NA)+
               scale_fill_viridis()+
               geom_sf(data = coast_sf)+
               theme_bw()+
               theme(legend.position=c(.1,.7),
                     legend.background = element_blank(),
                     legend.key.size = unit(0.35, 'cm'))+
               labs(fill = '') +
               facet_wrap(~LongName)) %>%
  cowplot::plot_grid(plotlist = ., ncol = 2)
p_map
  
ggsave(paste0('output/', now, '/map_naa_relchange_', run_base, '_vs_', run_warm,'.png'),
       p_map,width = 8,height=6, dpi = 600)  
