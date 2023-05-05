# Alberto Rovellini
# 4/3/2023
# Code to create Fig. 5 for ICES paper
# Fig. 5 spatial patterns
# all the extraction code is take from Owen and PY's code
# only plot spatial for full model, with thermal HW forcings as well as plankton

# set up a functional group types table
# vertebrate_groups <- grps %>% filter(GroupType%in%c("FISH","SHARK","BIRD","MAMMAL")) %>% mutate(BiomassType="vertebrate")
# plankton_groups <- grps %>% filter(GroupType %in% c("PWN",'CEP','LG_ZOO','MED_ZOO','SM_ZOO','LG_PHY','SM_PHY')) %>% 
#   mutate(BiomassType="plankton")
# bottom_groups <- grps %>% filter(GroupType %in% c("MOB_EP_OTHER",'SED_EP_FF','SED_EP_OTHER','PHYTOBEN')) %>% 
#   mutate(BiomassType="2D")
# other_groups <- grps %>% filter(GroupType %in% c("LG_INF","MICROPHTYBENTHOS","SED_BACT","PL_BACT","SM_INF","CARRION","LAB_DET","REF_DET"))%>% 
#   mutate(BiomassType="other")
# biomass_groups <- bind_rows(vertebrate_groups,plankton_groups,bottom_groups,other_groups)
# 
# # add to grps df
# grps <- grps %>% left_join(biomass_groups)

# # output files
# # base run
# out_fl_base <- paste0(dir_base, 'outputGOA0', run_base, '_test.nc')
# out_base <- tidync(out_fl_base)
# this_nc_base <- ncdf4::nc_open(out_fl_base)
# # hw run
# out_fl_hw <- paste0(dir_hw, 'outputGOA0', run_hw, '_test.nc')
# out_hw <- tidync(out_fl_hw)
# this_nc_hw <- ncdf4::nc_open(out_fl_hw)

# derived values for output
# depths <- out %>% hyper_filter(t=t==0) %>% hyper_tibble(select_var="dz") %>% dplyr::select(-t)
# glimpse(depths)

# volumes of each layer
# extract from base run
# volumes <- out_base %>% hyper_filter(t=t==0) %>% hyper_tibble(select_var="volume") %>% dplyr::select(-t)
# 
# # time dimension
# # extract from base run
# ts <- ncdf4::ncvar_get(this_nc_base,varid = "t") %>% as.numeric
# tyrs <- ts/(60*60*24*365)
# 
# # area of each box is the same as volume of the deepest depth layer, because the dz of that layer is 1
# areas <- volumes %>% filter(z==max(z)) %>% dplyr::select(b,volume) %>% rename(area=volume)
# 
# # functional group dimensions
# # extract from base run
# fg_dimensions <- hyper_grids(out_base) %>% 
#   pluck("grid") %>% 
#   purrr::map_df(function(x){
#     out_base %>% activate(x) %>% hyper_vars() %>% 
#       mutate(grd=x)
#   })

# apply naa plotting function

fg_to_plot <- c('Capelin',  
                'Arrowtooth_flounder',
                'Sandlance', 
                'Pollock',
                'Herring', 
                'Cod')#,
                #'Seabird_dive_fish', 'Seabird_surface_fish')

box_naa_base <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_base, this.nc = this_nc_base, run = 'base', spatial = TRUE))
box_naa_hw <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_hw_prod, this.nc = this_nc_hw_prod, run = 'hw_prod', spatial = TRUE))

# join the two
box_naa <- box_naa_base %>%
  left_join(box_naa_hw, by = c('t', 'box_id', 'Name')) %>%
  mutate(n_percent_change = (abun_hw_prod - abun_base)/abun_base*100) %>%
  filter(t %in% seq(0.6,40,1)) %>% # keep only september values, highest SST, this depends on the time step output of the model
  filter(t >= 30 & t < 35) %>% # keep only during hw
  group_by(box_id, Name) %>%
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
               facet_wrap(~Name)) %>%
  cowplot::plot_grid(plotlist = ., ncol = 2)
p_map
  
ggsave(paste0('output/', 'map_naa_relchange_', run_base, '_vs_', run_hw,'.png'),
       p_map,width = 8,height=6, dpi = 600)  
