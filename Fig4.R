# Alberto Rovellini
# 3/21/2023
# Code to create Fig. 4 for ECCWO poster
# Fig. 4 numbers at age
# all the extraction code is take from Owen and PY's code

# # set up a functional group types table
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

# output files
# base run
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

# # volumes of each layer
# # extract from base run
# volumes <- out_base %>% hyper_filter(t=t==0) %>% hyper_tibble(select_var="volume") %>% dplyr::select(-t)
# 
# # time dimension
# # extract from base run
# ts <- ncdf4::ncvar_get(this_nc_base,varid = "t") %>% as.numeric
# tyrs <- ts/(60*60*24*365)
# 
# # area of each box is the same as volume of the deepest depth layer, because the dz of that layer is 1
# areas <- volumes %>% filter(z==max(z)) %>% dplyr::select(b,volume) %>% rename(area=volume)

# functional group dimensions
# extract from base run
# fg_dimensions <- hyper_grids(out_base) %>% 
#   pluck("grid") %>% 
#   purrr::map_df(function(x){
#     out_base %>% activate(x) %>% hyper_vars() %>% 
#       mutate(grd=x)
#   })
# 
# # apply naa plotting function
# 
# fg_to_plot <- c('Capelin', 'Sandlance', 'Herring', 
#                 'Arrowtooth_flounder', 'Pollock', 'Cod',
#                 'Seabird_dive_fish', 'Seabird_surface_fish')

naa_base <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_base, this.nc = this_nc_base, run = 'base', spatial = FALSE))
naa_hw <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_hw, this.nc = this_nc_hw, run = 'hw', spatial = FALSE))
naa_prod <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_prod, this.nc = this_nc_prod, 'prod', spatial = FALSE))
naa_hw_prod <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_hw_prod, this.nc = this_nc_hw_prod, 'hw_prod', spatial = FALSE))

# bind them and then plot them
# naa_all <- rbind(naa_base, naa_hw)
# naa_all$Name <- factor(naa_all$Name, levels = fg_to_plot) # reorder groups
# naa_all$age <- factor(naa_all$age, levels = as.character(1:10)) # treat age groups as a factor and order them 
# 
# # filter out the burn-in, or the worst of it at least
# naa_all <- naa_all %>% filter(year >= 20)
# 
# p <- ggplot()+
#   geom_line(data = naa_all, aes(year,abun,col=age), linewidth = .8)+
#   scale_color_viridis_d(begin = 0.1, end = 0.9)+
#   geom_rect(data = data.frame(xmin = c(NA, 30),
#                               xmax = c(NA, 35),
#                               ymin = c(NA, -Inf),
#                               ymax = c(NA, Inf),
#                               run = c('Control','Heat wave')),
#             aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = NA, fill = 'red', alpha = 0.2)+
#   theme_sleek()+
#   labs(col="Age Group",y="Numbers (Millions)",x="Year")+
#   facet_grid2(run ~ Name, scales = 'free_y', independent = 'y')
# p
# ggsave(paste0('output/', 'naa_cod_', run_base, '_vs_', run_hw,'.png'),
#        p,width = 12,height=5)

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
  rowwise() %>%
  mutate(Guild = case_when(
    Name %in% c('Capelin', 'Sandlance', 'Herring') ~ 'Forage fish',
    Name %in% c('Arrowtooth_flounder', 'Pollock', 'Cod') ~ 'Groundfish',
    Name %in% c('Seabird_dive_fish', 'Seabird_surface_fish') ~ 'Seabirds'
  )) %>%
  ungroup()

# make labels
run_labs <- c('Temperature + plankton', 'Temperature', 'Plankton')
names(run_labs) <- c('abun_hw_prod','abun_hw','abun_prod')

# plot
naa_hm <- ggplot()+
  geom_tile(data = naa_heatmap, aes(x = year, y = age_group, fill = percent_change))+
  scale_fill_viridis()+
  geom_vline(xintercept = 30, color = 'red', linetype = 'dashed')+
  geom_vline(xintercept = 35, color = 'red', linetype = 'dashed')+
  theme_bw()+
  labs(x = 'Year', y = '', fill = '%', title = 'Relative change in numbers at age from control run')+
  facet_grid(Guild~run, scales = 'free_y', labeller = labeller(run = run_labs))
naa_hm

ggsave(paste0('output/', 'naa_relchange.png'),
       naa_hm,width = 7.5,height=8.5)
