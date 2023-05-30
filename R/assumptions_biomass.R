# make plot to compare final biomass between runs with different assumptions

# list runs
# review these at home...
# cold runs
cold_niches_summer_dists <- 1226
cold_no_niches <- 1230
cold_niches_winter_dists <- 1234
cold_no_q10 <- 1250
# cold_q10_1

# warm runs
warm_niches_summer_dists <- 1227
warm_no_niches <- 1231
warm_niches_winter_dists <- 1235
warm_no_q10 <- 1251
# warm_q10_1

# set paths to directories
dir_cold_niches_summer_dists <- paste0('../../../GOA/Parametrization/output_files/data/out_', cold_niches_summer_dists, '/')
dir_cold_no_niches <- paste0('../../../GOA/Parametrization/output_files/data/out_', cold_no_niches, '/')
dir_cold_niches_winter_dists <- paste0('../../../GOA/Parametrization/output_files/data/out_', cold_niches_winter_dists, '/')
dir_cold_no_q10 <- paste0('../../../GOA/Parametrization/output_files/data/out_', cold_no_q10, '/')

dir_warm_niches_summer_dists <- paste0('../../../GOA/Parametrization/output_files/data/out_', warm_niches_summer_dists, '/')
dir_warm_no_niches <- paste0('../../../GOA/Parametrization/output_files/data/out_', warm_no_niches, '/')
dir_warm_niches_winter_dists <- paste0('../../../GOA/Parametrization/output_files/data/out_', warm_niches_winter_dists, '/')
dir_warm_no_q10 <- paste0('../../../GOA/Parametrization/output_files/data/out_', warm_no_q10, '/')

# read in biomass tables
# cold
biom_cold_niches_summer_dists <- read.table(paste0(dir_cold_niches_summer_dists, 'outputGOA0', cold_niches_summer_dists, '_testBiomIndx.txt'), 
                            sep = ' ',
                            header = T)
biom_cold_no_niches <- read.table(paste0(dir_cold_no_niches, 'outputGOA0', cold_no_niches, '_testBiomIndx.txt'), 
                                   sep = ' ',
                                   header = T)
biom_cold_niches_winter_dists <- read.table(paste0(dir_cold_niches_winter_dists, 'outputGOA0', cold_niches_winter_dists, '_testBiomIndx.txt'), 
                                     sep = ' ',
                                     header = T)
biom_cold_no_q10 <- read.table(paste0(dir_cold_no_q10, 'outputGOA0', cold_no_q10, '_testBiomIndx.txt'), 
                                            sep = ' ',
                                            header = T)

# warm
biom_warm_niches_summer_dists <- read.table(paste0(dir_warm_niches_summer_dists, 'outputGOA0', warm_niches_summer_dists, '_testBiomIndx.txt'), 
                        sep = ' ',
                        header = T)
biom_warm_no_niches <- read.table(paste0(dir_warm_no_niches, 'outputGOA0', warm_no_niches, '_testBiomIndx.txt'), 
                        sep = ' ',
                        header = T)
biom_warm_niches_winter_dists <- read.table(paste0(dir_warm_niches_winter_dists, 'outputGOA0', warm_niches_winter_dists, '_testBiomIndx.txt'), 
                             sep = ' ',
                             header = T)
biom_warm_no_q10 <- read.table(paste0(dir_warm_no_q10, 'outputGOA0', warm_no_q10, '_testBiomIndx.txt'), 
                               sep = ' ',
                               header = T)

# write function that takes biom series, subsets to columns of interest, takes last 5 years, averages biomass

get_biom <- function(this_run){
  this_run <- this_run %>% select(KWT:DR)
  biom_end_of_run <- this_run[(nrow(this_run)-4):nrow(this_run),]
  mean_biom <- data.frame(colMeans(biom_end_of_run))
  mean_biom
}

end_biom <- do.call('cbind', lapply(list(biom_cold_niches_summer_dists, 
                                         biom_cold_no_niches,
                                         biom_cold_niches_winter_dists,
                                         biom_cold_no_q10,
                                         biom_warm_niches_summer_dists, 
                                         biom_warm_no_niches,
                                         biom_warm_niches_winter_dists,
                                         biom_warm_no_q10), get_biom))

colnames(end_biom) <- c('biom_cold_niches_summer_dists', 
                        'biom_cold_no_niches', 
                        'biom_cold_niches_winter_dists',
                        'biom_cold_no_q10',
                        'biom_warm_niches_summer_dists', 
                        'biom_warm_no_niches', 
                        'biom_warm_niches_winter_dists',
                        'biom_warm_no_q10')

# make plot
end_biom <- end_biom %>%
  mutate(group = rownames(end_biom)) %>%
  pivot_longer(cols = -group, names_to = 'run', values_to = 'biomass') %>%
  left_join((biomass_groups) %>% select(Code, Name, LongName), by = c('group' = 'Code')) %>%
  left_join(guild_frame, by = c('Name'='fg')) %>%
  mutate(regime = substr(run, 6, 9),
         run = substr(run, 11, nchar(run)))

# order by run and descending by effect
# end_biom <- end_biom %>%
#   arrange(run, Guild, change)

# # fix order of run
# end_biom$run <- factor(end_biom$run, 
#                        levels = c('warm_to_base','prod_to_base','warm_prod_to_base'))
# 
# # fix factors
# end_biom$LongName <- factor(end_biom$LongName, levels = unique(end_biom$LongName))
# end_biom$Guild <- factor(end_biom$Guild, levels = unique(end_biom$Guild))
# 
# # drop carrion
# end_biom <- end_biom %>% filter(LongName != 'Carrion')
# 
# # add color
# end_biom <- end_biom %>%
#   rowwise() %>%
#   mutate(for_color = ifelse(change < 0, 'neg', 'pos'))
# 
# # make labels
# run_labs <- c('Temperature + plankton', 'Temperature', 'Plankton')
# names(run_labs) <- c('warm_prod_to_base','warm_to_base','prod_to_base')

to_plot_assumptions <- c(#"Seabird_dive_fish", "Seabird_surface_fish", #"Seabird_dive_invert", "Seabird_surface_inverts", 
                         "Pollock",  "Cod",
                         "Arrowtooth_flounder", "Halibut",# "Flathead_sole", "Rex_sole", "Flatfish_shallow", "Flatfish_deep" , 
                         #"Sablefish",  
                         #"Pacific_ocean_perch", "Rockfish_slope",# "Rockfish_pelagic_shelf", "Rockfish_demersal_shelf", 
                         #"Octopus", "Squid", 
                         "Herring", "Capelin", "Sandlance", "Eulachon")#,# "Forage_slope", 
                         #"Crab_tanner", "Crab_king", "Crab_other",             
                         #"Euphausiids", "Macrozooplankton",  "Mesozooplankton", "Microzooplankton", "Jellyfish", "Gelatinous_other", 
                         #"Diatoms", "Picophytoplankton")#, 
#"Detritus_labile", "Detritus_refractory")

p_assumptions <- end_biom %>%
  filter(Name %in% to_plot_assumptions) %>%
  ggplot(aes(x = LongName, y = biomass, group = regime))+
  geom_point(aes(shape = run, color = regime), size = 4, position = position_dodge(width = 0.8))+
  scale_color_viridis_d(begin = 0.2, end = 0.8) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1.05, vjust = 1, size = 11),
        axis.text.y = element_text(size = 11))+
  labs(x = '', y = 'Final biomass (mt)', shape = 'Assumption', color = 'Regime')+
  facet_grid(~Guild, scales = 'free_x', space = 'free_x')
p_assumptions

ggsave(paste0('output/', now, '/biom_assumptions.png'), p_assumptions, width = 8, height = 4.5)

  
