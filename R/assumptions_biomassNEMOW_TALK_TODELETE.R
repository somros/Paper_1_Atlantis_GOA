# make plot to compare final biomass between runs with different assumptions

# list runs
# review these at home...
# cold runs
cold_niches_summer_dists <- 1328
cold_no_niches <- 1332
cold_niches_winter_dists <- 1334
cold_q10_0 <- 1338
cold_q10_2 <- 1340
cold_q10_altE <- 1258
cold_no_q10 <- 1336

# warm runs
warm_niches_summer_dists <- 1329
warm_no_niches <- 1333
warm_niches_winter_dists <- 1335
warm_q10_0 <- 1339
warm_q10_2 <- 1341
warm_q10_altE <- 1259
warm_no_q10 <- 1337

# set paths to directories
dir_cold_niches_summer_dists <- paste0('../../../GOA/Parametrization/output_files/data/out_', cold_niches_summer_dists, '/')
dir_cold_no_niches <- paste0('../../../GOA/Parametrization/output_files/data/out_', cold_no_niches, '/')
dir_cold_niches_winter_dists <- paste0('../../../GOA/Parametrization/output_files/data/out_', cold_niches_winter_dists, '/')
dir_cold_q10_0 <- paste0('../../../GOA/Parametrization/output_files/data/out_', cold_q10_0, '/')
dir_cold_q10_2 <- paste0('../../../GOA/Parametrization/output_files/data/out_', cold_q10_2, '/')
dir_cold_q10_altE <- paste0('../../../GOA/Parametrization/output_files/data/out_', cold_q10_altE, '/')
dir_cold_no_q10 <- paste0('../../../GOA/Parametrization/output_files/data/out_', cold_no_q10, '/')


dir_warm_niches_summer_dists <- paste0('../../../GOA/Parametrization/output_files/data/out_', warm_niches_summer_dists, '/')
dir_warm_no_niches <- paste0('../../../GOA/Parametrization/output_files/data/out_', warm_no_niches, '/')
dir_warm_niches_winter_dists <- paste0('../../../GOA/Parametrization/output_files/data/out_', warm_niches_winter_dists, '/')
dir_warm_q10_0 <- paste0('../../../GOA/Parametrization/output_files/data/out_', warm_q10_0, '/')
dir_warm_q10_2 <- paste0('../../../GOA/Parametrization/output_files/data/out_', warm_q10_2, '/')
dir_warm_q10_altE <- paste0('../../../GOA/Parametrization/output_files/data/out_', warm_q10_altE, '/')
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
biom_cold_q10_0 <- read.table(paste0(dir_cold_q10_0, 'outputGOA0', cold_q10_0, '_testBiomIndx.txt'), 
                               sep = ' ',
                               header = T)
biom_cold_q10_2 <- read.table(paste0(dir_cold_q10_2, 'outputGOA0', cold_q10_2, '_testBiomIndx.txt'), 
                              sep = ' ',
                              header = T)
biom_cold_q10_altE <- read.table(paste0(dir_cold_q10_altE, 'outputGOA0', cold_q10_altE, '_testBiomIndx.txt'), 
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
biom_warm_q10_0 <- read.table(paste0(dir_warm_q10_0, 'outputGOA0', warm_q10_0, '_testBiomIndx.txt'), 
                              sep = ' ',
                              header = T)
biom_warm_q10_2 <- read.table(paste0(dir_warm_q10_2, 'outputGOA0', warm_q10_2, '_testBiomIndx.txt'), 
                              sep = ' ',
                              header = T)
biom_warm_q10_altE <- read.table(paste0(dir_warm_q10_altE, 'outputGOA0', warm_q10_altE, '_testBiomIndx.txt'), 
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
                                         biom_cold_q10_2,
                                         biom_cold_q10_0,
                                         #biom_cold_q10_altE,
                                         biom_cold_no_q10,
                                         biom_warm_niches_summer_dists, 
                                         biom_warm_no_niches,
                                         biom_warm_niches_winter_dists,
                                         biom_warm_q10_2,
                                         biom_warm_q10_0,
                                         #biom_warm_q10_altE,
                                         biom_warm_no_q10), get_biom))

colnames(end_biom) <- c('biom_cold_niches_summer_dists', 
                        'biom_cold_no_niches', 
                        'biom_cold_niches_winter_dists',
                        'biom_cold_q10_2',
                        'biom_cold_q10_0',
                        #'biom_cold_q10_altE',
                        'biom_cold_no_q10',
                        'biom_warm_niches_summer_dists', 
                        'biom_warm_no_niches', 
                        'biom_warm_niches_winter_dists',
                        'biom_warm_q10_2',
                        'biom_warm_q10_0',
                        #'biom_warm_q10_altE',
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

# to_plot_assumptions <- unique(end_biom$Name) # plot only fish but at least all fish
#to_plot_assumptions <- grps %>% filter(GroupType == 'FISH') %>% pull(Name)

to_plot_assumptions <- c(#"Seabird_dive_fish", "Seabird_surface_fish", #"Seabird_dive_invert", "Seabird_surface_inverts",
                         "Pollock",  "Cod",
                         "Arrowtooth_flounder", "Halibut",# "Flathead_sole", "Rex_sole", "Flatfish_shallow", "Flatfish_deep" ,
                         #"Sablefish",
                         #"Pacific_ocean_perch", "Rockfish_slope",# "Rockfish_pelagic_shelf", "Rockfish_demersal_shelf",
                         #"Octopus", "Squid",
                         "Herring", "Capelin", "Sandlance", "Eulachon", "Forage_slope")#,
                         #"Crab_tanner", "Crab_king", "Crab_other",
                         #"Euphausiids", "Macrozooplankton",  "Mesozooplankton", "Microzooplankton", "Jellyfish", "Gelatinous_other",
                         #"Diatoms", "Picophytoplankton")#,
#"Detritus_labile", "Detritus_refractory")

# change scenario names
key <- data.frame(run = unique(end_biom$run),
                  label = c('Base model', 
                            'No thermal niches',
                            'Winter distributions',
                            'All fish unimodal bioenergetics',
                            'Default Q10 bioenergetics',
                            #'Lower assimilation',
                            'No temperature sensitivities'))
# join
end_biom <- end_biom %>% left_join(key, by = 'run')

# make a key with higher and lower realism for facets (how do we keep base in both)
key_realism <- data.frame(label = c('Base model', 
                                    'Winter distributions',
                                    'No thermal niches',
                                    'Default Q10 bioenergetics',
                                    'All fish unimodal bioenergetics',
                                    #'Lower assimilation',
                                    'No temperature sensitivities'),
                          realism = c('Lower realism', 
                                      'Higher realism',
                                      'Lower realism',
                                      'Lower realism',
                                      'Higher realism',
                                      #'Lower assimilation',
                                      'Lower realism'))

# bind to data
end_biom <- end_biom %>%
  left_join(key_realism)

# duplicate the base model and set it high realism so that it appears in all facets
end_biom <- rbind(end_biom,
                  end_biom %>% filter(label == 'Base model') %>% mutate(realism = 'Higher realism'))

# reorder
end_biom$label <- factor(end_biom$label, levels = c('Base model', 
                                                    'Winter distributions',
                                                    'All fish unimodal bioenergetics',
                                                    'No thermal niches',
                                                    'Default Q10 bioenergetics',
                                                    #'Lower assimilation',
                                                    'No temperature sensitivities'))

# for appendix fig, break guild text over rows
end_biom$Guild <- gsub('Sebastes and Sebastolobus','Sebastes and\nSebastolobus',end_biom$Guild)
end_biom$Guild <- gsub('Cartilaginous fish','Cartilaginous\nfish',end_biom$Guild)
end_biom$Guild <- gsub('Other demersal fish','Other\ndemersal fish',end_biom$Guild)


p_assumptions <- end_biom %>%
  filter(Name %in% to_plot_assumptions) %>%
  ggplot(aes(x = LongName, y = biomass/1000, group = regime))+
  geom_point(aes(shape = label, color = regime), size = 4, position = position_dodge(width = 0.8))+
  #scale_color_viridis_d(begin = 0.2, end = 0.8) +
  scale_color_manual(values = c('dodgerblue4','firebrick3'))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1.05, vjust = 1, size = 11),
        axis.text.y = element_text(size = 11))+
  labs(x = '', y = 'Final biomass (1000 mt)', shape = 'Assumption', color = 'Regime')+
  facet_grid(realism~Guild, scales = 'free_x', space = 'free_x')
p_assumptions
ggsave(paste0('output/', now, '/biom_assumptions.png'), p_assumptions, width = 12, height = 4.5)

ggsave(paste0('output/', now, '/biom_assumptions.png'), p_assumptions, width = 8, height = 5)

# all species
# p_assumptions_all <- end_biom %>%
#   filter(Name %in% to_plot_assumptions) %>%
#   ggplot(aes(x = LongName, y = biomass/1000, group = regime))+
#   geom_point(aes(shape = label, color = regime), size = 4, position = position_dodge(width = 0.8))+
#   #scale_color_viridis_d(begin = 0.2, end = 0.8) +
#   scale_color_manual(values = c('dodgerblue4','firebrick3'))+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 50, hjust = 1.05, vjust = 1, size = 11),
#         axis.text.y = element_text(size = 11))+
#   labs(x = '', y = 'Final biomass (1000 mt)', shape = 'Assumption', color = 'Regime')+
#   ggh4x::facet_grid2(realism~Guild, scales = 'free', independent = "y", space = 'free_x')
# p_assumptions_all
# 
# ggsave(paste0('output/', now, '/biom_assumptions_all.png'), p_assumptions_all, width = 13, height = 7)

# plot relative
end_biom_base <- end_biom %>%
  filter(run == 'niches_summer_dists', realism == 'Lower realism') %>%
  dplyr::select(group, regime, biomass) %>%
  rename(biomass_base = biomass)

end_biom_scenarios <- end_biom %>% filter(run != 'niches_summer_dists')
end_biom1 <- end_biom %>%
  left_join(end_biom_base, by = c('group','regime')) %>%
  mutate(biom_change = (biomass - biomass_base) / biomass_base * 100)

p_assumptions_rel <- end_biom1 %>%
  filter(Name %in% to_plot_assumptions, label != 'Base model') %>%
  ggplot(aes(x = LongName, y = biom_change, group = regime))+
  geom_point(aes(shape = label, color = regime), size = 4, position = position_dodge(width = 0.8))+
  geom_hline(yintercept = 0, color = 'grey', linetype = 'dashed')+
  #scale_color_viridis_d(begin = 0.2, end = 0.8) +
  scale_color_manual(values = c('dodgerblue4','firebrick3'))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1.05, vjust = 1, size = 11),
        axis.text.y = element_text(size = 11))+
  labs(x = '', y = 'Terminal biomass change from base scenario (%)', shape = 'Assumption', color = 'Regime')+
  facet_grid(realism~Guild, scales = 'free_x', space = 'free_x')
p_assumptions_rel
ggsave(paste0('output/', now, '/biom_assumptions_rel.png'), p_assumptions_rel, width = 12, height = 4.5)
