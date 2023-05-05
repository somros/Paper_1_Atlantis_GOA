# Alberto Rovellini
# 3/21/2023
# Code to create Fig. 7 for ICES paper
# Biomass difference at the end of the run

dir_base <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_base, '/')
dir_hw <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_hw, '/')
dir_prod <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_prod, '/')
dir_hw_prod <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_hw_prod, '/')


# read in biomass tables
biom_base <- read.table(paste0(dir_base, 'outputGOA0', run_base, '_testBiomIndx.txt'), 
                        sep = ' ',
                        header = T)
biom_hw <- read.table(paste0(dir_hw, 'outputGOA0', run_hw, '_testBiomIndx.txt'), 
                        sep = ' ',
                        header = T)
biom_prod <- read.table(paste0(dir_prod, 'outputGOA0', run_prod, '_testBiomIndx.txt'), 
                        sep = ' ',
                        header = T)
biom_hw_prod <- read.table(paste0(dir_hw_prod, 'outputGOA0', run_hw_prod, '_testBiomIndx.txt'), 
                        sep = ' ',
                        header = T)

# write function that takes biom series, subsets to columns of interest, takes last 5 years, averages biomass

get_biom <- function(this_run){
  this_run <- this_run %>% select(KWT:DR)
  biom_end_of_run <- this_run[(nrow(this_run)-4):nrow(this_run),]
  mean_biom <- data.frame(colMeans(biom_end_of_run))
  mean_biom
}

end_biom <- do.call('cbind', lapply(list(biom_base, biom_hw, biom_prod, biom_hw_prod), get_biom))

colnames(end_biom) <- c('biom_base', 'biom_hw', 'biom_prod', 'biom_hw_prod')

#calculate percent change in biomass
end_biom <- end_biom %>%
  mutate(group = rownames(end_biom),
         hw_to_base = (biom_hw - biom_base) / biom_base * 100,
         prod_to_base = (biom_prod - biom_base) / biom_base * 100,
         hw_prod_to_base = (biom_hw_prod - biom_base) / biom_base * 100) %>%
  select(group, hw_to_base:hw_prod_to_base) %>%
  pivot_longer(cols = -group, names_to = 'run', values_to = 'change')

# add group names
end_biom <- end_biom %>%
  left_join((biomass_groups) %>% select(Code, Name), by = c('group' = 'Code'))

# order by run and descending by effect
end_biom <- end_biom %>%
  arrange(run, change)

# fix factors
end_biom$Name <- factor(end_biom$Name, levels = unique(end_biom$Name))

# drop carrion
end_biom <- end_biom %>% filter(Name != 'Carrion')

# add color
end_biom <- end_biom %>%
  rowwise() %>%
  mutate(for_color = ifelse(change < 0, 'neg', 'pos'))

# plot
p_biom <- ggplot(end_biom, aes(x=Name, y=change, color = for_color)) + 
  geom_hline(yintercept = 0, color = 'red') +
  geom_point(stat='identity', fill="black", size=2)  +
  geom_segment(aes(y = 0,
                   x = Name,
                   yend = change,
                   xend = Name, 
                   color = for_color)) +
  scale_color_viridis_d(begin = 0.2, end = 0.8) +
  theme_bw() +
  labs(x = '', y = '% change in biomass from base scenario') + 
  coord_flip()+
  facet_wrap(~run)
p_biom

ggsave('output/biom_change.png', p_biom, width = 7, height = 7.5)

