# Alberto Rovellini
# 5/8/2023
# Code to create a figure showing biomass differences at the end of the run between scenarios

# dir_base <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_base, '/')
# dir_warm <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_warm, '/')
# dir_prod <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_prod, '/')
# dir_warm_prod <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_warm_prod, '/')


# read in biomass tables
biom_base <- read.table(paste0(dir_base, 'outputGOA0', run_base, '_testBiomIndx.txt'), 
                        sep = ' ',
                        header = T)
biom_warm <- read.table(paste0(dir_warm, 'outputGOA0', run_warm, '_testBiomIndx.txt'), 
                        sep = ' ',
                        header = T)
biom_prod <- read.table(paste0(dir_prod, 'outputGOA0', run_prod, '_testBiomIndx.txt'), 
                        sep = ' ',
                        header = T)
biom_warm_prod <- read.table(paste0(dir_warm_prod, 'outputGOA0', run_warm_prod, '_testBiomIndx.txt'), 
                        sep = ' ',
                        header = T)

# write function that takes biom series, subsets to columns of interest, takes last 5 years, averages biomass

get_biom <- function(this_run){
  this_run <- this_run %>% select(KWT:DR)
  biom_end_of_run <- this_run[(nrow(this_run)-4):nrow(this_run),]
  mean_biom <- data.frame(colMeans(biom_end_of_run))
  mean_biom
}

end_biom <- do.call('cbind', lapply(list(biom_base, biom_warm, biom_prod, biom_warm_prod), get_biom))

colnames(end_biom) <- c('biom_base', 'biom_warm', 'biom_prod', 'biom_warm_prod')

#calculate percent change in biomass
end_biom <- end_biom %>%
  mutate(group = rownames(end_biom),
         warm_to_base = (biom_warm - biom_base) / biom_base * 100,
         prod_to_base = (biom_prod - biom_base) / biom_base * 100,
         warm_prod_to_base = (biom_warm_prod - biom_base) / biom_base * 100) %>%
  select(group, warm_to_base:warm_prod_to_base) %>%
  pivot_longer(cols = -group, names_to = 'run', values_to = 'change')

# add group names
end_biom <- end_biom %>%
  left_join((biomass_groups) %>% select(Code, Name, LongName), by = c('group' = 'Code')) %>%
  left_join(guild_frame, by = c('Name'='fg'))

# order by run and descending by effect
end_biom <- end_biom %>%
  arrange(run, Guild, change)

# fix factors
end_biom$LongName <- factor(end_biom$LongName, levels = unique(end_biom$LongName))
end_biom$Guild <- factor(end_biom$Guild, levels = unique(end_biom$Guild))

# drop carrion
end_biom <- end_biom %>% filter(LongName != 'Carrion')

# add color
end_biom <- end_biom %>%
  rowwise() %>%
  mutate(for_color = ifelse(change < 0, 'neg', 'pos'))

# make labels
run_labs <- c('Temperature + plankton', 'Temperature', 'Plankton')
names(run_labs) <- c('warm_prod_to_base','warm_to_base','prod_to_base')

# fudge guild labels to have them horizontal in the figure
end_biom$Guild <- gsub(' ',
                        '\n',
                        end_biom$Guild)

# plot
p_biom <- ggplot(end_biom, aes(x=LongName, y=change, color = for_color)) + 
  geom_hline(yintercept = 0, color = 'red') +
  geom_point(stat='identity', fill="black", size=2)  +
  geom_segment(aes(y = 0,
                   x = LongName,
                   yend = change,
                   xend = LongName, 
                   color = for_color)) +
  scale_color_viridis_d(begin = 0.2, end = 0.8) +
  theme_bw() +
  labs(x = '', y = '% change in biomass from base scenario', color = '') + 
  coord_flip()+
  facet_grid(Guild~run, scales = 'free_y', space = 'free_y', labeller = labeller(run = run_labs))+
  theme(strip.text.y = element_text(angle = 0))
p_biom

ggsave('output/biom_change.png', p_biom, width = 8.5, height = 8.5)
