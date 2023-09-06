# Alberto Rovellini
# 5/8/2023
# Code to create a figure showing biomass differences at the end of the run between scenarios for ICES paper

print('Doing fig_biomass_changes.R')

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

# fix order of run
end_biom$run <- factor(end_biom$run, 
                        levels = c('warm_to_base','prod_to_base','warm_prod_to_base'))

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
#run_labs <- c('Temperature + plankton', 'Temperature', 'Plankton')
#run_labs <- c('Scenario 4', 'Scenario 2', 'Scenario 3')
run_labs <- c('Scenario 4\n(Increased temperature,\nDecreased LTL productivity)', 
              'Scenario 2\n(Increased temperature)', 
              'Scenario 3\n(Decreased LTL productivity)') # addressing Joe's comment
names(run_labs) <- c('warm_prod_to_base','warm_to_base','prod_to_base')

# fudge guild labels to have them horizontal in the figure
end_biom$Guild <- gsub(' ',
                        '\n',
                        end_biom$Guild)

# order guilds so that they go from low to high trophic level starting from the top of the plot
# end_biom1 <- end_biom %>%
#   filter(Name %in% plot_these) %>%
#   mutate(Guild = factor(Guild, levels = c("Phytoplankton",
#                                           "Zooplankton",
#                                           "Forage\nfish",
#                                           "Flatfish",
#                                           "Gadids",
#                                           "Sebastes\nand\nSebastolobus",
#                                           "Other\ndemersal\nfish",
#                                           "Seabirds")))

end_biom1 <- end_biom %>%
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
p_biom <- end_biom1 %>%
  ggplot(aes(x=LongName, y=change, color = for_color)) + 
  geom_hline(yintercept = 0, color = 'grey', linetype = 'dashed') +
  geom_point(stat='identity', fill="black", size=3)  +
  geom_segment(aes(y = 0,
                   x = LongName,
                   yend = change,
                   xend = LongName, 
                   color = for_color),
               linewidth = 1.5) +
  #scale_color_viridis_d(begin = 0.2, end = 0.8) +
  scale_color_manual(values = c('dodgerblue4','firebrick3'))+
  theme_bw() +
  labs(x = '', y = '% change in biomass from base scenario') + 
  guides(color="none") +
  coord_flip()+
  facet_grid(Guild~run, scales = 'free_y', space = 'free_y', labeller = labeller(run = run_labs))+
  theme(strip.text.y = element_text(angle = 0))
p_biom

ggsave(paste0('output/', now, '/biom_change.png'), p_biom, width = 8.3, height = 11)

# some numbers
tt <- end_biom1 %>%
  group_by(run, Name) %>%
  summarise(meanchange = mean(change)) %>%
  ungroup() %>%
  filter(run == 'prod_to_base')
