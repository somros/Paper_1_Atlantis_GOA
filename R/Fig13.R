# Alberto Rovellini
# 5/8/2023
# plot differences in terminal catch (5y avg) among scenarios
# Since we impose F at relatively low levels, I do not expect this to be much different among runs
# 

# pick run
run_base <- 1210
run_hw <- 1211
run_prod <- 1212
run_hw_prod <- 1213

dir_base <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_base, '/')
dir_hw <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_hw, '/')
dir_prod <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_prod, '/')
dir_hw_prod <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_hw_prod, '/')

# read catch file
# for now only txt file, may need to work on the NetCDF file if we care about catch by box (do we with one F?)
catch_base <- read.table(paste0(dir_base, 'outputGOA0', run_base, '_testCatch.txt'), 
                        sep = ' ',
                        header = T)
catch_hw <- read.table(paste0(dir_hw, 'outputGOA0', run_hw, '_testCatch.txt'), 
                      sep = ' ',
                      header = T)
catch_prod <- read.table(paste0(dir_prod, 'outputGOA0', run_prod, '_testCatch.txt'), 
                        sep = ' ',
                        header = T)
catch_hw_prod <- read.table(paste0(dir_hw_prod, 'outputGOA0', run_hw_prod, '_testCatch.txt'), 
                           sep = ' ',
                           header = T)

get_catch <- function(this_run){
  this_run <- this_run %>% select(KWT:BIV)
  catch_end_of_run <- this_run[(nrow(this_run)-4):nrow(this_run),]
  mean_catch <- data.frame(colMeans(catch_end_of_run))
  mean_catch
}

end_catch <- do.call('cbind', lapply(list(catch_base, catch_hw, catch_prod, catch_hw_prod), get_catch))

colnames(end_catch) <- c('catch_base', 'catch_hw', 'catch_prod', 'catch_hw_prod')

#calculate percent change in biomass
end_catch <- end_catch %>%
  mutate(group = rownames(end_catch),
         hw_to_base = (catch_hw - catch_base) / catch_base * 100,
         prod_to_base = (catch_prod - catch_base) / catch_base * 100,
         hw_prod_to_base = (catch_hw_prod - catch_base) / catch_base * 100) %>%
  select(group, hw_to_base:hw_prod_to_base) %>%
  pivot_longer(cols = -group, names_to = 'run', values_to = 'change')

# add group names and guilds
end_catch <- end_catch %>%
  left_join((biomass_groups) %>% select(Code, Name, LongName), by = c('group' = 'Code')) %>%
  left_join(guild_frame, by = c('Name'='fg'))

# order by run, guild, and descending by effect
end_catch <- end_catch %>%
  arrange(run, Guild, change)

# fix factors for names and guilds
end_catch$LongName <- factor(end_catch$LongName, levels = unique(end_catch$LongName))
end_catch$Guild <- factor(end_catch$Guild, levels = unique(end_catch$Guild))

# add color
end_catch <- end_catch %>%
  rowwise() %>%
  mutate(for_color = ifelse(change < 0, 'neg', 'pos'))

# drop empties
end_catch <- end_catch %>%
  drop_na()

# make labels
run_labs <- c('Temperature + plankton', 'Temperature', 'Plankton')
names(run_labs) <- c('hw_prod_to_base','hw_to_base','prod_to_base')

# fudge guild labels to have them horizontal in the figure
end_catch$Guild <- gsub(' ',
                        '\n',
                        end_catch$Guild)

# plot
p_catch <- end_catch %>%
  filter(Guild != 'Seabirds', Guild != 'Marine\nmammals') %>%
  ggplot(aes(x=LongName, y=change, color = for_color)) + 
  geom_hline(yintercept = 0, color = 'red') +
  geom_point(stat='identity', fill="black", size=2)  +
  geom_segment(aes(y = 0,
                   x = LongName,
                   yend = change,
                   xend = LongName, 
                   color = for_color)) +
  scale_color_viridis_d(begin = 0.2, end = 0.8) +
  theme_bw() +
  labs(x = '', y = '% change in catch from base scenario') + 
  coord_flip()+
  facet_grid(Guild~run, scales = 'free_y', space = 'free_y', labeller = labeller(run = run_labs))+
  theme(strip.text.y = element_text(angle = 0))
p_catch

ggsave('output/catch_change.png', p_catch, width = 8.5, height = 7.5)
