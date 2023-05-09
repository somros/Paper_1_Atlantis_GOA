# Alberto Rovellini
# 5/8/2023
# Code to create a figure illustrating changes in diets between runs for the ICES paper
# 1. Barchart for selected predators and prey
# 2. heat,ap for all functional groups by guild
# all the extraction code is take from Owen and PY's code


# Barchart of selected groups ---------------------------------------------

# these are the predators of interest for this plot (Codes)
preds_to_keep <- c('CAP','SAN','HER','ATF','POL','COD','BDF','BSF')
pred_names <- grps %>% 
  filter(Code %in% preds_to_keep) %>% 
  arrange(factor(Code, levels = preds_to_keep)) %>%
  pull(Name)

# pick runs to compare
control <- run_base
experiment <- run_warm_prod

# read in diet data
diets_base <- read.table(paste0(dir_base, 'outputGOA0', control, '_testDietCheck.txt'), header = T)
diets_warm <- read.table(paste0(dir_warm_prod, 'outputGOA0', experiment, '_testDietCheck.txt'), header = T)

# apply function to get diet changes
diets_base1 <- compare_diets(diets_base, prednames = pred_names, run = 'control', age_split = 'none')
diets_warm1 <- compare_diets(diets_warm, prednames = pred_names, run = 'experiment', age_split = 'none')

dietchange <- diets_base1 %>% 
  left_join(diets_warm1, by = c('Predator_Name','Predator_LongName','Cohort','Prey_Name','Prey_LongName')) %>%
  mutate(percent_change = (Prop_experiment - Prop_control) / Prop_control * 100)

# # order
dietchange <- dietchange %>%
  arrange(factor(Predator_Name, levels = pred_names), Cohort)

# fix order of Perdator_Name
dietchange$Predator_Name <- factor(dietchange$Predator_Name, levels = pred_names)

# fix order of age_group
dietchange$Cohort <- factor(dietchange$Cohort,
                            levels = rev(unique(dietchange$Cohort)))

# add predator guild for facets 
dietchange <- dietchange %>%
  left_join(guild_frame, by = c('Predator_Name' = 'fg')) %>%
  left_join(guild_frame, by = c('Prey_Name' = 'fg')) %>%
  rename(Predator_Guild = Guild.x, Prey_Guild = Guild.y)

# fix order of guilds
dietchange$Guild <- factor(dietchange$Predator_Guild,
                           levels = c('Forage fish', 'Flatfish', 'Gadids', 'Seabirds'))

# make some colors

colourCount <- length(unique(dietchange$Prey_Name))
getPalette <- colorRampPalette(brewer.pal(12, "Paired"))

p_diet <- dietchange %>%
  ggplot(aes(x = Predator_LongName, y = percent_change, fill = Prey_LongName))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values=getPalette(colourCount))+
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red')+
  theme_bw()+
  labs(x = '', y = '% change', fill = 'Prey (>1% of diet)')+
  facet_wrap(Predator_Guild~Predator_LongName, scales = 'free_x')
p_diet

ggsave(paste0('output/', 'diets_change_', control, '_vs_', experiment,'.png'),
       p_diet,width = 6.5,height=5)  

# All groups heatmap ------------------------------------------------------

# do it again with all predators
# TODO: probably step 1 above should be a subset of step 2 here, rather than applying the functions twice
preds_to_keep <- pred_codes
pred_names <- grps %>%
  filter(Code %in% preds_to_keep) %>%
  arrange(factor(Code, levels = preds_to_keep)) %>%
  pull(Name)

# apply function to get diet changes
diets_base1 <- compare_diets(diets_base, prednames = pred_names, run = 'control', age_split = 'none')
diets_warm1 <- compare_diets(diets_warm, prednames = pred_names, run = 'experiment', age_split = 'none')

dietchange <- diets_base1 %>% 
  left_join(diets_warm1, by = c('Predator_Name','Predator_LongName','Cohort','Prey_Name','Prey_LongName')) %>%
  mutate(percent_change = (Prop_experiment - Prop_control) / Prop_control * 100)

# # order
dietchange <- dietchange %>%
  arrange(factor(Predator_Name, levels = pred_names), Cohort)

# fix order of Perdator_Name
dietchange$Predator_Name <- factor(dietchange$Predator_Name, levels = pred_names)

# fix order of age_group
dietchange$Cohort <- factor(dietchange$Cohort,
                            levels = rev(unique(dietchange$Cohort)))

# add predator guild for facets 
dietchange <- dietchange %>%
  left_join(guild_frame, by = c('Predator_Name' = 'fg')) %>%
  left_join(guild_frame, by = c('Prey_Name' = 'fg')) %>%
  rename(Predator_Guild = Guild.x, Prey_Guild = Guild.y)

# fudge guild labels to have them horizontal in the figure
dietchange$Predator_Guild <- gsub(' ',
                                  '\n',
                                  dietchange$Predator_Guild)
dietchange$Prey_Guild <- gsub(' ',
                              '\n',
                              dietchange$Prey_Guild)

# not needed for now, by stage is very cluttered and does not help - unless we decide to focus on a few species only
# TODO: add switches to recognize if there are ontogenetic splits and plot accordingly
# construct predator * cohort if we have cohorts
# dietchange <- dietchange %>%
#   mutate(Predator_Name_Stage = paste(Predator_Name, Stage, sep = '_'))



# view, first only vertebrates
p_dietchange_verts_5y <- dietchange %>%
  filter(Predator_Name %in% (vertebrate_groups %>% pull(Name))) %>%
  ggplot()+
  geom_tile(aes(x = Predator_Name, y = Prey_Name, fill = percent_change), color = 'grey')+
  scale_fill_viridis()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  labs(x = 'Predator', y = 'Prey', fill = 'Relative % change\nin diet proportion\n from baseline run')+
  facet_grid(Prey_Guild ~ Predator_Guild, scales = 'free', space = 'free')+
  theme(strip.text.y = element_text(angle = 0))

p_dietchange_verts_5y

ggsave(paste0('output/', 'diets_change_5y_', run_base, '_vs_', run_warm,'.png'),
       p_dietchange_verts_5y, width = 13,height=11)  

# this plot highlights that predation is really low on some groups
# see POP and other rockfish groups, nothing is really eating them
# predation on adult groundfish is probably relatively uncaptured in the AFSC diet data
