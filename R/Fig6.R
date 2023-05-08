# Alberto Rovellini
# 5/8/2023
# Code to create a figure illustrating changes in diets between runs for the ICES paper
# all the extraction code is take from Owen and PY's code

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
  left_join(guild_frame, by = c('Predator_Name' = 'fg')) 

# fix order of guilds
dietchange$Guild <- factor(dietchange$Guild,
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
  facet_wrap(Guild~Predator_LongName, scales = 'free_x')
p_diet

ggsave(paste0('output/', 'diets_change_', control, '_vs_', experiment,'.png'),
       p_diet,width = 6.5,height=5)  
