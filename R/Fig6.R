# Alberto Rovellini
# 4/3/2023
# Code to create Fig. 6 for ICES paper
# Fig. 6 changes in diets
# all the extraction code is take from Owen and PY's code
# Plot this ponly for full forcings (HW temperature and production)

# producers and detritus
prods_gtype <- c('PHYTOBEN', 'LG_PHY', 'SM_PHY', 'SED_BACT', 'PL_BACT', 'CARRION', 'LAB_DET', 'REF_DET')
# predators:
preds_gtype <- setdiff((grps %>% pull(GroupType) %>% unique()), prods_gtype)

# and corresponding codes
pred_codes <- grps %>% filter(GroupType %in% preds_gtype) %>% pull(Code)

# these are the predators of interest (Codes)
preds_to_keep <- c('CAP','SAN','HER','ATF','POL','COD','BDF','BSF')
pred_names <- grps %>% 
  filter(Code %in% preds_to_keep) %>% 
  arrange(factor(Code, levels = preds_to_keep)) %>%
  pull(Name)
#prey_to_keep <- c('ZS','ZM','ZL','EUP')

diets_base <- read.table(paste0(dir_base, 'outputGOA0', run_base, '_testDietCheck.txt'), header = T)
diets_hw <- read.table(paste0(dir_hw_prod, 'outputGOA0', run_hw_prod, '_testDietCheck.txt'), header = T)

# apply function to get diet changes
diets_base1 <- compare_diets(diets_base, prednames = pred_names, run = 'base', age_split = 'none')
diets_hw1 <- compare_diets(diets_hw, prednames = pred_names, run = 'hw_prod', age_split = 'none')

dietchange <- diets_base1 %>% 
  left_join(diets_hw1, by = c('Predator_Name','Cohort','Prey_Name')) %>%
  mutate(percent_change = (Prop_hw_prod-Prop_base)/Prop_base*100)

# # order
dietchange <- dietchange %>%
  arrange(factor(Predator_Name, levels = pred_names), Cohort)

# fix order of Perdator_Name
dietchange$Predator_Name <- factor(dietchange$Predator_Name, levels = pred_names)

# fix order of age_group
dietchange$Cohort <- factor(dietchange$Cohort,
                                levels = rev(unique(dietchange$Cohort)))

# add facets 
dietchange <- dietchange %>%
  rowwise() %>%
  mutate(Guild = case_when(
    Predator_Name %in% c('Capelin', 'Sandlance', 'Herring') ~ 'Forage fish',
    Predator_Name %in% c('Arrowtooth_flounder', 'Pollock', 'Cod') ~ 'Groundfish',
    Predator_Name %in% c('Seabird_dive_fish', 'Seabird_surface_fish') ~ 'Seabirds'
  )) %>%
  ungroup()

# construct predator * cohort if we have cohorts
# THIS BREAKS IT, leave for now (not using a heatmap here)
# dietchange <- dietchange %>%
#   rowwise() %>%
#   mutate(Predator_Name_Cohort = ifelse(is.na(Cohort), as.character(Predator_Name), paste(Predator_Name, Cohort, sep = '_')))

# view
# pp <- dietchange %>%
#   ggplot()+
#   geom_tile(aes(x = Prey_Name, y = Predator_Name_Cohort, fill = percent_change))+
#   scale_fill_viridis()+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
#   facet_grid(rows = 'Guild', scales = 'free_y')
# pp

# make some colors

colourCount <- length(unique(dietchange$Prey_Name))
getPalette <- colorRampPalette(brewer.pal(12, "Paired"))

p_diet <- dietchange %>%
  ggplot(aes(x = Predator_Name, y = percent_change, fill = Prey_Name))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values=getPalette(colourCount))+
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red')+
  theme_bw()+
  labs(x = '', y = '% change', fill = 'Prey (>1% of diet)')+
  facet_wrap(Guild~Predator_Name, scales = 'free_x')
p_diet

ggsave(paste0('output/', 'diets_change_', run_base, '_vs_', run_hw,'.png'),
       p_diet,width = 6.5,height=5)  
