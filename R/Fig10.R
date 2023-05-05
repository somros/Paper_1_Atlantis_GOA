# Alberto Rovellini
# 5/5/2023
# Code to create Fig. 9 for ICES paper
# Fig. 10 changes in diets
# all the extraction code is take from Owen and PY's code
# Plot this only for full forcings (HW temperature and production)

# producers and detritus
prods_gtype <- c('PHYTOBEN', 'LG_PHY', 'SM_PHY', 'SED_BACT', 'PL_BACT', 'CARRION', 'LAB_DET', 'REF_DET')
# predators:
preds_gtype <- setdiff((grps %>% pull(GroupType) %>% unique()), prods_gtype)

# and corresponding codes
pred_codes <- grps %>% filter(GroupType %in% preds_gtype) %>% pull(Code)

# age class at maturity to split age classes between juveniles and adults
agemat <- read.csv('../data/agemat.csv', header = T)

# these are the predators of interest (Codes)
preds_to_keep <- pred_codes
pred_names <- grps %>%
  filter(Code %in% preds_to_keep) %>%
  arrange(factor(Code, levels = preds_to_keep)) %>%
  pull(Name)
#prey_to_keep <- c('ZS','ZM','ZL','EUP')

# read diet data
diets_base <- read.table(paste0(dir_base, 'outputGOA0', run_base, '_testDietCheck.txt'), header = T)
diets_hw <- read.table(paste0(dir_hw_prod, 'outputGOA0', run_hw_prod, '_testDietCheck.txt'), header = T)

# apply function to get diet changes
diets_base1 <- compare_diets(diets_base, prednames = pred_names, run = 'base', age_split = 'stage')
diets_hw1 <- compare_diets(diets_hw, prednames = pred_names, run = 'hw_prod', age_split = 'stage')

dietchange <- diets_base1 %>% 
  left_join(diets_hw1, by = c('Predator_Name','Stage','Prey_Name')) %>%
  mutate(percent_change = (Prop_hw_prod-Prop_base)/Prop_base*100)

# # order
dietchange <- dietchange %>%
  arrange(factor(Predator_Name, levels = pred_names), Stage)

# fix order of Perdator_Name
dietchange$Predator_Name <- factor(dietchange$Predator_Name, levels = pred_names)

# fix order of age_group
dietchange$Stage <- factor(dietchange$Stage,
                                levels = rev(unique(dietchange$Stage)))

# add facets 
dietchange <- dietchange %>%
  left_join(guild_frame, by = c('Predator_Name' = 'fg')) %>%
  left_join(guild_frame, by = c('Prey_Name' = 'fg')) %>%
  rename(Predator_Guild = Guild.x, Prey_Guild = Guild.y)


# construct predator * cohort if we have cohorts
dietchange <- dietchange %>%
  mutate(Predator_Name_Stage = paste(Predator_Name, Stage, sep = '_'))

# view, first only vertebrates
p_dietchange_verts_5y <- dietchange %>%
  filter(Predator_Name %in% (vertebrate_groups %>% pull(Name))) %>%
  ggplot()+
  geom_tile(aes(x = Predator_Name_Stage, y = Prey_Name, fill = percent_change), color = 'grey')+
  scale_fill_viridis()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  labs(x = 'Predator by stage', 'Prey', fill = 'Relative % change\nin diet proportion\n from baseline run')+
  facet_grid(Prey_Guild ~ Predator_Guild, scales = 'free')
p_dietchange_verts_5y

ggsave(paste0('output/', 'diets_change_5y_', run_base, '_vs_', run_hw,'.png'),
       p_dietchange_verts_5y, width = 13,height=11)  
