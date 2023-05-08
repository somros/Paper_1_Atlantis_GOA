# Alberto Rovellini
# 5/5/2023
# Code to create a figure illustrating changes in diets for all groups as a heatmap
# all the extraction code is take from Owen and PY's code

# these are the predators of interest (Codes)
preds_to_keep <- pred_codes
pred_names <- grps %>%
  filter(Code %in% preds_to_keep) %>%
  arrange(factor(Code, levels = preds_to_keep)) %>%
  pull(Name)

# read diet data
# read again in case we are only compiling this code (it is fast)
diets_base <- read.table(paste0(dir_base, 'outputGOA0', run_base, '_testDietCheck.txt'), header = T)
diets_hw <- read.table(paste0(dir_hw_prod, 'outputGOA0', run_hw_prod, '_testDietCheck.txt'), header = T)


