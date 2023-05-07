# plot and produce tables for the methods
library(tidyverse)
library(viridis)
library(RColorBrewer)

# pick run
this_run <- 1190

# read in prm file
prm_file <- paste0('../../Parametrization/output_files/data/out_', this_run, '/GOAbioparam_test.prm')
prm <- readLines(prm_file)

# identify rows where the pPREY matrix is
pprey_rows <- grep('pPREY',prm)
first_row <- pprey_rows[2] # discard the first one as it is a message in the GOA prm models
last_row <- pprey_rows[length(pprey_rows)]+2 # adding the vector of values and the empty row at the end

# read the pprey matrix
pprey_matrix <- prm[first_row:last_row]

# alternatively, read pprey matrix directly
# pprey_matrix <- readLines( paste0('out_', this_run, '/pprey_new/pprey_newtmp.prm'))

pprey_names <- pprey_matrix[grep('pPREY', pprey_matrix)] # get name rows
pprey_vals <- pprey_matrix[-grep('pPREY', pprey_matrix)] # get value rows

# doing some cleaning of the text
# clean empty rows
pprey_vals <- pprey_vals[which(nchar(pprey_vals)>0)]

# replace tabs with spaces
pprey_vals <- gsub('\t', ' ', pprey_vals)

# turn the pprey matrix to a data frame to manipulate
val_list <- list()

for(i in 1:length(pprey_vals)){
  
  # split each string assuming that values are separated by a space
  this_pprey_vec <- as.data.frame(t(as.numeric(unlist(strsplit(pprey_vals[i],' ')))))
  
  # drop NAs at the end of the string (cases where there was a string of blanks)
  this_pprey_vec <- this_pprey_vec %>% select_if(~ !any(is.na(.)))
  
  # write out
  val_list[[i]] <- this_pprey_vec
}

val_frame <- bind_rows(val_list) %>%
  set_names(c(grps %>% pull(Code), 'DLsed', 'DRsed', 'DCsed'))

# add column with stage of the prey
val_frame1 <- val_frame2 <- val_frame %>% mutate(pprey = pprey_names,
                                                 tmp = gsub('pPREY','', gsub('  81.*', '', pprey)),
                                                 prey_stage = as.numeric(substr(tmp,1,1)),
                                                 pred_stage = as.numeric(substr(tmp, nchar(tmp), nchar(tmp))),
                                                 pred = gsub('2','', gsub('1','',tmp))) %>%
  select(pred, prey_stage, pred_stage, KWT:DRsed)

# identify verts and inverts
vertnames <- vertebrate_groups %>% pull(LongName)
invertnames <- setdiff((grps %>% pull(LongName)), vertnames)


#diets <- read.csv('../data/goa_pprey_matrix.csv')

# diets %>%
#   filter(grepl('RFD',name)) %>%
#   select(name,SBF)

# plot
# diets <- diets %>%
#   mutate(tmp = substr(name, 6, (nchar(name)-3))) %>%
#   rowwise() %>%
#   mutate(Prey_stage = ifelse(substr(tmp,1,1) == '1', 1, ifelse(substr(tmp,1,1) == '2', 2, NA)),
#          Pred_stage = ifelse(substr(tmp,nchar(tmp),nchar(tmp)) == '1', 1, ifelse(substr(tmp,nchar(tmp),nchar(tmp)) == '2', 2, NA)),
#          Pred_name = ifelse(substr(tmp,1,1) %in% c('1','2'), substr(tmp,2,4), tmp)) %>%
#   select(Pred_name, Pred_stage, Prey_stage, KWT:DRsed) 

# reconstruct invertebrate diets
# diets_inv_tmp <- diets %>% filter(is.na(Pred_stage))
# 
# diets_inv <- diets_inv_tmp[rep(seq_len(nrow(diets_inv_tmp)), each = 4), ]
# diets_inv$Pred_stage <- rep(c(1,2), nrow(diets_inv_tmp)*2)
# diets_inv$Prey_stage <- rep(c(1,1,2,2), nrow(diets_inv_tmp))
# 
# # and now bring together
# diets <- rbind(diets %>% filter(!is.na(Pred_stage)), diets_inv) %>% as_tibble()

# diets %>%
#   filter(Pred_name == "RFD") %>%
#   select(Pred_name:Prey_stage, SBF)

diets <- val_frame1
diets <- diets %>% mutate(pred_stage = replace_na(pred_stage, 2),
                          prey_stage = replace_na(prey_stage, 2)) %>%
  rename(Pred_name = pred)

diets_long <- diets %>%
  pivot_longer(-(Pred_name:pred_stage), names_to = 'Prey_name', values_to = 'ppreyval') %>%
  group_by(Pred_name, pred_stage, prey_stage) %>%
  mutate(Prop = ppreyval / sum(ppreyval) * 100) %>%
  ungroup() %>%
  mutate(Stage = paste0('Prey', prey_stage, ':Predator', pred_stage))
         

# attach long names

diets_long <- diets_long %>%
  left_join(grps %>% select(Code, LongName), by = c('Pred_name' = 'Code')) %>%
  left_join(grps %>% select(Code, LongName), by = c('Prey_name' = 'Code')) %>%
  select(LongName.x, LongName.y, Stage, Prop) %>%
  rename(Pred_name = LongName.x, Prey_name = LongName.y) %>%
  mutate(Prop = na_if(Prop, 0)) %>%
  filter(!is.na(Prey_name))

# p <- diets_long %>% ggplot()+
#   geom_tile(aes(x = Prey_name, y = Pred_name, fill = Prop), color = 'darkgrey')+
#   scale_fill_viridis()+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 60, hjust = 1.05, vjust = 1, size = 11),
#         axis.text.y = element_text(size = 11))+
#   facet_wrap(~Stage)+
#   labs(x = "Prey", y = "Predator",
#        fill = "Ingested\nprey (%)")
# 
# p
# 
# ggsave('complete_dietmatrix.png', p, width = 19, height = 18, units = 'in', dpi = 300)
# 
# # really hard to read as a plot...
# 
# # make a table
# 
# diet_tab <- diets %>%
#   left_join(fg, by = c('Pred_name' = 'Code')) %>%
#   mutate(Pred_stage = str_replace(Pred_stage, '1', 'J'),
#          Pred_stage = str_replace(Pred_stage, '2', 'A'),
#          Prey_stage = str_replace(Prey_stage, '1', 'J'),
#          Prey_stage = str_replace(Prey_stage, '2', 'A')) %>%
#   select(Name, Pred_stage, Prey_stage, KWT:DRsed) %>%
#   set_names(c('Predator','Predator stage','Prey stage'), 
#             fg$Name, 'Carrion_sediments', 'Detritus_labile_sediment', 'Detritus_refractory_sediment') %>%
#   mutate(across(where(is.numeric), ~ .x*10*100))
# 
# # write as csv
# write.csv(diet_tab, 'diets_table.csv', row.names = F)

# Make plots for individual groups for the methods ---------------------------------------
# Separate juveniles from adults

# diets_long %>%
#   filter(Pred_name == 'Rockfish_demersal_shelf' & Prey_name == 'Sablefish')

all_fg <- unique(diets_long$Pred_name)

diets_long <- diets_long %>%
  mutate(Stage = case_when(
    Pred_name %in% invertnames ~ 'Pred:Prey',
    Stage == 'Prey1:Predator1' ~ 'Pred_juv:Prey_juv',
    Stage == 'Prey1:Predator2' ~ 'Pred_adult:Prey_juv',
    Stage == 'Prey2:Predator1' ~ 'Pred_juv:Prey_adult',
    Stage == 'Prey2:Predator2' ~ 'Pred_adult:Prey_adult'
  )) 
  
diets_long$Stage <- factor(diets_long$Stage, levels = c('Pred_juv:Prey_juv',
                                                        'Pred_juv:Prey_adult',
                                                        'Pred_adult:Prey_juv',
                                                        'Pred_adult:Prey_adult',
                                                        'Pred:Prey'))

for (i in 1:length(all_fg)){
  this_fg <- all_fg[i]
  
  this_diet <- diets_long %>% 
    filter(Pred_name == this_fg) %>%
    drop_na() 
  
  colourCount <- length(unique(this_diet$Prey_name)) 
  getPalette <- colorRampPalette(brewer.pal(12, "Paired"))
  
  p <- this_diet %>%
    ggplot()+
    geom_bar(aes(x = Stage, y = Prop, fill = Prey_name), stat = 'identity', position = 'stack')+
    scale_fill_manual(values = getPalette(colourCount))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 20, hjust = 1.05, vjust = 1, size = 11),
          axis.text.y = element_text(size = 11))+
    labs(x = '', y = "Diet preference (%)",
         fill = "Prey")
  
  if(this_diet$Stage[1] == 'Pred:Prey') {
    ggsave(paste('output/diets/',this_fg,'diet.png',sep='_'), p, width = 4, height = 6)
  } else {
    ggsave(paste('output/diets/',this_fg,'diet.png',sep='_'), p, width = 10.5, height = 6)
  }
  
}
