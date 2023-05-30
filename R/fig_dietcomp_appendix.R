# Alberto Rovellini
# 5/8/2023
# Code to generate plots of the diets of each functional group, as they are entered in the PPREY matrix of a run of choice
# To be used in the section of the text (appendix) for each individual group
# Two pathways:
# 1. Diets from a pprey matrix (input). Note that this should be a pre-calibration diet matrix
# 2. Diet compositions as output


# Input from PPREY matrix -------------------------------------------------

# pick run
this_run <- 1190
this_dir <- paste0('../../../GOA/Parametrization/output_files/data/out_', this_run, '/')


# read in prm file
prm_file <- paste0('../../Parametrization/output_files/data/out_', this_run, '/GOAbioparam_test.prm')
prm <- readLines(prm_file)

# identify rows where the pPREY matrix is
pprey_rows <- grep('pPREY',prm)
first_row <- pprey_rows[2] # discard the first one as it is a message in the GOA prm models
last_row <- pprey_rows[length(pprey_rows)]+2 # adding the vector of values and the empty row at the end

# read the pprey matrix
pprey_matrix <- prm[first_row:last_row]

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


# All groups heatmap ------------------------------------------------------

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
    ggsave(paste('output/diets_input/',this_fg,'diet.png',sep='_'), p, width = 4, height = 6)
  } else {
    ggsave(paste('output/diets_input/',this_fg,'diet.png',sep='_'), p, width = 10.5, height = 6)
  }
  
}


# Output from model run ---------------------------------------------------

diets_base <- read.table(paste0(this_dir, 'outputGOA0', this_run, '_testDietCheck.txt'), header = T)

# do it again with all predators
# TODO: probably step 1 above should be a subset of step 2 here, rather than applying the functions twice
preds_to_keep <- pred_codes
pred_names <- grps %>%
  filter(Code %in% preds_to_keep) %>%
  arrange(factor(Code, levels = preds_to_keep)) %>%
  pull(Name)

# apply function to get diet changes
diets_processed <- compare_diets(diets_base, prednames = pred_names, run = this_run, age_split = 'stage')

# # order
diets_processed <- diets_processed %>%
  arrange(factor(Predator_Name, levels = pred_names), Stage)

# fix order of Perdator_Name
diets_processed$Predator_Name <- factor(diets_processed$Predator_Name, levels = pred_names)

# fix order of age_group
diets_processed$Stage <- factor(diets_processed$Stage,
                            levels = rev(unique(diets_processed$Stage)))

# add predator guild for facets 
diets_processed <- diets_processed %>%
  left_join(guild_frame, by = c('Predator_Name' = 'fg')) %>%
  left_join(guild_frame, by = c('Prey_Name' = 'fg')) %>%
  rename(Predator_Guild = Guild.x, Prey_Guild = Guild.y)

# fudge guild labels to have them horizontal in the figure
diets_processed$Predator_Guild <- gsub(' ',
                                  '\n',
                                  diets_processed$Predator_Guild)
diets_processed$Prey_Guild <- gsub(' ',
                              '\n',
                              diets_processed$Prey_Guild)

# make barcharts instead of heatmap

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
    ggsave(paste('output/diets_input/',this_fg,'diet.png',sep='_'), p, width = 4, height = 6)
  } else {
    ggsave(paste('output/diets_input/',this_fg,'diet.png',sep='_'), p, width = 10.5, height = 6)
  }
  
}



# this plot highlights that predation is really low on some groups
# see POP and other rockfish groups, nothing is really eating them
# predation on adult groundfish is probably relatively uncaptured in the AFSC diet data
