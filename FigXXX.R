# deal with DetailedDietCheck
library(tidyverse)
library(circlize)
library(RColorBrewer)

run_base <- 1044
run_hw <- 1076

# set paths to directories
dir_base <- paste0('../../GOA/Parametrization/output_files/data/out_', run_base, '/')
dir_hw <- paste0('../../GOA/Parametrization/output_files/data/out_', run_hw, '/')

# read fg
atlantis_fg <- read.csv('../data/GOA_Groups.csv', header = T)

# producers and detritus
prods_gtype <- c('PHYTOBEN', 'LG_PHY', 'SM_PHY', 'SED_BACT', 'PL_BACT', 'CARRION', 'LAB_DET', 'REF_DET')
# predators:
preds_gtype <- setdiff((atlantis_fg %>% pull(GroupType) %>% unique()), prods_gtype)

# and corresponding codes
pred_codes <- atlantis_fg %>% filter(GroupType %in% preds_gtype) %>% pull(Code)

num_preds <- length(pred_codes) #70
num_cohorts <- sum(atlantis_fg %>% filter(Code %in% pred_codes) %>% pull(NumCohorts)) #445
num_boxes <- 109
num_layers <- 7
num_years <- 40

# tot rows per year
rpy <- num_cohorts*num_boxes*num_layers
rpy

# these are the predators of interest (Codes)
preds_to_keep <- c('CAP','SAN','HER','ATF','POL','COD','BDF','BSF')
prey_to_keep <- c('ZS','ZM','ZL','EUP')

to_skip <- 1
to_read <- rpy * (num_years / 2)

# create empty list to fill
ddc_list <- list()

# first read the header, you will need it later
ddc_header <- unlist(read.table(paste0(dir_hw, 'outputGOA01076_testDetailedDietCheck.txt'),
                  nrow = 1,
                  header = F))

# now loop over two chunks of the file (this is not a long-term solution)
for(i in 1:2){
  
  #print(paste('Doing year', i))

  ddc <- read.table(paste0(dir_hw, 'outputGOA01076_testDetailedDietCheck.txt'),
                    nrow = to_read,
                    skip = to_skip,
                    header = F)
  
  colnames(ddc) <- ddc_header
  
  ddc_short <- ddc %>% # removing the spatial aspect
    filter(Predator %in% c(prey_to_keep, preds_to_keep)) %>%
    group_by(Time, Predator, Cohort) %>%
    summarise(across(where(is.numeric), sum)) %>%
    dplyr::select(-Box, - Layer) %>%
    ungroup()
  
  # write to list
  ddc_list[[i]] <- ddc_short
  
  # remove ddc and free up RAM
  rm(list = c('ddc','ddc_short'))
  gc()
  # update counter
  to_skip <- to_skip + to_read
 
}

ddc_df <- bind_rows(ddc_list)

# How do we best represent this? Perhaps a Chordis plot?
# EDIT after trying:
# Probably not a good idea, it is rather misleading and not easy to execute with the detailed diet matrix

# prey of interest to have in the plot:
# df_for_chord <- ddc_df[,colnames(ddc_df) %in% c('Time','Predator','Cohort', preds_to_keep, prey_to_keep)]
#   
# df_for_chord <- df_for_chord %>%
#   ungroup() %>%
#   filter(Time == 365) %>%
#   dplyr::select(-c(Time, Cohort)) %>%
#   group_by(Predator) %>%
#   summarise(across(where(is.numeric), sum)) %>%
#   ungroup() %>%
#   arrange(Predator) %>%
#   select(Predator, df_for_chord$Predator) %>%
#   left_join((atlantis_fg %>% select(Code, Name)), by = c('Predator'='Code')) %>%
#   select(-Predator) %>% 
#   column_to_rownames("Name") %>% 
#   mutate(across(where(is.numeric), log1p)) %>%
#   as.matrix()
# 
# png('chord_hw_t0.png', width = 15, height = 15, units = 'in', res = 600)
# 
# par(cex=1.5, mar = c(0,0,0,0))
# 
# chordDiagram(df_for_chord, 
#              directional = 1,
#              direction.type = c("diffHeight", "arrows"),
#              link.arr.type = "big.arrow",
#              annotationTrack = "grid", 
#              reduce = 0,
#              preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(t))))))
# # we go back to the first track and customize sector labels
# circos.track(track.index = 1, panel.fun = function(x, y) {
#   circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
#               facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
# }, bg.border = NA
# )

# dev.off()
