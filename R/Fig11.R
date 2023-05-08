# Alberto Rovellini 
# 5/6/2023
# Making plots of spatial distributions for all species to put in the document

# read S1-S4
dists_vert <- read.csv('../../Parametrization/build_init_prm_10COHORTS/data/seasonal_distribution_POP.csv')
dists_invert <- read.csv('../../Parametrization/build_init_prm_10COHORTS/data/seasonal_distribution_inverts_POP.csv')
dists <- cbind(dists_vert, dists_invert)

# reshape
dists_long <- dists %>%
  mutate(box_id = 0:108) %>%
  pivot_longer(-box_id) %>%
  mutate(seas = substr(name, (nchar(name)-1), (nchar(name))),
         stage = substr(name, (nchar(name)-3), (nchar(name)-3)),
         name = substr(name, 1, (nchar(name)-5))) %>%
  left_join(grps %>% select(Code, Name, LongName), by = c('name'='Name'))

# fix pollock from the get go, we now use adults distributions for juveniles so drop juveniles
dists_long <- dists_long %>%
  filter(!(name == 'Pollock' & stage == 'J'))

# set group names 
vertnames <- intersect(vertebrate_groups %>% pull(Name), unique(dists_long$name))
invertnames <- intersect(setdiff((grps %>% pull(Name)), vertnames), unique(dists_long$name))

# make function that checks if: 
# 1. there are seasonal differences within stages, if not collapse into one
# 2. there are ontogenetic differences, if not collapse into one

handle_dists <- function(this_species, isvert = TRUE){
  
  print(paste('Doing', this_species, sep = " "))
  
  # work on one species at a time
  dat <- dists_long %>% filter(name == this_species)
  
  # within a stage, check if vectors are identical across seasons (expect this to be the case for the vast majority)
  if(isvert){
    stgs <- c('J','A')
  } else {
    stgs <- 'A'
  }
  
  drop_seas <- rep(NA, length(stgs))
  
  for(stg in 1:length(stgs)){
    
    dat1 <- dat %>%
      filter(stage == stgs[stg]) %>%
      pivot_wider(id_cols = box_id, names_from = seas, values_from = value)
    
    drop_seas[stg] <- all(sapply(list(dat1$S2, dat1$S3, dat1$S4), FUN = identical, dat1$S1))
    
  }
  
  # within a season, see if there are ontogenetic differences
  # only for vertebrates
  if(isvert){
    
    seas <- c('S1','S2','S3','S4')
    drop_stage <- rep(NA, length(seas))
    
    for(s in 1:length(seas)){
      
      dat1 <- dat %>%
        filter(seas == seas[s]) %>%
        pivot_wider(id_cols = box_id, names_from = stage, values_from = value)
      
      drop_stage[s] <- identical(dat1$A, dat1$J)
      
    }
    
  } else {
    drop_stage <- TRUE
  }
  
  # if all seasons identical keep only S3
  if(all(drop_seas)){
    dat2 <- dat %>% filter(seas == 'S3') %>% mutate(seas = 'All seasons')
  } else {
    dat2 <- dat
  }
  if(all(drop_stage)){
    dat3 <- dat2 %>% filter(stage == 'A') %>% mutate(stage = 'All stages')
  } else {
    dat3 <- dat2
  }
  
  return(dat3)
} 

vertsp <- bind_rows(lapply(vertnames, handle_dists, isvert = TRUE))
invertsp <- bind_rows(lapply(invertnames, handle_dists, isvert = FALSE))
allsp <- rbind(vertsp, invertsp)

# Change A and J with juvenile and adult and order J first
allsp <- allsp %>%
  mutate(stage = case_when(
    stage == 'J' ~ 'Juvenile',
    stage == 'A' ~ 'Adult',
    .default = stage
  ))

# make plot
longnames <- unique(allsp$LongName)
for(i in 1:length(longnames)){
  df <- goa_sf %>%
    full_join (allsp %>% filter(LongName == longnames[i]), by=  'box_id') %>%
    rowwise() %>%
    mutate(value = ifelse(boundary == TRUE, NA, value)) %>%
    ungroup()
  
  stg <- unique(df$stage)
  seas <- unique(df$seas)
  
  p <- df %>%
    ggplot()+
    geom_sf(aes(fill=value), color=NA)+
    scale_fill_viridis()+
    geom_sf(data = coast_sf)+
    theme_bw()+
    labs(title = '', x='',y='',fill='Biomass\nproportion')
  
    if(length(stg) > 1 & length(seas) == 1){
      
      p <- p+facet_wrap(~stage, ncol = 1)
      ggsave(paste0('output/s/', longnames[i], '.png'),p,width = 6,height=4)
      
    } else if (length(stg) == 1 & length(seas) > 1) {
      
      p <- p+facet_wrap(~seas, ncol = 2)
      ggsave(paste0('output/s/', longnames[i], '.png'),p,width = 6,height=3)
      
    } else {
      
      p <- p
      ggsave(paste0('output/s/', longnames[i], '.png'),p,width = 6,height=2.4)
      
    }
}


# now make plots
# plotgrid <- function(df,name){
#   ggplot(df)+
#     geom_sf(aes(fill=S), color=NA)+
#     scale_fill_viridis()+
#     theme_bw()+
#     labs(title = name, x='Lon',y='Lat',fill='S')
# }
# 
# nested_tmp <- atlantis_box %>% select(box_id) %>%
#   full_join(all_groups,by='box_id') %>%
#   mutate(groupstage=paste0(group,stage)) %>%
#   group_by(groupstage) %>%
#   nest() %>%
#   mutate(plots = purrr::map2(data, groupstage, plotgrid))
# 
# gridExtra::grid.arrange(grobs = nested_tmp$plots, ncol = 2) # for doc
# 
# #for saving
# p <- gridExtra::arrangeGrob(grobs = nested_tmp$plots, ncol = 2)
# ggsave(paste(outdir, 's1_s4_GOA_V3.png', sep = '/'),p,width=10,height = 60,limitsize=F)
