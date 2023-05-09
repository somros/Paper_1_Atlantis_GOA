# Alberto Rovellini 
# 5/6/2023
# Making plots of spatial distributions for all species to put in the appendix to the ICES paper
# This is based on S1-S4 and will only provide relative distributions instead of predicitons of biomass / CPUE
# This code should facet plots appropriately depedning on the number of seasons and stages we have different S values for
# This will be run-independent to a good extent, but if we update the distributions (for example by including winter maps) need to update this too

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

# apply function to drop duplicated distributions
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
