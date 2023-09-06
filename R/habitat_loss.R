# Alberto Rovellini
# 8/16/2023
# Code to analyse the loss in viable habitat between warm and cold scenario
# This is to provide a quantitative corollary to the spatial analysis, as requested by Joe under review

print('Doing habitat loss')

# netcdf output file from warm run
out_fl_warm <- paste0(dir_warm, 'outputGOA0', run_warm, '_test.nc')
out_warm <- tidync(out_fl_warm)
this_nc_warm <- ncdf4::nc_open(out_fl_warm)

# get temp
temp_array_raw <- temp_array <- ncdf4::ncvar_get("Temp", nc=this_nc_warm)

dim_lyr <- dim(temp_array)[1]
dim_box <- dim(temp_array)[2]
dim_t <- dim(temp_array)[3]

# turn the array to a 2D data frame, add box and time dimensions, etc.
dim(temp_array) <- c(dim_lyr, dim_box * dim_t)

temp_df <- temp_array %>%
  t() %>%
  data.frame() %>%
  mutate(box_id = rep(1:dim_box, dim_t),
         ts = rep(1:dim_t, each = dim_box)) %>%
  mutate(box_id = box_id - 1) %>%
  filter(ts > 1) %>% # here 1 is the initial conditions, which immdiately get replaced
  mutate(ts = ts - 1,
         ts = ts / 5) # change to years

colnames(temp_df) <-   c(paste0('lyr',6:1), 'sed', 'box_id', 'ts')

# Spatial delta in temperature between cold and warm years ----------------

# This works now because we are looping a cold year for 30 years and then a warm year for 20 years
# So, we can compare within the same run (equivalent to comparing the last 20 years between warm and cold runs)

temp_df <- temp_df %>%
  dplyr::select(ts,box_id,lyr1,sed) %>% # sed equates to the deepest, lyr1 is the surface
  pivot_longer(-c(ts,box_id), names_to = 'lyr', values_to = 'temp') %>%
  rowwise() %>%
  mutate(period = ifelse(ts >= spinup_length, 'warm', 'cold')) %>% # change these when we have the real forcings
  filter(ts %in% seq(seas,50,1)) %>% # this is July, for September use 0.8. This will matter for spatial patterns
  group_by(period, box_id, lyr) %>%
  summarize(mean_temp = mean(temp)) %>% # means by box per period (pre and during warm period)
  ungroup()

# now read in s1-s4 for all species
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

# now read in the thermal niches
niche <- read.csv('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/thermal_responses/thermal_niches_aquamaps_0_100_percentiles.csv')

# added for AMSS 1/17/2023
# Change COD, POL, ATF, HAL so that max is same as the maximum in the bioenergetics
niche$max[niche$Code=='POL'] <- 15
niche$max[niche$Code=='COD'] <- 21
niche$max[niche$Code=='ATF'] <- 26
niche$max[niche$Code=='HAL'] <- 18

# loop over all species. This only makes sense for species that have thermal niches
species_to_do <- unique(niche$Code)
sp_list <- list(NA, length = length(species_to_do))

for(i in 1:length(species_to_do)){
  
  this_species <- species_to_do[i]
  
  print(paste('Doing ', this_species))
  
  # S
  # only do adults or 'All stage', and only summer or 'All seasons'
  this_s <- allsp %>%
    filter(Code == this_species, seas %in% c('S3','All seasons'), stage %in% c('Adult','All stages'))
  
  # join niche
  this_data <- this_s %>%
    left_join(niche, by = 'Code') %>%
    select(LongName, flagdemXXX, min, max, box_id, value)
  
  # now join with temp, cold and warm
  if(unique(this_data$flagdemXXX == 0)) {
    temp_dat <- temp_df %>% 
      filter(lyr == 'lyr1') %>%
      select(-lyr) %>%
      pivot_wider(names_from = period, values_from = mean_temp)
  } else {
    temp_dat <- temp_df %>% 
      filter(lyr == 'sed') %>%
      select(-lyr) %>%
      pivot_wider(names_from = period, values_from = mean_temp)
  }
  
  # add column to see if a given box is within the limits when cold and when warm
  prop_cold <- this_data %>%
    left_join(temp_dat %>% select(-warm), by = 'box_id') %>%
    rowwise() %>%
    mutate(viable = ifelse(cold <= max & cold >= min, 1, 0)) %>%
    ungroup() %>%
    filter(viable > 0) %>%
    pull(value) %>%
    sum()
  
  prop_warm <- this_data %>%
    left_join(temp_dat %>% select(-cold), by = 'box_id') %>%
    rowwise() %>%
    mutate(viable = ifelse(warm <= max & warm >= min, 1, 0)) %>%
    ungroup() %>%
    filter(viable > 0) %>%
    pull(value) %>%
    sum()
  
  sp_list[[i]] <- data.frame('Code' = this_species, prop_cold, prop_warm)
  
}

sp_frame <- bind_rows(sp_list)

sp_frame <- sp_frame %>% 
  mutate(propchange = (prop_warm-prop_cold)/prop_cold*100) %>%
  left_join(grps %>% select(Code, LongName), by = 'Code') %>%
  arrange(-propchange)

# addition 9/4/2023
# read in temperature input files, instead of output, so that we capture all time steps
# pull surface and bottom
# get max and min in each box
# netcdf output file from warm run
out_fl_cold <- '../data/goa_temp_1999.nc'
out_cold <- tidync(out_fl_cold)
this_nc_cold <- ncdf4::nc_open(out_fl_cold)

out_fl_warm <- '../data/goa_temp_2014.nc'
out_warm <- tidync(out_fl_warm)
this_nc_warm <- ncdf4::nc_open(out_fl_warm)

# get temp
temp_array_raw_cold <- temp_array_cold <- ncdf4::ncvar_get("temperature", nc=this_nc_cold)
temp_array_raw_warm <- temp_array_warm <- ncdf4::ncvar_get("temperature", nc=this_nc_warm)

# same for both
dim_lyr <- dim(temp_array_warm)[1]
dim_box <- dim(temp_array_warm)[2]
dim_t <- dim(temp_array_warm)[3]

# turn the array to a 2D data frame, add box and time dimensions, etc.
dim(temp_array_cold) <- c(dim_lyr, dim_box * dim_t)
dim(temp_array_warm) <- c(dim_lyr, dim_box * dim_t)

temp_df_cold <- temp_array_cold %>%
  t() %>%
  data.frame() %>%
  mutate(box_id = rep(1:dim_box, dim_t),
         ts = rep(1:dim_t, each = dim_box)) %>%
  mutate(box_id = box_id - 1)

colnames(temp_df_cold) <-   c(6:1, 7, 'box_id', 'ts') # 7 is sediment, 6 is deepest, surface changes

temp_df_warm <- temp_array_warm %>%
  t() %>%
  data.frame() %>%
  mutate(box_id = rep(1:dim_box, dim_t),
         ts = rep(1:dim_t, each = dim_box)) %>%
  mutate(box_id = box_id - 1)

colnames(temp_df_warm) <-   c(6:1, 7, 'box_id', 'ts') # 7 is sediment, 6 is deepest, surface changes

# need to manipulate layers
temp_df_1_cold <- temp_df_cold %>%
  pivot_longer(-c(ts,box_id), names_to = 'layer', values_to = 'value') %>%
  drop_na() %>%
  mutate(layer = as.numeric(layer)) %>%
  group_by(ts, box_id) %>%
  mutate(new_layer = (layer - min(layer) + 1)) %>%
  filter(new_layer == max(new_layer) | new_layer == min(new_layer)) %>%
  ungroup() %>%
  arrange(ts, box_id, layer) %>%
  filter(box_id %in% setdiff(box_id, boundary_boxes)) # discard boundary boxes

temp_df_1_warm <- temp_df_warm %>%
  pivot_longer(-c(ts,box_id), names_to = 'layer', values_to = 'value') %>%
  drop_na() %>%
  mutate(layer = as.numeric(layer)) %>%
  group_by(ts, box_id) %>%
  mutate(new_layer = (layer - min(layer) + 1)) %>%
  filter(new_layer == max(new_layer) | new_layer == min(new_layer)) %>%
  ungroup() %>%
  arrange(ts, box_id, layer) %>%
  filter(box_id %in% setdiff(box_id, boundary_boxes)) # discard boundary boxes

# get min per box
surface_min_cold <- temp_df_1_cold %>% filter(new_layer == 1) %>% group_by(box_id) %>% slice_min(value) %>% select(box_id, value) %>% mutate(lyr = 'surface', run = 'cold', type = 'min')
surface_max_cold <- temp_df_1_cold %>% filter(new_layer == 1) %>% group_by(box_id) %>% slice_max(value) %>% select(box_id, value) %>% mutate(lyr = 'surface', run = 'cold', type = 'max')
bottom_min_cold <- temp_df_1_cold %>% filter(layer == 7) %>% group_by(box_id) %>% slice_min(value) %>% select(box_id, value) %>% mutate(lyr = 'bottom', run = 'cold', type = 'min')
bottom_max_cold <- temp_df_1_cold %>% filter(layer == 7) %>% group_by(box_id) %>% slice_max(value) %>% select(box_id, value) %>% mutate(lyr = 'bottom', run = 'cold', type = 'max')

surface_min_warm <- temp_df_1_warm %>% filter(new_layer == 1) %>% group_by(box_id) %>% slice_min(value) %>% select(box_id, value) %>% mutate(lyr = 'surface', run = 'warm', type = 'min')
surface_max_warm <- temp_df_1_warm %>% filter(new_layer == 1) %>% group_by(box_id) %>% slice_max(value) %>% select(box_id, value) %>% mutate(lyr = 'surface', run = 'warm', type = 'max')
bottom_min_warm <- temp_df_1_warm %>% filter(layer == 7) %>% group_by(box_id) %>% slice_min(value) %>% select(box_id, value) %>% mutate(lyr = 'bottom', run = 'warm', type = 'min')
bottom_max_warm <- temp_df_1_warm %>% filter(layer == 7) %>% group_by(box_id) %>% slice_max(value) %>% select(box_id, value) %>% mutate(lyr = 'bottom', run = 'warm', type = 'max')

# bind all together
temp_lims <- rbind(surface_min_cold,surface_max_cold,surface_min_warm,surface_max_warm,
                   bottom_min_cold, bottom_max_cold, bottom_min_warm,bottom_max_warm)

# now need to bring in the spatial distributions for each species
# now read in s1-s4 for all species
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

# now read in the thermal niches
niche <- read.csv('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/thermal_responses/thermal_niches_aquamaps_0_100_percentiles.csv')

# added for AMSS 1/17/2023
# Change COD, POL, ATF, HAL so that max is same as the maximum in the bioenergetics
niche$max[niche$Code=='POL'] <- 15
niche$max[niche$Code=='COD'] <- 21
niche$max[niche$Code=='ATF'] <- 26
niche$max[niche$Code=='HAL'] <- 18

# loop over all species. This only makes sense for species that have thermal niches
species_to_do <- unique(niche$Code)
sp_list <- list(NA, length = length(species_to_do))

for(i in 1:length(species_to_do)){
  
  this_species <- species_to_do[i]
  
  print(paste('Doing ', this_species))
  
  # S
  # only do adults or 'All stage', and only summer or 'All seasons'
  this_s <- allsp %>%
    filter(Code == this_species, seas %in% c('S3','All seasons'), stage %in% c('Adult','All stages'))
  
  # join niche
  this_data <- this_s %>%
    left_join(niche, by = 'Code') %>%
    select(LongName, flagdemXXX, min, max, box_id, value)
  
  # now join with temp, cold and warm
  if(unique(this_data$flagdemXXX == 0)) {
    temp_dat <- temp_lims %>% 
      filter(lyr == 'surface') %>%
      select(-lyr) %>%
      distinct()
  } else {
    temp_dat <- temp_lims %>% 
      filter(lyr == 'bottom') %>%
      select(-lyr) %>%
      distinct()
  }
  
  # wrangle so that we have the form needed for below
  temp_dat1 <- temp_dat %>%
    pivot_wider(id_cols = c(box_id, run), values_from = value, names_from = type)
  
  # add column to see if a given box is within the limits when cold and when warm
  prop_cold <- this_data %>%
    left_join(temp_dat1 %>% filter(run == 'cold'), by = 'box_id') %>%
    drop_na() %>% # drop boundary boxes
    rowwise() %>%
    mutate(viable = ifelse(min.x <= min.y & max.x >= max.y, 1, 0)) %>%
    ungroup() %>%
    filter(viable > 0) %>%
    pull(value) %>%
    sum()
  
  prop_warm <- this_data %>%
    left_join(temp_dat1 %>% filter(run == 'warm'), by = 'box_id') %>%
    drop_na() %>% # drop boundary boxes
    rowwise() %>%
    mutate(viable = ifelse(min.x <= min.y & max.x >= max.y, 1, 0)) %>% # you want min of spawning < min temp and max spawning > max temp to move in there
    ungroup() %>%
    filter(viable > 0) %>%
    pull(value) %>%
    sum()
  
  sp_list[[i]] <- data.frame('Code' = this_species, prop_cold, prop_warm)
  
}

sp_frame <- bind_rows(sp_list)

sp_frame <- sp_frame %>% 
  mutate(propchange = (prop_warm-prop_cold)/prop_cold*100) %>%
  left_join(grps %>% select(Code, LongName), by = 'Code') %>%
  arrange(-propchange)
