# Alberto Rovellini
# 5/8/2023
# Code to create Introduction and Methods figures for ICES paper
# 1. Atlantis GOA model domain
# 2. Temperature values over time (mean for model domain)
# 3. Delta temperature in space between warm and cold regimes
# 4. Temperature across the model domain (seasonal averages)
# 5. CEATTLE bioenergetics
# 6. Thermal windows from Aquamaps

# Plot model domain -------------------------------------------------------

p_geometry <- goa_sf %>%
  ggplot()+
  geom_sf(aes(fill=botz))+
  #scale_fill_viridis()+
  scale_fill_gradient(low='navyblue', high='cadetblue1')+
  geom_sf(data=coast_sf, fill = 'grey')+
  coord_sf()+
  #geom_sf_text(aes(label=.bx0))+
  theme_bw()+
  labs(fill = 'Depth (m)')#, title = 'Spatial domain of Atlantis Gulf of Alaska')
p_geometry
ggsave(paste0('output/', 'goa_geometry.png'), p_geometry, width = 8, height = 3, dpi = 600)


# Plot time series of temperature in warm run -----------------------------

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

# plot time series
p_ts <- temp_df %>%
  dplyr::select(ts,box_id,lyr1,sed) %>%
  pivot_longer(-c(ts,box_id), names_to = 'lyr', values_to = 'temp') %>%
  rowwise() %>%
  mutate(temp = ifelse(box_id %in% boundary_boxes, NA, temp)) %>% # turn boundary boxes to NA temp, we do not want those to confuse the average plot
  ungroup() %>%
  left_join(areas, by = c('box_id'='b')) %>% # add areas for weighting
  group_by(ts, lyr) %>%
  summarise(temp = weighted.mean(temp, area, na.rm = T)) %>%
  ungroup() %>%
  filter(lyr == 'sed') %>%
  ggplot()+
  geom_line(aes(x = ts, y = temp), linewidth = 0.8)+
  theme_bw()+
  labs(x = 'Year', y = 'SBT (\u00B0C)')
p_ts
ggsave(paste0('output/', 'goa_sbt_ts_', run_warm, '.png'), p_ts, width = 5, height = 1.2, dpi = 600)

# # plot by box to compare with input files viewed in Shane's code
# temp_df %>%
#   dplyr::select(ts,box_id,lyr1) %>%
#   filter(ts <= 1) %>%
#   ggplot()+
#   geom_line(aes(x = ts, y = lyr1, color = factor(box_id)))#+
#   #guides(color = "none")


# Spatial delta in temperature between cold and warm years ----------------

# This works now because we are looping a cold year for 30 years and then a warm year for 20 years
# So, we can compare within the same run (equivalent to comparing the last 20 years between warm and cold runs)

spinup_length <- 30
seas <- 0.6 # one of 0.2, 0.4, 0.6, 0.8, 1 if we use 73 days for the output frequency on run.prm

temp_df <- temp_df %>%
  dplyr::select(ts,box_id,lyr1,sed) %>%
  pivot_longer(-c(ts,box_id), names_to = 'lyr', values_to = 'temp') %>%
  rowwise() %>%
  mutate(period = ifelse(ts >= spinup_length, 'warm', 'cold')) %>% # change these when we have the real forcings
  filter(ts %in% seq(seas,40,1)) %>% # this is July, for September use 0.8. This will matter for spatial patterns
  group_by(period, box_id, lyr) %>%
  summarize(mean_temp = mean(temp)) %>% # means by box per period (pre and during heatwave)
  ungroup()

# now join to spatial data set
goa_temp_sf <- goa_sf %>%
  left_join(temp_df, by = 'box_id') %>%
  rowwise() %>%
  mutate(mean_temp = ifelse(boundary == "TRUE" || botz >= 0, NA, mean_temp)) %>%
  ungroup() %>%
  dplyr::select(period, box_id, lyr, mean_temp) %>%
  pivot_wider(names_from = 'period', values_from = 'mean_temp') %>%
  mutate(delta_temp = warm - cold)

# plot delta
# surface
p_delta_s <- ggplot()+
  geom_sf(data = goa_temp_sf %>% filter(lyr == 'lyr1'), aes(fill = delta_temp))+
  scale_fill_viridis()+
  geom_sf(data = coast_sf)+
  theme_bw()+
  labs(fill = '\u0394SST (\u00B0C)')
p_delta_s
ggsave(paste0('output/', 'goa_delta_sst_', run_warm, '_vs_', run_base, '.png'), p_delta_s, width = 8, height = 3)

# bottom
p_delta_b <- ggplot()+
  geom_sf(data = goa_temp_sf %>% filter(lyr == 'sed'), aes(fill = delta_temp))+
  scale_fill_viridis()+
  geom_sf(data = coast_sf)+
  theme_bw()+
  labs(fill = '\u0394SBT (\u00B0C)')
p_delta_b
ggsave(paste0('output/', 'goa_delta_sbt_', run_warm, '_vs_', run_base, '.png'), p_delta_b, width = 6, height = 2.5, dpi = 600)


# Plot seasonal average of seawater temperatures in space ---------------------------

# plot summer temps
# surface
p_summer_s <- ggplot()+
  geom_sf(data = goa_temp_sf %>% filter(lyr == 'lyr1'), aes(fill = warm))+
  scale_fill_viridis()+
  geom_sf(data = coast_sf)+
  theme_bw()+
  labs(fill = 'Heatwave SST (\u00B0C)')
p_summer_s
ggsave(paste0('output/', 'goa_summer_sst_', run_warm, '_vs_', run_base, '.png'), p_summer_s, width = 8, height = 3)

# bottom
p_summer_b <- ggplot()+
  geom_sf(data = goa_temp_sf %>% filter(lyr == 'sed'), aes(fill = warm))+
  scale_fill_viridis()+
  geom_sf(data = coast_sf)+
  theme_bw()+
  labs(fill = 'Heatwave SBT (\u00B0C)')
p_summer_b
ggsave(paste0('output/', 'goa_summer_sbt_', run_warm, '_vs_', run_base, '.png'), p_summer_b, width = 8, height = 3)


# Productivity forcings ---------------------------------------------------

# compare base to prod run
# comparing biomass here even though the forcings are applied to growth
# This is useful because it gives us a sense of the actual scaling of the biomass (is there more? less? for which groups?)
biom_base <- read.table(paste0(dir_base, 'outputGOA0', run_base, '_testBiomIndx.txt'), header = T)
biom_prod <- read.table(paste0(dir_prod, 'outputGOA0', run_prod, '_testBiomIndx.txt'), header = T)

# keep columns of interest, reshape data
biom_base_long <- biom_base %>%
  select(Time:DR) %>%
  mutate(Time = Time / 365) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'mt_base') %>%
  left_join((grps %>% select(Code, Name)), by = 'Code') 

biom_prod_long <- biom_prod %>%
  select(Time:DR) %>%
  mutate(Time = Time / 365) %>%
  pivot_longer(-Time, names_to = 'Code', values_to = 'mt_prod') %>%
  left_join((grps %>% select(Code, Name)), by = 'Code') 

# join and get biomass relative to base run
biom_all <- biom_base_long %>%
  left_join(biom_prod_long, by = c('Time','Code','Name')) %>%
  mutate(Control = mt_base / mt_base,
         `Heat wave` = mt_prod / mt_base) %>%
  select(Time, Code, Name, Control, `Heat wave`) %>%
  pivot_longer(-c(Time, Code, Name), names_to = 'Run', values_to = 'rel_biomass')

# plot biomass over time, keep only plankton groups that we are changing
p_prod <- biom_all %>%
  filter(Code %in% c('ZM','PL')) %>%
  ggplot(aes(x = Time, y = rel_biomass, group = Run))+
  geom_line(aes(color = Run, linetype = Run), linewidth = 0.8)+
  scale_color_viridis_d(begin = 0.2, end = 0.8)+
  annotate("rect", xmin = 30, xmax = 35, ymin = -Inf, ymax = Inf,
           alpha = .2, fill = 'yellow')+
  theme_bw()+
  labs(x = 'Year', y = 'Biomass relative to control')+
  facet_wrap(~Name, ncol = 1)
p_prod

ggsave(paste0('output/', 'relative_plankton_biomass_', run_base, '_vs_', run_prod, '.png'), 
       p_prod, width = 5, height = 1.5, dpi = 600)

# CEATTLE -----------------------------------------------------------------

# Bioenergetics from CEATTLE
# Pollock, Cod, and ATF from: Holsman, K. K., & Aydin, K. (2015). Comparative methods for evaluating climate change impacts on the foraging ecology of Alaskan groundfish. Marine Ecology Progress Series, 521, 217–235. https://doi.org/10.3354/meps11102
# Halibut from: Holsman, KK, Aydin, K, Sullivan, J, Hurst, T, Kruse, GH. Climate effects and bottom-up controls on growth and size-at-age of Pacific halibut (Hippoglossus stenolepis) in Alaska (USA). Fish Oceanogr. 2019; 28: 345– 358. https://doi.org/10.1111/fog.12416

dat <- data.frame('Par' = c('Cq', 'Tc0', 'Tcm'),
                  'Pollock' = c(2.6, 10, 15),
                  'Cod' = c(2.41, 13.7, 21),
                  'ATF' = c(2.497, 20.512, 26),
                  'Halibut' = c(3.084, 12.97, 18))

# TC0 is the temperature where laboratory consumption rates are highest, TCM is the maximum water
# temperature above which consumption ceases and CQ approximates the Q10 or the rate at which 
# the function increases over relatively low water temperatures.

make_curve <- function(species, dat, Tamb){
  
  Cq <- dat %>% filter(Par == 'Cq') %>% pull(species)
  Tc0 <- dat %>% filter(Par == 'Tc0') %>% pull(species)
  Tcm <- dat %>% filter(Par == 'Tcm') %>% pull(species)
  
  Y <- log(Cq) * (Tcm - Tc0 + 2)
  Z <- log(Cq) * (Tcm - Tc0)
  X <- (Z^2 * (1 + (1 + 40/Y)^0.5)^2)/400
  V <- (Tcm - Tamb) / (Tcm - Tc0)
  
  Tcorr <- V^X * exp((X * (1 - V)))
  
  return(Tcorr)
  
}

tcorr_frame <- data.frame('Tamb' = seq(0, 30, 0.1)) %>%
  mutate(Pollock = make_curve('Pollock', dat, Tamb),
         Cod = make_curve('Cod', dat, Tamb),
         ATF = make_curve('ATF', dat, Tamb),
         Halibut = make_curve('Halibut', dat, Tamb)) 

tcorr_frame_long <- tcorr_frame %>%
  pivot_longer(-Tamb, names_to = 'Species', values_to = 'Tcorr')

tcorr_frame_long$Species <- gsub('ATF', 'Arrowtooth_flounder', tcorr_frame_long$Species)
  
p_q10 <- tcorr_frame_long %>%
  ggplot(aes(x = Tamb, y = Tcorr, color = Species))+
  geom_line(linewidth = 1.5)+
  scale_color_viridis_d(begin = 0.1, end = 0.9)+
  theme_bw()+
  labs(x = 'Temperature (\u00B0C)', 'Tcorr')

p_q10
ggsave('output/CEATTLE_bioenergetics.png', p_q10, width = 4.5, height = 2, dpi = 300)


# AQUAMPAS ----------------------------------------------------------------

dat <- read.csv('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/thermal_responses/thermal_niches_aquamaps_0_100_percentiles.csv')

# added for AMSS 1/17/2023
# Change COD, POL, ATF, HAL so that max is same as the maximum in the bioenergetics
dat$max[dat$Code=='POL'] <- 15
dat$max[dat$Code=='COD'] <- 21
dat$max[dat$Code=='ATF'] <- 26
dat$max[dat$Code=='HAL'] <- 18

temp <- seq(-5,35,0.1)
k <- 2 # this is same for all for now

make_niche <- function(min_sp, max_sp, current_enviro, K_const_sp = k){
  
  step1 <- K_const_sp * exp(current_enviro - min_sp) / (K_const_sp + (exp(current_enviro - min_sp) - 1.0))
  step2 <- K_const_sp * exp(max_sp - current_enviro) / (K_const_sp + (exp(max_sp - current_enviro) - 1.0))
  
  # case sensitive_biologistic_window: // Gaussian shape
  numScalar <- 1.0
  step3 <- step1 / K_const_sp
  step4 <- step2 / K_const_sp
  if (step3 > step4) {
    numScalar <- step4
  } else {
    numScalar <- step3
  }
  
  return(numScalar)
}

dat1 <- dat %>%
  rename(mint = min, maxt = max)

# attach temperature as column
dat2 <- do.call("rbind", replicate(length(temp), dat1, simplify = FALSE)) %>%
  arrange(Index) %>%
  mutate(temp = rep(temp, nrow(dat)))

# apply function
to_show <- c('ATF','Cod','Pollock','Halibut')

dat3 <- dat2 %>%
  mutate(niche = purrr::pmap(list(min_sp = mint, max_sp = maxt, current_enviro = temp), make_niche)) %>%
  unnest(cols = c('niche')) %>%
  select(Code, Name, mint, maxt, temp, niche)

# ATF too long for poster
dat3$Name <- gsub('Arrowtooth_flounder', 'ATF', dat3$Name)

dat_vline <- dat3 %>%
  select(Code, Name, mint, maxt) %>%
  filter(Name %in% to_show)%>%
  pivot_longer(-c(Code, Name), names_to = 'edge', values_to = 'temp')

# rewrite edges
dat_vline$edge <- gsub('mint', 'Minimum', dat_vline$edge)
dat_vline$edge <- gsub('maxt', 'Maximum', dat_vline$edge)

# plot
p_niche <- dat3 %>%
  filter(Name %in% to_show)%>%
  ggplot()+
  geom_line(aes(x = temp, y = niche), linewidth = 1.5)+
  geom_vline(data = dat_vline, aes(xintercept = temp, color = edge), linewidth = 1.5)+
  scale_color_viridis_d(begin = 0.2, end = 0.8)+
  theme_bw()+
  labs(x = 'Temperature (\u00B0C)', y = 'Scalar on abundance', color = '')+
  facet_wrap(~Name, ncol = 4)
p_niche
ggsave('output/thermal_niche_groundfish.png', p_niche, width = 6, height = 2.5, dpi = 600)
  