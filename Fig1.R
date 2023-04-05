# Alberto Rovellini
# 3/21/2023
# Code to create Fig. 1 for ECCWO poster
# Fig. 1 Atlantis GOA map, Temperature values in space, and trajectory for heat wave from ROMS hindcast, highlight the heatwave

coast <- maps::map(database = 'worldHires', regions = c('USA','Canada'), plot = FALSE, fill=TRUE)

coast_sf <- coast %>% 
  st_as_sf(crs = 4326) %>% 
  st_transform(crs = st_crs(goa_sf)) %>% 
  st_combine() %>%
  st_crop(goa_sf %>% st_bbox())

# make boundary boxes grey
goa_sf <- goa_sf %>% 
  rowwise() %>%
  mutate(botz = ifelse(boundary == TRUE, NA, botz)) %>%
  ungroup()

# # plot
# ggplot()+
#   geom_sf(data = goa_sf, aes(fill = -botz))+
#   scale_fill_viridis()+
#   geom_sf(data = coast_sf)+
#   theme_bw()

# time series of temperature
# compare a run with the HW temp forcing to the base run
# use netcdf output from runs with real forcings, average per cell from 2000-2012 and 2013-2016
# show surface and bottom

# netcdf output file from hw run
out_fl_hw <- paste0(dir_hw, 'outputGOA0', run_hw, '_test.nc')
out_hw <- tidync(out_fl_hw)
this_nc_hw <- ncdf4::nc_open(out_fl_hw)

# get temp
temp_array_raw <- temp_array <- ncdf4::ncvar_get("Temp", nc=this_nc_hw)

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
  dplyr::select(ts,box_id,lyr1) %>%
  rowwise() %>%
  mutate(lyr1 = ifelse(box_id %in% boundary_boxes, NA, lyr1)) %>% # turn boundary boxes to NA temp, we do not want those to confuse the average plot
  ungroup() %>%
  left_join(areas, by = c('box_id'='b')) %>% # add areas for weighting
  group_by(ts) %>%
  summarise(sst = weighted.mean(lyr1, area, na.rm = T)) %>%
  ungroup() %>%
  ggplot()+
  geom_line(aes(x = ts, y = sst), linewidth = 0.8)+
  theme_bw()+
  labs(x = 'Year', y = 'SST (\u00B0C)')
p_ts
ggsave(paste0('output/', 'goa_sst_ts_', run_hw, '.png'), p_ts, width = 5, height = 2)

# # plot by box to compare with input files viewed in Shane's code
temp_df %>%
  dplyr::select(ts,box_id,lyr1) %>%
  filter(ts <= 1) %>%
  ggplot()+
  geom_line(aes(x = ts, y = lyr1, color = factor(box_id)))#+
  #guides(color = "none")

sst_df <- temp_df %>%
  dplyr::select(ts,box_id,lyr1) %>%
  rowwise() %>%
  mutate(hw = ifelse(ts < 30 || ts >= 35, 'pre_hw', 'hw')) %>% # change these when we have the real forcings
  filter(ts %in% seq(0.6,40,1)) %>% # this is July, for September use 0.8. This will matter for spatial patterns
  group_by(hw, box_id) %>%
  summarize(mean_sst = mean(lyr1)) %>% # means by box per period (pre and during heatwave)
  ungroup()

# now join to spatial data set
goa_sst_sf <- goa_sf %>%
  left_join(sst_df, by = 'box_id') %>%
  rowwise() %>%
  mutate(mean_sst = ifelse(boundary == "TRUE" || botz >= 0, NA, mean_sst)) %>%
  ungroup() %>%
  dplyr::select(box_id, hw, mean_sst) %>%
  pivot_wider(names_from = 'hw', values_from = 'mean_sst') %>%
  mutate(delta_sst = hw - pre_hw)

# plot delta
p_delta <- ggplot()+
  geom_sf(data = goa_sst_sf, aes(fill = delta_sst))+
  scale_fill_viridis()+
  geom_sf(data = coast_sf)+
  theme_bw()+
  labs(fill = '\u0394SST (\u00B0C)')
p_delta
ggsave(paste0('output/', 'goa_delta_sst_', run_hw, '_vs_', run_base, '.png'), p_delta, width = 8, height = 3)

# plot summer temps
p_summer <- ggplot()+
  geom_sf(data = goa_sst_sf, aes(fill = hw))+
  scale_fill_viridis()+
  geom_sf(data = coast_sf)+
  theme_bw()+
  labs(fill = 'Heatwave SST (\u00B0C)')
p_summer
ggsave(paste0('output/', 'goa_summer_sst_', run_hw, '_vs_', run_base, '.png'), p_summer, width = 8, height = 3)
  