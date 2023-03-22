# Alberto Rovellini
# 3/21/2023
# Code to create Fig. 1 for ECCWO poster
# Fig. 1 Atlantis GOA map, Temperature values in space, and trajectory for heat wave from ROMS hindcast, highlight the heatwave

library(dplyr)
library(ggplot2)
library(tidyr)
library(rbgm)
library(ggsidekick)
library(maps)
library(mapdata)
library(tidync)
library(sf)
library(viridis)

# map of GOA model
# file name here of downloaded .bgm file from current Atlantis model
# read bgm
fl <- '../data/GOA_WGS84_V4_final.bgm'
# load the file
bgm <- read_bgm(fl)
# names(bgm)
goa_sf <- box_sf(bgm)
#goa_sf <- goa_sf %>% mutate(box_id = box_id+1) #ALBI: adding this for correct matching of boxes
st_crs(goa_sf) <- st_crs(attr(goa_sf$geometry, "crs")$proj)

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

# plot
ggplot()+
  geom_sf(data = goa_sf, aes(fill = -botz))+
  scale_fill_viridis()+
  geom_sf(data = coast_sf)+
  theme_bw()

# time series of temperature

# Change in surface temperature by box? Can take annual before and during heatwave
# use netcdf output from runs with real forcings, average per cell from 2000-2012 and 2013-2016
# show surface and bottom
run_hw <- 1062

# set paths to directories
dir_hw <- paste0('../../GOA/Parametrization/output_files/data/out_', run_hw, '/')

# netcdf output file from hw run
out_fl_hw <- paste0(dir_hw, 'outputGOA0', run_hw, '_test.nc')
out_hw <- tidync(out_fl_hw)
this_nc_hw <- ncdf4::nc_open(out_fl_hw)

# get temp
temp_array_raw <- temp_array <- ncdf4::ncvar_get("Temp", nc=this_nc_hw)

# turn the array to a 2D data frame, add box and time dimensions, etc.
dim(temp_array) <- c(7, 109 * 201)

temp_df <- temp_array %>%
  t() %>%
  data.frame() %>%
  mutate(box_id = rep(1:109, 201),
         ts = rep(1:201, each = 109)) %>%
  mutate(box_id = box_id - 1) %>%
  filter(ts > 1) %>% # here 1 is the initial conditions, which immdiately get replaced
  mutate(ts = ts - 1,
         ts = ts / 5) # change to years

colnames(temp_df) <-   c(paste0('lyr',7:1), 'box_id', 'ts')

sst_df <- temp_df %>%
  dplyr::select(ts,box_id,lyr1) %>%
  rowwise() %>%
  mutate(hw = ifelse(ts < 30 || ts >= 35, 'pre_hw', 'hw')) %>% # change these when we have the real forcings
  filter(ts %in% seq(0.8,40,1)) %>% # keep only september values, highest SST, this depends on the time step output of the model
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

# plot
p <- ggplot()+
  geom_sf(data = goa_sst_sf, aes(fill = delta_sst))+
  scale_fill_viridis()+
  geom_sf(data = coast_sf)+
  theme_bw()+
  labs(fill = 'Delta SST \n(pre-heatwave \nand during heatwave)')
p
ggsave('goa_delta_sst.png', p, width = 10, height = 3.5)
  
# sst_df %>%
#   group_by(ts) %>%
#   summarize(lyr1 = mean(lyr1)) %>%
#   filter(ts < 2) %>%
#   ungroup() %>%
#   ggplot(aes(x = ts, y = lyr1))+
#   geom_line()
