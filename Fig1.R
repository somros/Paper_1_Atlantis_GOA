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

# map of GOA model
# file name here of downloaded .bgm file from current Atlantis model
# read bgm
fl <- '../data/GOA_WGS84_V4_final.bgm'
# load the file
bgm <- read_bgm(fl)
# names(bgm)
goa_sf <- box_sf(bgm)
goa_sf <- goa_sf %>% mutate(box_id = box_id+1) #ALBI: adding this for correct matching of boxes
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
  theme_sleek()

# time series of temperature


# surface temperature by box? from netcdf input or output?


# bioenergetics graph

