# make all plots
library(tidyverse)
library(rbgm)
library(ggsidekick)
library(maps)
library(mapdata)
library(tidync)
library(sf)
library(viridis)
library(ggh4x)
library(RColorBrewer)

rm(list = ls())
gc()

select <- dplyr::select

run_base <- 1113 # this is the control run (no HW) we compare results to
run_hw <- 1114 # just HW (temp) forcings
run_prod <- 1117 # just plankton prod scalar
run_hw_prod <- 1115 # temp and plankton scalar - "true" heatwave scenario

# set paths to directories
dir_base <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_base, '/')
dir_hw <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_hw, '/')
dir_prod <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_prod, '/')
dir_hw_prod <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_hw_prod, '/')

# Read files --------------------------------------------------------------
# Geography: read bgm
fl <- '../data/GOA_WGS84_V4_final.bgm'
bgm <- read_bgm(fl)
goa_sf <- box_sf(bgm)
st_crs(goa_sf) <- st_crs(attr(goa_sf$geometry, "crs")$proj)
boundary_boxes <- goa_sf %>% filter(boundary == TRUE) %>% pull(box_id) # get boundary boxes

# Biology: read Groups.csv
grps <- read.csv('../data/GOA_Groups.csv', header = T)

# set up a functional group types table
vertebrate_groups <- grps %>% filter(GroupType%in%c("FISH","SHARK","BIRD","MAMMAL")) %>% mutate(BiomassType="vertebrate")
plankton_groups <- grps %>% filter(GroupType %in% c("PWN",'CEP','LG_ZOO','MED_ZOO','SM_ZOO','LG_PHY','SM_PHY')) %>%
  mutate(BiomassType="plankton")
bottom_groups <- grps %>% filter(GroupType %in% c("MOB_EP_OTHER",'SED_EP_FF','SED_EP_OTHER','PHYTOBEN')) %>%
  mutate(BiomassType="2D")
other_groups <- grps %>% filter(GroupType %in% c("LG_INF","MICROPHTYBENTHOS","SED_BACT","PL_BACT","SM_INF","CARRION","LAB_DET","REF_DET"))%>%
  mutate(BiomassType="other")
biomass_groups <- bind_rows(vertebrate_groups,plankton_groups,bottom_groups,other_groups)

# add to grps df
grps <- grps %>% left_join(biomass_groups)

# read in guilds
guild_frame <- read.csv('../data/fg_to_guild.csv')
guild_frame <- guild_frame %>% mutate(fg = str_remove(fg, '_N'))

# Model output: read NetCDF files
# base run
out_fl_base <- paste0(dir_base, 'outputGOA0', run_base, '_test.nc')
out_base <- tidync(out_fl_base)
this_nc_base <- ncdf4::nc_open(out_fl_base)
# hw run
out_fl_hw <- paste0(dir_hw, 'outputGOA0', run_hw, '_test.nc')
out_hw <- tidync(out_fl_hw)
this_nc_hw <- ncdf4::nc_open(out_fl_hw)
# prod run
out_fl_prod <- paste0(dir_prod, 'outputGOA0', run_prod, '_test.nc')
out_prod <- tidync(out_fl_prod)
this_nc_prod <- ncdf4::nc_open(out_fl_prod)
# hw and prod run
out_fl_hw_prod <- paste0(dir_hw_prod, 'outputGOA0', run_hw_prod, '_test.nc')
out_hw_prod <- tidync(out_fl_hw_prod)
this_nc_hw_prod <- ncdf4::nc_open(out_fl_hw_prod)

# derived values for output
depths <- out_base %>% hyper_filter(t=t==0) %>% hyper_tibble(select_var="dz") %>% dplyr::select(-t)
# glimpse(depths)

# # volumes of each layer
# # extract from base run
volumes <- out_base %>% hyper_filter(t=t==0) %>% hyper_tibble(select_var="volume") %>% dplyr::select(-t)
#
# # time dimension
# # extract from base run
ts <- ncdf4::ncvar_get(this_nc_base,varid = "t") %>% as.numeric
tyrs <- ts/(60*60*24*365)
#
# # area of each box is the same as volume of the deepest depth layer, because the dz of that layer is 1
areas <- volumes %>% filter(z==max(z)) %>% dplyr::select(b,volume) %>% rename(area=volume)

# produce plots
file_plot_list <- list('eccwo_functions.R','Fig1.R', 'Fig2.R', 'Fig3.R', 'Fig4.R', 'Fig5.R', 'Fig6.R')
sapply(file_plot_list, source)
