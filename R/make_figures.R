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

# make new folder to save plots in
now <- gsub(' ', '_', gsub(':', '.', Sys.time()))

run_base <- 1234 # this is the control run we compare results to
run_warm <- 1235 # just warm (temp) forcings
run_prod <- 1236 # just plankton prod scalar
run_warm_prod <- 1237 # temp and plankton scalar - "true" heatwave scenario

# set paths to directories
dir_base <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_base, '/')
dir_warm <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_warm, '/')
dir_prod <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_prod, '/')
dir_warm_prod <- paste0('../../../GOA/Parametrization/output_files/data/out_', run_warm_prod, '/')

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

# producers and detritus
prods_gtype <- c('PHYTOBEN', 'LG_PHY', 'SM_PHY', 'SED_BACT', 'PL_BACT', 'CARRION', 'LAB_DET', 'REF_DET')
# predators:
preds_gtype <- setdiff((grps %>% pull(GroupType) %>% unique()), prods_gtype)

# and corresponding codes
pred_codes <- grps %>% filter(GroupType %in% preds_gtype) %>% pull(Code)

# age class at maturity to split age classes between juveniles and adults
agemat <- read.csv('../data/agemat.csv', header = T)

# Model output: read NetCDF files
# base run
out_fl_base <- paste0(dir_base, 'outputGOA0', run_base, '_test.nc')
out_base <- tidync(out_fl_base)
this_nc_base <- ncdf4::nc_open(out_fl_base)
# warm run
out_fl_warm <- paste0(dir_warm, 'outputGOA0', run_warm, '_test.nc')
out_warm <- tidync(out_fl_warm)
this_nc_warm <- ncdf4::nc_open(out_fl_warm)
# prod run
out_fl_prod <- paste0(dir_prod, 'outputGOA0', run_prod, '_test.nc')
out_prod <- tidync(out_fl_prod)
this_nc_prod <- ncdf4::nc_open(out_fl_prod)
# warm and prod run
out_fl_warm_prod <- paste0(dir_warm_prod, 'outputGOA0', run_warm_prod, '_test.nc')
out_warm_prod <- tidync(out_fl_warm_prod)
this_nc_warm_prod <- ncdf4::nc_open(out_fl_warm_prod)

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

spinup_length <- 30 # how many years is the spinup?
seas <- 0.8 # one of 0.2, 0.4, 0.6, 0.8, 1 if we use 73 days for the output frequency on run.prm

# # area of each box is the same as volume of the deepest depth layer, because the dz of that layer is 1
areas <- volumes %>% filter(z==max(z)) %>% dplyr::select(b,volume) %>% rename(area=volume)

# coast shapefile
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

# optional subset of groups to plot
#plot_these <- grps$Name
plot_these <- c("Seabird_dive_fish", "Seabird_surface_fish", #"Seabird_dive_invert", "Seabird_surface_inverts", 
                "Pollock",  "Cod",
                "Arrowtooth_flounder", "Halibut",# "Flathead_sole", "Rex_sole", "Flatfish_shallow", "Flatfish_deep" , 
                "Sablefish",  
                "Pacific_ocean_perch", "Rockfish_slope",# "Rockfish_pelagic_shelf", "Rockfish_demersal_shelf", 
                #"Octopus", "Squid", 
                "Herring", "Capelin", "Sandlance", "Eulachon",# "Forage_slope", 
                #"Crab_tanner", "Crab_king", "Crab_other",             
                "Euphausiids", "Macrozooplankton",  "Mesozooplankton", "Microzooplankton", "Jellyfish", "Gelatinous_other", 
                "Diatoms", "Picophytoplankton")#, 
                #"Detritus_labile", "Detritus_refractory")

# produce plots
file_plot_list <- list('R/ices_functions.R',
                       'R/fig_intro_methods.R',
                       'R/fig_biomass_changes.R',
                       'R/fig_catch_changes.R', # to run this you need to have all results from the same set of runs
                       'R/fig_waa_hm_static.R',
                       'R/fig_naa_hm_static.R',
                       #'R/fig_waa_hm_temporal.R', # these made more sense for the HW stuff
                       #'R/fig_naa_hm_temporal.R', # these made more sense for the HW stuff
                       'R/fig_spatial_naa.R', 
                       'R/fig_diet_changes.R'#,
                       #'fig_s_appendix.R', # these are for the appendix
                       #'fig_dietcomp_appendix.R' # these are for the appendix
                       )

sapply(file_plot_list, source)
