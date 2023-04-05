# utilities and checks

# check that the scalar is kicking in at the correct time

library(dplyr)
library(ggplot2)
library(tidyr)
library(rbgm)
library(tidync)

select <- dplyr::select

run_base <- 1095
run_hw <- 1058

# set paths to directories
dir_base <- paste0('../../GOA/Parametrization/output_files/data/out_', run_base, '/')
dir_hw <- paste0('../../GOA/Parametrization/output_files/data/out_', run_hw, '/')

# output files
# base run
out_fl_base <- paste0(dir_base, 'outputGOA0', run_base, '_test.nc')
out_base <- tidync(out_fl_base)
this_nc_base <- ncdf4::nc_open(out_fl_base)
# hw run
out_fl_hw <- paste0(dir_hw, 'outputGOA0', run_hw, '_test.nc')
out_hw <- tidync(out_fl_hw)
this_nc_hw <- ncdf4::nc_open(out_fl_hw)

# get a variable from base
var_array_raw <- var_array <- ncdf4::ncvar_get("Diatoms_N", nc=this_nc_base)

dim_lyr <- dim(var_array)[1]
dim_box <- dim(var_array)[2]
dim_t <- dim(var_array)[3]

# turn the array to a 2D data frame, add box and time dimensions, etc.
dim(var_array) <- c(dim_lyr, dim_box * dim_t)

var_df <- var_array %>%
  t() %>%
  data.frame() %>%
  mutate(box_id = rep(1:dim_box, dim_t),
         ts = rep(1:dim_t, each = dim_box)) %>%
  mutate(box_id = box_id - 1) %>%
  filter(ts > 1) %>% # here 1 is the initial conditions, which immdiately get replaced
  mutate(ts = ts - 1,
         ts = ts / 5) # change to years

colnames(var_df) <-   c(paste0('lyr',7:1), 'box_id', 'ts')

# reshape
var_df_base <- var_df %>%
  pivot_longer(-c(box_id, ts), names_to = 'lyr', values_to = 'N_base') %>%
  group_by(ts) %>%
  summarize(N_base = sum(N_base)) %>%
  ungroup()

# get a variable from hw
var_array_raw <- var_array <- ncdf4::ncvar_get("Diatoms_N", nc=this_nc_hw)

dim_lyr <- dim(var_array)[1]
dim_box <- dim(var_array)[2]
dim_t <- dim(var_array)[3]

# turn the array to a 2D data frame, add box and time dimensions, etc.
dim(var_array) <- c(dim_lyr, dim_box * dim_t)

var_df <- var_array %>%
  t() %>%
  data.frame() %>%
  mutate(box_id = rep(1:dim_box, dim_t),
         ts = rep(1:dim_t, each = dim_box)) %>%
  mutate(box_id = box_id - 1) %>%
  filter(ts > 1) %>% # here 1 is the initial conditions, which immdiately get replaced
  mutate(ts = ts - 1,
         ts = ts / 5) # change to years
  
colnames(var_df) <-   c(paste0('lyr',7:1), 'box_id', 'ts')

# reshape
var_df_hw <- var_df %>%
  pivot_longer(-c(box_id, ts), names_to = 'lyr', values_to = 'N_hw') %>%
  group_by(ts) %>%
  summarize(N_hw = sum(N_hw)) %>%
  ungroup()
  
# join base to hw
var_df <- var_df_base %>%
  left_join(var_df_hw, by = 'ts') %>%
  mutate(ratio = N_hw / N_base)

ggplot(var_df, aes(ts, ratio))+
  geom_line()
  