# Alberto Rovellini
# 3/21/2023
# Code to create Fig. 3 for ECCWO poster
# Fig. 3 weight at age and biomass of forage fish and groups that feed on it
# all the extraction code is take from Owen and PY's code

library(tidyverse)
library(rbgm)
library(ggsidekick)
library(tidync)
library(viridis)
library(ggh4x)
library(sf)

run_base <- 1031
run_hw <- 1063

# set paths to directories
dir_base <- paste0('../../GOA/Parametrization/output_files/data/out_', run_base, '/')
dir_hw <- paste0('../../GOA/Parametrization/output_files/data/out_', run_hw, '/')

# file name here of downloaded .bgm file from current Atlantis model
# read bgm
fl <- '../data/GOA_WGS84_V4_final.bgm'
# load the file
bgm <- read_bgm(fl)
# names(bgm)
goa_sf <- box_sf(bgm)
goa_sf <- goa_sf %>% mutate(box_id = box_id+1) #ALBI: adding this for correct matching of boxes
st_crs(goa_sf) <- st_crs(attr(goa_sf$geometry, "crs")$proj)

# read fg
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

# output files
# base run
out_fl_base <- paste0(dir_base, 'outputGOA0', run_base, '_test.nc')
out_base <- tidync(out_fl_base)
this_nc_base <- ncdf4::nc_open(out_fl_base)
# hw run
out_fl_hw <- paste0(dir_hw, 'outputGOA0', run_hw, '_test.nc')
out_hw <- tidync(out_fl_hw)
this_nc_hw <- ncdf4::nc_open(out_fl_hw)

# derived values for output
# depths <- out %>% hyper_filter(t=t==0) %>% hyper_tibble(select_var="dz") %>% dplyr::select(-t)
# glimpse(depths)

# volumes of each layer
# extract from base run
volumes <- out_base %>% hyper_filter(t=t==0) %>% hyper_tibble(select_var="volume") %>% dplyr::select(-t)

# time dimension
# extract from base run
ts <- ncdf4::ncvar_get(this_nc_base,varid = "t") %>% as.numeric
tyrs <- ts/(60*60*24*365)

# area of each box is the same as volume of the deepest depth layer, because the dz of that layer is 1
areas <- volumes %>% filter(z==max(z)) %>% dplyr::select(b,volume) %>% rename(area=volume)

# functional group dimensions
# extract from base run
fg_dimensions <- hyper_grids(out_base) %>% 
  pluck("grid") %>% 
  purrr::map_df(function(x){
    out_base %>% activate(x) %>% hyper_vars() %>% 
      mutate(grd=x)
  })

# Important function to remove the boundary box values from these calculations

boundary_boxes <- goa_sf %>% filter(boundary == TRUE) %>% pull(box_id)

setNA <- function(mat) {
  mat2 <- mat
  if(length(dim(mat2))==3) mat2[,boundary_boxes,]<-NA
  if(length(dim(mat2))==2) mat2[boundary_boxes,] <- NA
  mat2
}

plot_wage_timeseries <- function(fg,out,this.nc,hw){
  # get the attributes associated with each functional group
  fg_atts <- grps %>% filter(Name==fg)
  
  if(fg_atts$BiomassType!="vertebrate") stop("weight at age only for vertebrates.")
  
  #Extract from the output .nc file the appropriate reserve N time series variables
  resN_vars <- hyper_vars(out) %>% # all variables in the .nc file active grid
    filter(grepl("_ResN",name)) %>% # filter for abundance variables
    filter(grepl(fg,name)) # filter for specific functional group
  
  #Extract from the output .nc file the appropriate structural N time series variables
  strucN_vars <- hyper_vars(out) %>% # all variables in the .nc file active grid
    filter(grepl("_StructN",name)) %>% # filter for abundance variables
    filter(grepl(fg,name)) # filter for specific functional group
  
  # Get numbers by box
  abun_vars <- hyper_vars(out) %>% # all variables in the .nc file active grid
    filter(grepl("_Nums",name)) %>% # filter for abundance variables
    filter(grepl(fg,name)) # filter for specific functional group
  
  if(nrow(resN_vars)==0) {return("no data.")}
  else {
    # # Actually pull the data from the .nc
    resN <- purrr::map(resN_vars$name,ncdf4::ncvar_get,nc=this.nc) 
    strucN <- purrr::map(strucN_vars$name,ncdf4::ncvar_get,nc=this.nc)
    nums <-purrr::map(abun_vars$name,ncdf4::ncvar_get,nc=this.nc) #numbers by age group,box,layer,time
    totnums <-nums %>% purrr::map(apply,MARGIN=3,FUN=sum) # total numbers by age group, time
    relnums <- purrr::map2(nums,totnums,sweep,MARGIN=3,FUN=`/`) # divide nums by totnums along the time axis to get relative annual nums per age group/box/layer
    
    # add the two matrices to get total nitrogen weight
    rnsn <- purrr::map2(resN,strucN,`+`)
    
    # multiply and sum to get abundance-weighted mean weight at age
    rnsn_summ <- purrr::map2(rnsn,relnums,`*`) %>% 
      purrr::map(apply,MARGIN=3,FUN=sum) %>% # mean total N by time
      bind_cols() %>% # bind age groups elements together
      suppressMessages() %>% 
      set_names(resN_vars$name) %>% 
      mutate(t=tyrs) %>%
      # pivot to long form
      pivot_longer(cols = contains(fg_atts$Name),names_to = 'age_group',values_to = 'totN') %>%
      mutate(age=parse_number(age_group)) %>% 
      mutate(weight=totN*20*5.7/1000000) %>%   # convert totN to weight/individual in kg
      dplyr::filter(t>0) %>%
      mutate(year = ceiling(t)) %>%
      group_by(year, age_group, age) %>%
      summarise(weight = mean(weight)) %>%
      ungroup() %>%
      mutate(run = ifelse(isTRUE(hw), 'Heat wave', 'Control'),
             Name = fg_atts$Name)
    
    return(rnsn_summ)
    
    # plot
    # plot_out <- rnsn_summ %>%
    #   ggplot(aes(year,weight,col=factor(age)))+
    #   geom_line(linewidth = .8)+
    #   scale_color_viridis_d(begin = .2, end = .8)+
    #   annotate("rect", xmin = 30, xmax = 35, ymin = -Inf, ymax = Inf,
    #            alpha = .2, fill = 'red')+
    #   theme_sleek()+
    #   labs(col="Age Group",y="Wet Weight per Individual (kg)",x="Year",title=paste0(fg_atts$LongName,"\nWeight-at-Age"))
    # 
    # return(plot_out)
  }
}

fg_to_plot <- c('Capelin', 'Sandlance', 'Herring', 
                'Arrowtooth_flounder', 'Pollock', 'Cod', 'Shallow_demersal',
                'Seabird_dive_fish', 'Seabird_surface_fish')

waa_base <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_base, this.nc = this_nc_base, hw = FALSE))
waa_hw <- bind_rows(purrr::map(fg_to_plot, plot_wage_timeseries, out = out_hw, this.nc = this_nc_hw, hw = TRUE))

# bind them and then plot them
waa_all <- rbind(waa_base, waa_hw)
waa_all$Name <- factor(waa_all$Name, levels = fg_to_plot) # reorder groups
waa_all$age <- factor(waa_all$age, levels = as.character(1:10)) # treat age groups as a factor and order them 

# filter out the burn-in, or the worst of it at least
waa_all <- waa_all %>% filter(year >= 20)

p <- ggplot()+
  geom_line(data = waa_all, aes(year,weight,col=age), linewidth = .8)+
  scale_color_viridis_d(begin = 0.1, end = 0.9)+
  geom_rect(data = data.frame(xmin = c(NA, 30),
                              xmax = c(NA, 35),
                              ymin = c(NA, -Inf),
                              ymax = c(NA, Inf),
                              run = c('Control','Heat wave')),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = NA, fill = 'red', alpha = 0.2)+
  theme_sleek()+
  labs(col="Age Group",y="Wet Weight per Individual (kg)",x="Year")+
  facet_grid2(run ~ Name, scales = 'free_y', independent = 'y')
p
ggsave('waa.png',p,width = 16,height=5)
