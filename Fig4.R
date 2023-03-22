# Alberto Rovellini
# 3/21/2023
# Code to create Fig. 4 for ECCWO poster
# Fig. 3 numbers at age
# all the extraction code is take from Owen and PY's code

library(tidyverse)
library(rbgm)
library(ggsidekick)
library(tidync)
library(viridis)
library(ggh4x)
library(sf)

select <- dplyr::select

run_base <- 1044
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

plot_abun <- function(fg,out,this.nc,hw){
  
  # if the species is TURNED OFF, return an empty plot
  if(grps$IsTurnedOn[grps$Name==fg]==0) return(tibble(t=0,abun=0) %>% ggplot(aes(t,abun))+geom_blank())
  # get the attributes associated with each functional group
  fg_atts <- grps %>% filter(Name==fg)
  #Extract from the output .nc file the appropriate time series variables
  abun_vars <- hyper_vars(out) %>% # all variables in the .nc file active grid
    filter(grepl("_Nums",name)) %>% # filter for abundance variables
    filter(grepl(fg,name)) # filter for specific functional group
  # Actually pull the data from the .nc
  abun1 <- purrr::map(abun_vars$name,ncdf4::ncvar_get,nc=this.nc) %>% 
    lapply(setNA) %>%
    purrr::map(apply,MARGIN=3,FUN=sum,na.rm=T) %>% 
    bind_cols() %>% 
    suppressMessages() %>% 
    set_names(abun_vars$name) %>% 
    mutate(t=tyrs)
  
  abun2 <- abun1 %>%
    pivot_longer(cols = contains(fg_atts$Name),names_to = 'age_group',values_to = 'abun') %>%
    mutate(age=parse_number(age_group)) %>%
    mutate(year = ceiling(t)) %>%
    group_by(year, age_group, age) %>%
    summarise(abun = mean(abun)) %>%
    ungroup() %>%
    mutate(run = ifelse(isTRUE(hw),'Heat wave', 'Control'),
           Name = fg_atts$Name)
  
  return(abun2)
  
  # # plot
  # plot_out <- abun2 %>%
  #   filter(t>0) %>% # remove plotting artifact
  #   ggplot(aes(t,abun/1e6,col=factor(age)))+
  #   geom_line()+
  #   labs(col="Age Group",y="Numbers (Millions)",x="Year",title=paste0(fg_atts$LongName,"\nNumbers-at-Age"))
  # 
  # return(plot_out)
  
}

fg_to_plot <- c('Capelin', 'Sandlance', 'Herring', 
                'Arrowtooth_flounder', 'Pollock', 'Cod',
                'Seabird_dive_fish', 'Seabird_surface_fish')

naa_base <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_base, this.nc = this_nc_base, hw = FALSE))
naa_hw <- bind_rows(purrr::map(fg_to_plot, plot_abun, out = out_hw, this.nc = this_nc_hw, hw = TRUE))

# bind them and then plot them
naa_all <- rbind(naa_base, naa_hw)
naa_all$Name <- factor(naa_all$Name, levels = fg_to_plot) # reorder groups
naa_all$age <- factor(naa_all$age, levels = as.character(1:10)) # treat age groups as a factor and order them 

# filter out the burn-in, or the worst of it at least
naa_all <- naa_all %>% filter(year >= 20)

p <- ggplot()+
  geom_line(data = naa_all, aes(year,abun,col=age), linewidth = .8)+
  scale_color_viridis_d(begin = 0.1, end = 0.9)+
  geom_rect(data = data.frame(xmin = c(NA, 30),
                              xmax = c(NA, 35),
                              ymin = c(NA, -Inf),
                              ymax = c(NA, Inf),
                              run = c('Control','Heat wave')),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = NA, fill = 'red', alpha = 0.2)+
  theme_sleek()+
  labs(col="Age Group",y="Numbers (Millions)",x="Year")+
  facet_grid2(run ~ Name, scales = 'free_y', independent = 'y')
p
ggsave('naa_cod.png',p,width = 12,height=5)

# heatmap for changes in condition
naa_base_heatmap <- naa_base %>%
  select(year:abun, Name) %>%
  rename(abun_ctrl = abun)
naa_hw_heatmap <- naa_hw %>%
  select(year:abun, Name) %>%
  rename(abun_hw = abun)

naa_heatmap <- naa_base_heatmap %>%
  left_join(naa_hw_heatmap, by = c('year','age','age_group','Name')) %>%
  mutate(percent_change = ((abun_hw-abun_ctrl)/abun_ctrl)*100) %>%
  filter(year >= 25)

# remove _Nums from age_group
naa_heatmap$age_group <- gsub('_Nums', '', naa_heatmap$age_group)

# order
naa_heatmap <- naa_heatmap %>%
  arrange(year, factor(Name, levels = fg_to_plot), age)

# fix order of age_group
naa_heatmap$age_group <- factor(naa_heatmap$age_group, 
                                levels = rev(unique(naa_heatmap$age_group)))

# add facets 
naa_heatmap <- naa_heatmap %>%
  rowwise() %>%
  mutate(Guild = case_when(
    Name %in% c('Capelin', 'Sandlance', 'Herring') ~ 'Forage fish',
    Name %in% c('Arrowtooth_flounder', 'Pollock', 'Cod') ~ 'Groundfish',
    Name %in% c('Seabird_dive_fish', 'Seabird_surface_fish') ~ 'Seabirds'
  )) %>%
  ungroup()

# plot
phm <- ggplot()+
  geom_tile(data = naa_heatmap, aes(x = year, y = age_group, fill = percent_change))+
  scale_fill_viridis()+
  geom_vline(xintercept = 30, color = 'red', linetype = 'dashed')+
  geom_vline(xintercept = 35, color = 'red', linetype = 'dashed')+
  theme_bw()+
  labs(x = 'Year', y = '', fill = 'Percent change', title = 'Relative change in numbers at age - heatwave run vs control run')+
  facet_grid(rows = 'Guild', scales = 'free_y')
phm
ggsave('naa_relchange.png',phm,width = 8,height=9)
