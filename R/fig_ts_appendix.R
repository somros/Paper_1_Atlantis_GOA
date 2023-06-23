# Alberto Rovellini
# 6/23/2023
# This script makes plots of biomass, weight at age, and numbers at age for all species and organize them in panels
# This is to be used for S1

# how many groups have terminal biomass within some bounds of initial biomass?


# weight at age

extract_wage_all <- function(out, this.nc){
  
  #Extract from the output .nc file the appropriate reserve N time series variables
  resN_vars <- hyper_vars(out) %>% # all variables in the .nc file active grid
    filter(grepl("_ResN",name)) # filter for reserve N

  #Extract from the output .nc file the appropriate structural N time series variables
  strucN_vars <- hyper_vars(out) %>% # all variables in the .nc file active grid
    filter(grepl("_StructN",name)) # filter for structural N

  # Get numbers by box
  abun_vars <- hyper_vars(out) %>% # all variables in the .nc file active grid
    filter(grepl("_Nums",name))  # filter for abundance variables

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
      pivot_longer(cols = -t,names_to = 'age_group',values_to = 'totN') %>%
      mutate(age=parse_number(age_group)) %>% 
      mutate(weight=totN*20*5.7/1000000) %>%   # convert totN to weight/individual in kg
      dplyr::filter(t>0) %>%
      mutate(year = t) %>%#ceiling(t)) %>%
      group_by(year, age_group, age) %>%
      summarise(weight = mean(weight)) %>%
      ungroup() %>%
      mutate(Name = gsub('[[:digit:]]+', '', (gsub('_ResN','',age_group)))) %>%
      left_join(grps %>% dplyr::select(Name, LongName), by = 'Name')
    

    return(rnsn_summ)
  }
}

wage_all <- extract_wage_all(out = out_base, this.nc = this_nc_base)

# plot
# Npages
npage <- 4

# organize names into pages
page_key <- wage_all %>% 
  dplyr::select(LongName) %>% 
  distinct() %>% 
  arrange(LongName) %>%
  mutate(page = rep(1:4, each = ceiling(nrow(.) / npage))[1:nrow(.)])

# add page numbers
wage_all <- wage_all %>%
  left_join(page_key, by = 'LongName')

for(i in 1:npage){
  
  p_waa <- wage_all %>%
    filter(page == i) %>%
    ggplot(aes(x = year, y = weight, color = factor(age)))+
    geom_line()+
    scale_color_viridis_d(begin = 0.1, end = 0.9)+
    theme_bw()+
    labs(x = 'Year', y = 'Mean body weight (kg)', color = 'Age group')+
    facet_wrap(~LongName, scales = 'free_y', ncol = 3,  nrow = 4)
  
  ggsave(paste0('output/', 'WAA_ts_', i, '.png'), p_waa, width = 8, height = 10)
  
}

rm(wage_all)
gc()

# numbers at age
# function that extracts abundance from NetCDF files

extract_nage_all <- function(out, this.nc){
  
  out = out_base
  this.nc = this_nc_base
  
  #Extract from the output .nc file the appropriate time series variables
  abun_vars <- hyper_vars(out) %>% # all variables in the .nc file active grid
    filter(grepl("_Nums",name)) # filter for abundance variables
  # Actually pull the data from the .nc
  
  abun1 <- purrr::map(abun_vars$name,ncdf4::ncvar_get,nc=this.nc) %>% 
    lapply(setNA) %>%
    purrr::map(apply,MARGIN=3,FUN=sum,na.rm=T) %>% 
    bind_cols() %>% 
    suppressMessages() %>% 
    set_names(abun_vars$name) %>% 
    mutate(t=tyrs)
  
  abun2 <- abun1 %>%
    pivot_longer(cols = -t, names_to = 'age_group', values_to = 'abun') %>%
    mutate(age=parse_number(age_group)) %>%
    mutate(year = t) %>%#ceiling(t)) %>%
    group_by(year, age_group, age) %>%
    summarise(abun = mean(abun)) %>%
    ungroup() %>%
    mutate(Name = gsub('[[:digit:]]+', '', (gsub('_Nums','',age_group)))) %>%
    left_join(grps %>% dplyr::select(Name, LongName), by = 'Name')
  
  
  return(abun2)
}

nage_all <- extract_nage_all(out = out_base, this.nc = this_nc_base)

# add page numbers
nage_all <- nage_all %>%
  left_join(page_key, by = 'LongName')

for(i in 1:npage){
  
  p_waa <- nage_all %>%
    filter(page == i) %>%
    ggplot(aes(x = year, y = abun, color = factor(age)))+
    geom_line()+
    scale_color_viridis_d(begin = 0.1, end = 0.9)+
    theme_bw()+
    labs(x = 'Year', y = 'Number of individuals', color = 'Age group')+
    facet_wrap(~LongName, scales = 'free_y', ncol = 3,  nrow = 4)
  
  ggsave(paste0('output/', 'NAA_ts_', i, '.png'), p_waa, width = 8, height = 10)
  
}



  