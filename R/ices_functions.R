plot_wage_timeseries <- function(fg, out, this.nc, run){
  # get the attributes associated with each functional group
  fg_atts <- grps %>% filter(Name==fg)
  
  if(fg_atts$BiomassType!="vertebrate") stop("weight at age only for vertebrates.")
  
  #Extract from the output .nc file the appropriate reserve N time series variables
  resN_vars <- hyper_vars(out) %>% # all variables in the .nc file active grid
    filter(grepl("_ResN",name)) %>% # filter for reserve N
    filter(grepl(fg,name)) # filter for specific functional group
  
  #Extract from the output .nc file the appropriate structural N time series variables
  strucN_vars <- hyper_vars(out) %>% # all variables in the .nc file active grid
    filter(grepl("_StructN",name)) %>% # filter for structural N
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
      mutate(Name = fg_atts$Name)
    
    colnames(rnsn_summ)[4] <- paste('weight', run, sep = '_')
    
    return(rnsn_summ)
  }
}

plot_abun <- function(fg, out, this.nc, run, spatial = FALSE){
  
  # if the species is TURNED OFF, return an empty plot
  if(grps$IsTurnedOn[grps$Name==fg]==0) return(tibble(t=0,abun=0) %>% ggplot(aes(t,abun))+geom_blank())
  # get the attributes associated with each functional group
  fg_atts <- grps %>% filter(Name==fg)
  #Extract from the output .nc file the appropriate time series variables
  abun_vars <- hyper_vars(out) %>% # all variables in the .nc file active grid
    filter(grepl("_Nums",name)) %>% # filter for abundance variables
    filter(grepl(fg,name)) # filter for specific functional group
  # Actually pull the data from the .nc
  
  # do different things depending on whether we want to keep the boxes or not
  if(!spatial){
    
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
      mutate(Name = fg_atts$Name)
    
    colnames(abun2)[4] <- paste('abun', run, sep = '_')
    
  } else {
    
    abun1 <- purrr::map(abun_vars$name,ncdf4::ncvar_get,nc=this.nc) %>% 
      lapply(setNA) %>%
      lapply(collapse_array) %>%
      Reduce("+", .) %>% # sum over age classes
      mutate(t = tyrs) %>%
      suppressMessages() 
    
    abun2 <- abun1 %>% 
      pivot_longer(-t, names_to = 'box_id', values_to = 'n') %>%
      filter(t > 0) %>% 
      mutate(Name = fg_atts$Name)
    
    colnames(abun2)[3] <- paste('abun', run, sep = '_')
      
  }
  
  return(abun2)
}

# reshape diets and compare between the periods
compare_diets <- function(dietcheck, prednames, run, age_split = 'none'){
  
  if(age_split == 'none'){
    
    dietcheck1 <- dietcheck %>%
      filter(Predator %in% preds_to_keep & Updated == 0) %>% # IDK what Updated means but it seems to pertain the last time step only
      group_by(Time, Predator) %>% # drop cohorts
      summarise(across(KWT:DR, mean)) %>%
      ungroup() %>%
      mutate(Time = Time / 365) %>%
      filter(Time >= 30 & Time < 35) %>% # focus on hw years only
      group_by(Predator) %>%
      summarise(across(KWT:DR, mean)) %>%
      ungroup() %>%
      pivot_longer(-Predator, names_to = 'Prey', values_to = 'Prop') %>%
      left_join((grps %>% select(Code, Name)), by = c('Predator'='Code')) %>%
      rename(Predator_Name = Name) %>%
      select(-Predator) %>%
      left_join((grps %>% select(Code, Name)), by = c('Prey'='Code')) %>%
      rename(Prey_Name = Name) %>%
      mutate(Cohort = NA) %>% # add empty cohort variable
      select(Prop, Predator_Name, Cohort, Prey_Name)%>%
      filter(Prop > 0.01)

      colnames(dietcheck1)[1] <- paste('Prop', run, sep = '_')
    
  } else if (age_split == 'cohort') {
    
    dietcheck1 <- dietcheck %>%
      filter(Predator %in% preds_to_keep & Updated == 0) %>% # IDK what Updated means but it seems to pertain the last time step only
      mutate(Time = Time / 365) %>%
      filter(Time >= 30 & Time < 35) %>% # focus on hw years only
      group_by(Predator, Cohort) %>%
      summarise(across(KWT:DR, mean)) %>%
      ungroup() %>%
      mutate(Cohort = Cohort + 1) %>%
      pivot_longer(-c(Predator, Cohort), names_to = 'Prey', values_to = 'Prop') %>%
      left_join((grps %>% select(Code, Name)), by = c('Predator'='Code')) %>%
      rename(Predator_Name = Name) %>%
      select(-Predator) %>%
      left_join((grps %>% select(Code, Name)), by = c('Prey'='Code')) %>%
      rename(Prey_Name = Name) %>%
      select(Prop, Predator_Name, Cohort, Prey_Name)%>%
      filter(Prop > 0.01)
    
      colnames(dietcheck1)[1] <- paste('Prop', run, sep = '_')
      
  } else {
    
    # we use this otpion for the plot for the last 5 year average by age class and all predators, can also place this in another function
    
    dietcheck1 <- dietcheck %>%
      mutate(Time = Time / 365) %>%
      filter(Time >= (max(Time)-5)) %>% # focus on hw years only
      left_join(agemat, by = c('Predator' = 'Code')) %>%
      rowwise() %>%
      mutate(Stage = ifelse(is.na(agemat), 1, ifelse(Cohort < agemat, 0, 1))) %>%
      ungroup() %>%
      group_by(Predator, Stage) %>%
      summarise(across(KWT:DR, mean)) %>%
      ungroup() %>%
      pivot_longer(-c(Predator, Stage), names_to = 'Prey', values_to = 'Prop') %>%
      left_join((grps %>% select(Code, Name)), by = c('Predator'='Code')) %>%
      rename(Predator_Name = Name) %>%
      select(-Predator) %>%
      left_join((grps %>% select(Code, Name)), by = c('Prey'='Code')) %>%
      rename(Prey_Name = Name) %>%
      select(Prop, Predator_Name, Stage, Prey_Name)%>%
      filter(Prop > 0.01)
    
    colnames(dietcheck1)[1] <- paste('Prop', run, sep = '_')
    
  }
  return(dietcheck1)
}

# function to set values in the boundary boxes to NA
setNA <- function(mat) {
  mat2 <- mat
  if(length(dim(mat2))==3) mat2[,(boundary_boxes+1),]<-NA
  if(length(dim(mat2))==2) mat2[(boundary_boxes+1),] <- NA
  mat2
}

# function sum over depth layers in each array slice
collapse_array <- function(mat){
  mat2 <- apply(mat, 3, colSums)
  mat3 <- data.frame(t(mat2))
  colnames(mat3) <- 0:108
  mat3
}
