### Functions for Bee & Plant interactions ###



#### A function for breaking up lists into subset dfs, usefull for ecoregion analyses ################################
split_list <- function(list, df_names){
  for(i in 1:length(list)){
    
    assign(paste0(df_names[i]), list[[i]], envir = .GlobalEnv)
    
  }
}

# Requirements! A structured list made. See example code below for construction of such list. 
#               A df_names vector for naming the outputs 
# Some example code for building the two arguments: 
# list_1 <- lapply(1:length(unique(regions_avgs_d$ecoRegion)),
                 # function(i)filter(regions_avgs_d, ecoRegion == unique(regions_avgs_d$ecoRegion)[i]))
# df_names <- c(regions_avgs_d$ecoRegion)

### A variant version thats needed if you want it to return as a df rather than a list ###
split_list_dfer <- function(list, df_names){
  for(i in 1:length(list)){
    
    assign(paste0(df_names[i]), as.data.frame(list[[i]]), envir = .GlobalEnv)
    
  }
}
#######################################################################################################################

### Splitting a DF ####################################################################################################
ecoRegion_df_splitter <- function(df, df_names){
  for(i in 1:length(df)){
    assign(paste0(df_names[i]), subset(df, ecoRegion == df_names[i]), envir = .GlobalEnv)
    
  }
}
# Requirements
# DF with ecoRegions 
# a vector of ecoRegion names 
#######################################################################################################################


### ReCreate

