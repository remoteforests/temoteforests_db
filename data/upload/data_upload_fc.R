read_data <- function(name){
  #' @description read the cleaned data from .csv
  #' @param name name of the database table into which the data should be uploaded
  
  if(name == "plot"){
    
    plot.list <- list.files(pattern = "*_plot.csv", recursive = F)
    
    data.df <- tibble()
    
    for(i in plot.list){
      
      data.df <- bind_rows(data.df,
                           read.table(i, sep = ",", header = T, stringsAsFactors = F) %>%
                             mutate(plot.date = as.numeric(plot.date),
                                    plot.plotid = as.character(plot.plotid),
                                    plot.census = as.numeric(plot.census),
                                    plot.country = as.character(plot.country),
                                    plot.location = as.character(plot.location),
                                    plot.stand = as.character(plot.stand),
                                    plot.standshort = as.character(plot.standshort),
                                    plot.plot = as.character(plot.plot),
                                    plot.subplot = as.numeric(plot.subplot),
                                    plot.lng = as.numeric(plot.lng),
                                    plot.lat = as.numeric(plot.lat),
                                    plot.plotsize = as.numeric(plot.plotsize),
                                    plot.dbh_min = as.numeric(plot.dbh_min),
                                    plot.plottype = as.numeric(plot.plottype),
                                    plot.foresttype = as.character(plot.foresttype),
                                    plot.altitude_m = as.numeric(plot.altitude_m),
                                    plot.slope = as.numeric(plot.slope),
                                    plot.aspect = as.numeric(plot.aspect),
                                    plot.landform = as.numeric(plot.landform),
                                    plot.hillform = as.numeric(plot.hillform),
                                    plot.ownership = as.numeric(plot.ownership)))
      
    }
    
    data.df <- rename_col(data.df)
    
    return(data.df)
    
  } else {
    
    if(name == "tree"){
      
      tree.list <- list.files(pattern = "*_tree.csv", recursive = F)
      
      data.df <- tibble()
      
      for(i in tree.list){
        
        data.df <- bind_rows(data.df,
                             read.table(i, sep = ",", header = T, stringsAsFactors = F) %>%
                               mutate(tree.date = as.numeric(tree.date),
                                      tree.plotid = as.character(tree.plotid),
                                      tree.treeid = as.character(tree.treeid),
                                      tree.treen = as.character(tree.treen),
                                      tree.onplot = as.numeric(tree.onplot),
                                      tree.treetype = as.character(tree.treetype),
                                      tree.x_m = as.numeric(tree.x_m),
                                      tree.y_m = as.numeric(tree.y_m),
                                      tree.status = as.numeric(tree.status),
                                      tree.census = as.numeric(tree.census),
                                      tree.growth = as.numeric(tree.growth),
                                      tree.layer = as.numeric(tree.layer),
                                      tree.species = as.character(tree.species),
                                      tree.dbh_mm = as.numeric(tree.dbh_mm),
                                      tree.height_m = as.numeric(tree.height_m),
                                      tree.crownht_m = as.numeric(tree.crownht_m),
                                      tree.crowndiam1_m = as.numeric(tree.crowndiam1_m),
                                      tree.crowndiam2_m = as.numeric(tree.crowndiam2_m),
                                      tree.decayht = as.numeric(tree.decayht),
                                      tree.decay_wood = as.numeric(tree.decay_wood),
                                      tree.decay = as.numeric(tree.decay)))
        
      }
      
      data.df <- rename_col(data.df)
      
      return(data.df)
      
    } else {
      
      if(name == "mortality"){
        
        mortality.list <- list.files(pattern = "*_mortality.csv", recursive = F)
        
        data.df <- tibble()
        
        for(i in mortality.list){
          
          data.df <- bind_rows(data.df,
                               read.table(i, sep = ",", header = T, stringsAsFactors = F) %>%
                                 mutate(mortality.date = as.numeric(mortality.date),
                                        mortality.treeid = as.character(mortality.treeid),
                                        mortality.mort_agent = as.numeric(mortality.mort_agent)))
          
        }
        
        data.df <- rename_col(data.df)
        
        return(data.df)
        
      } else {
        
        if(name == "microsites"){
          
          microsites.list <- list.files(pattern = "*_microsites.csv", recursive = F)
          
          data.df <- tibble()
          
          for(i in microsites.list){
            
            data.df <- bind_rows(data.df,
                                 read.table(i, sep = ",", header = T, stringsAsFactors = F) %>%
                                   mutate(microsites.date = as.numeric(microsites.date),
                                          microsites.treeid = as.character(microsites.treeid),
                                          microsites.microsite = as.numeric(microsites.microsite),
                                          microsites.count = as.numeric(microsites.count),
                                          microsites.method = as.numeric(microsites.method)))
            
          }
          
          data.df <- rename_col(data.df)
          
          return(data.df)
          
        } else {
          
          if(name == "tree_quality"){
            
            quality.list <- list.files(pattern = "*_tree_quality.csv", recursive = F)
            
            data.df <- tibble()
            
            for(i in quality.list){
              
              data.df <- bind_rows(data.df,
                                   read.table(i, sep = ",", header = T, stringsAsFactors = F) %>%
                                     mutate(tree_quality.date = as.numeric(tree_quality.date),
                                            tree_quality.treeid = as.character(tree_quality.treeid),
                                            tree_quality.quality = as.numeric(tree_quality.quality)))
              
            }
            
            data.df <- rename_col(data.df)
            
            return(data.df)
            
          } else {
            
            if(name == "deadwood"){
              
              deadwood.list <- list.files(pattern = "*_deadwood.csv", recursive = F)
              
              data.df <- tibble()
              
              for(i in deadwood.list){
                
                data.df <- bind_rows(data.df,
                                     read.table(i, sep = ",", header = T, stringsAsFactors = F) %>%
                                       mutate(deadwood.date = as.numeric(deadwood.date),
                                              deadwood.plotid = as.character(deadwood.plotid),
                                              deadwood.transect = as.numeric(deadwood.transect),
                                              deadwood.species = as.character(deadwood.species),
                                              deadwood.dbh_mm = as.numeric(deadwood.dbh_mm),
                                              deadwood.decay = as.numeric(deadwood.decay)))
                
              }
              
              data.df <- rename_col(data.df)
              
              return(data.df)
              
            } else {
              
              if(name == "regeneration"){
                
                regeneration.list <- list.files(pattern = "*_regeneration.csv", recursive = F)
                
                data.df <- tibble()
                
                for(i in regeneration.list){
                  
                  data.df <- bind_rows(data.df,
                                       read.table(i, sep = ",", header = T, stringsAsFactors = F) %>%
                                         mutate(regeneration.date = as.numeric(regeneration.date),
                                                regeneration.plotid = as.character(regeneration.plotid),
                                                regeneration.species = as.character(regeneration.species),
                                                regeneration.htclass = as.numeric(regeneration.htclass),
                                                regeneration.regeneratedon = as.numeric(regeneration.regeneratedon),
                                                regeneration.count = as.numeric(regeneration.count)))
                  
                }
                
                data.df <- rename_col(data.df)
                
                return(data.df)
                
              } else {
                
                if(name == "regeneration_subplot"){
                  
                  regeneration_subplot.list <- list.files(pattern = "*_regeneration_subplot.csv", recursive = F)
                  
                  data.df <- tibble()
                  
                  for(i in regeneration_subplot.list){
                    
                    data.df <- bind_rows(data.df,
                                         read.table(i, sep = ",", header = T, stringsAsFactors = F) %>%
                                           mutate(regeneration_subplot.date = as.numeric(regeneration_subplot.date),
                                                  regeneration_subplot.plotid = as.character(regeneration_subplot.plotid),
                                                  regeneration_subplot.subplot_n = as.numeric(regeneration_subplot.subplot_n),
                                                  regeneration_subplot.subplotsize_m2 = as.numeric(regeneration_subplot.subplotsize_m2),
                                                  regeneration_subplot.species = as.character(regeneration_subplot.species),
                                                  regeneration_subplot.htclass = as.numeric(regeneration_subplot.htclass),
                                                  regeneration_subplot.browsing = as.numeric(regeneration_subplot.browsing),
                                                  regeneration_subplot.regeneratedon = as.numeric(regeneration_subplot.regeneratedon),
                                                  regeneration_subplot.count = as.numeric(regeneration_subplot.count)))
                    
                  }
                  
                  data.df <- rename_col(data.df)
                  
                  return(data.df)
                  
                } else {
                  
                  if(name == "core"){
                    
                    core.list <- list.files(pattern = "*_core.csv", recursive = F)
                    
                    data.df <- tibble()
                    
                    for(i in core.list){
                      
                      data.df <- bind_rows(data.df,
                                           read.table(i, sep = ",", header = T, stringsAsFactors = F) %>%
                                             mutate(date = as.numeric(date),
                                                    treeid = as.character(treeid),
                                                    subcore = as.character(subcore),
                                                    coreht_m = as.numeric(coreht_m),
                                                    missing_mm = as.numeric(missing_mm),
                                                    missing_years = as.numeric(missing_years),
                                                    corestatus = as.numeric(corestatus),
                                                    crossdated = as.numeric(crossdated),
                                                    coretype = as.numeric(coretype)))
                      
                    }
                    
                    return(data.df)
                    
                  } else {
                    
                    if(name == "ring"){
                      
                      ring.list <- list.files(pattern = "*_ring.csv", recursive = F)
                      
                      data.df <- tibble()
                      
                      for(i in ring.list){
                        
                        data.df <- bind_rows(data.df,
                                             read.table(i, sep = ",", header = T, stringsAsFactors = F) %>%
                                               mutate(date = as.numeric(date),
                                                      treeid = as.character(treeid),
                                                      year = as.numeric(year),
                                                      incr_mm = as.numeric(incr_mm)))
                        
                      }
                      
                      return(data.df)
                      
                    } else {
                      
                      if(name == "vegetation"){
                        
                        vegetation.list <- list.files(pattern = "*_vegetation.csv", recursive = F)
                        
                        data.df <- tibble()
                        
                        for(i in vegetation.list){
                          
                          data.df <- bind_rows(data.df,
                                               read.table(i, sep = ",", header = T, stringsAsFactors = F) %>%
                                                 mutate(vegetation.date = as.numeric(vegetation.date),
                                                        vegetation.plotid = as.character(vegetation.plotid),
                                                        vegetation.sampling_date = as.POSIXct(vegetation.sampling_date),
                                                        vegetation.large_gap = as.numeric(vegetation.large_gap),
                                                        vegetation.gap_distance_m = as.numeric(vegetation.gap_distance_m),
                                                        vegetation.vegetationht = as.numeric(vegetation.vegetationht),
                                                        vegetation.vegetation_cover = as.numeric(vegetation.vegetation_cover),
                                                        vegetation.vaccinium_myrtillus_per = as.numeric(vegetation.vaccinium_myrtillus_per),
                                                        vegetation.rubus_per = as.numeric(vegetation.rubus_per),
                                                        vegetation.bryopsida_per = as.numeric(vegetation.bryopsida_per),
                                                        vegetation.polypodiopsida_per = as.numeric(vegetation.polypodiopsida_per),
                                                        vegetation.poaceae_per = as.numeric(vegetation.poaceae_per),
                                                        vegetation.ericaceae_per = as.numeric(vegetation.ericaceae_per),
                                                        vegetation.other_per = as.numeric(vegetation.other_per),
                                                        vegetation.biotope_quality = as.numeric(vegetation.biotope_quality),
                                                        vegetation.biotope_trend = as.numeric(vegetation.biotope_trend),
                                                        vegetation.large_herbivore_feces = as.numeric(vegetation.large_herbivore_feces)))
                          
                        }
                        
                        data.df <- rename_col(data.df)
                        
                        return(data.df)
                        
                      } else {
                        
                        if(name == "habitat"){
                          
                          habitat.list <- list.files(pattern = "*_habitat.csv", recursive = F)
                          
                          data.df <- tibble()
                          
                          for(i in habitat.list){
                            
                            data.df <- bind_rows(data.df,
                                                 read.table(i, sep = ",", header = T, stringsAsFactors = F) %>%
                                                   mutate(habitat.date = as.numeric(habitat.date),
                                                          habitat.plotid = as.character(habitat.plotid),
                                                          habitat.sampling_date = as.POSIXct(habitat.sampling_date),
                                                          habitat.animal_species = as.character(habitat.animal_species),
                                                          habitat.gender = as.numeric(habitat.gender),
                                                          habitat.habitat_sign_type = as.numeric(habitat.habitat_sign_type)))
                            
                          }
                          
                          data.df <- rename_col(data.df)
                          
                          return(data.df)
                          
                        } else {
                          
                          if(name == "soil"){
                            
                            soil.list <- list.files(pattern = "*_soil.csv", recursive = F)
                            
                            data.df <- tibble()
                            
                            for(i in soil.list){
                              
                              data.df <- bind_rows(data.df,
                                                   read.table(i, sep = ",", header = T, stringsAsFactors = F) %>%
                                                     mutate(soil.date = as.numeric(soil.date),
                                                            soil.plotid = as.character(soil.plotid),
                                                            soil.sample = as.numeric(soil.sample),
                                                            soil.soil_horizon = as.character(soil.soil_horizon),
                                                            soil.depth_cm = as.numeric(soil.depth_cm)))
                              
                            }
                            
                            data.df <- rename_col(data.df)
                            
                            return(data.df)
                            
                          } else {
                            
                            stop("Unknown name of the data file.")
                            
                          }
                        }
                      } 
                    }
                  } 
                }
              }
            }
          }
        }
      }
    }
  }
}

pull_id <- function(name){
  #' @description pull maximum id from database table
  #' @param name name of database table from which maximum id should be pulled
  
  id <- tbl(KELuser, name) %>% summarise(id = max(id, na.rm = T)) %>% pull(id)
  
  if(is.na(id)){id <- 0}
  
  return(id)
}

rename_col <- function(x){
  #' @description rename columns of dataframe created from list
  #' @param x dataframe to rename
  
  names(x) <- gsub("^(.*)[:.:](.*)$", "\\2", names(x))
  
  return(x)
}

colorder <- function(name){
  #'@description pull column names from database table 
  #'@param name name of database table from which column names should be pulled
  
  tbl(KELuser, name) %>% colnames()
}

prepare_data <- function(name){
  #' @description prepare the data for uploading into the database
  #' @param name name of the database table into which the data should be uploaded
  
  if(name == "plot"){
    
    id.max <- pull_id(name)
    
    data.df <- as.data.frame(data.list[name]) %>% rename_col(.) %>% 
      mutate(id = row_number() + id.max) %>% select(colorder(name))
    
    return(data.df)
    
  } else {
    
    if(name == "tree" | name == "deadwood" | name == "regeneration" | name == "regeneration_subplot" |
       name == "canopy_analysis" | name == "soil_profile" | name == "vegetation" | name == "habitat_signs") {
      
      id.max <- pull_id(name)
      
      plot_id <- tbl(KELuser, "plot") %>% select(date, plotid, plot_id = id) %>% collect()
      
      data.df <- as.data.frame(data.list[name]) %>% rename_col(.) %>% 
        inner_join(., plot_id, by = c("date", "plotid")) %>% 
        mutate(id = row_number() + id.max) %>% select(colorder(name))
      
      return(data.df)
      
    } else {
      
      if(name == "mortality" | name == "microsites" | name == "tree_quality" | name == "core"){
        
        id.max <- pull_id(name)
        
        tree_id <- tbl(KELuser, "tree") %>% select(tree_id = id, treeid, plot_id) %>%
          inner_join(., tbl(KELuser, "plot") %>% select(date, plot_id = id), by = "plot_id") %>%
          collect()
        
        data.df <- as.data.frame(data.list[name]) %>% rename_col(.) %>%
          inner_join(., tree_id, by = c("date", "treeid")) %>% 
          mutate(id = row_number() + id.max) %>% select(colorder(name))
        
        return(data.df)
        
      } else {
        
        if(name == "ring"){
          
          id.max <- pull_id(name)
          
          core_id <- tbl(KELuser, "core") %>% select(core_id = id, tree_id) %>%
            inner_join(., tbl(KELuser, "tree") %>% select(tree_id = id, treeid, plot_id), by = "tree_id") %>%
            inner_join(., tbl(KELuser, "plot") %>% select(plot_id = id, date), by = "plot_id") %>%
            collect()
          
          data.df <- as.data.frame(data.list[name]) %>% rename_col(.) %>%
            inner_join(., core_id, by = c("date", "treeid")) %>% 
            mutate(id = row_number() + id.max) %>% select(colorder(name))
          
          return(data.df)
          
        } else {
          
          if(name == "dist_tree"){
            
            id.max <- pull_id(name)
            
            data.df <- as.data.frame(data.list[name]) %>% rename_col(.) %>%
              mutate(id = row_number() + id.max) %>% select(colorder(name))
            
            return(data.df)
            
          } else {
            
            if(name == "dist_plot"){
              
              id.max <- pull_id(name)
              
              plot.id <- tbl(KELuser, "plot") %>% filter(census %in% 1) %>% 
                select(plotid, plot_id = id) %>% collect()
              
              data.df <- as.data.frame(data.list[name]) %>% rename_col(.) %>%
                inner_join(., plot.id, by = "plotid") %>% arrange(plotid, desc(type)) %>%
                mutate(id = row_number() + id.max) %>% select(colorder(name))
              
              return(data.df)
              
            } else {
              
              if(name == "dist_event"){
                
                id.max <- pull_id(name)
                
                dist.chrono.id <- tbl(KELuser, "dist_chrono") %>%
                  inner_join(., tbl(KELuser, "dist_plot"), by = c("dist_plot_id" = "id")) %>%
                  inner_join(., tbl(KELuser, "plot"), by = c("plot_id" = "id")) %>%
                  select(plotid, type, year, dist_chrono_id = id) %>%
                  collect()
                
                data.df <- as.data.frame(data.list[name]) %>% rename_col(.) %>%
                  inner_join(., dist.chrono.id, by = c("plotid", "type", "year")) %>% arrange(plotid, desc(type), year) %>%
                  mutate(id = row_number() + id.max) %>% select(colorder(name))
                
                return(data.df)
                
              } else {
                
                if(name == "deadwood_tree"){
                  
                  id.max <- pull_id(name)
                  
                  plot_id <- tbl(KELuser, "plot") %>% select(date, plotid, plot_id = id) %>% collect()
                  
                  data.df <- as.data.frame(data.list[name]) %>% rename_col(.) %>%
                    inner_join(., plot_id, by = c("date", "plotid")) %>% 
                    mutate(id = row_number() + id.max) 
                  
                  return(data.df)
                  
                } else {
                  
                  if(name == "deadwood_position"){
                    
                    id.max <- pull_id(name)
                    
                    plot_id <- tbl(KELuser, "plot") %>% select(date, plotid, plot_id = id) %>% collect()
                    
                    dwtree_id <- data.list$deadwood_tree %>% select(deadwood_id = id, plot_id, object_id)
                    
                    data.df <- as.data.frame(data.list[name]) %>% rename_col(.) %>%
                      inner_join(., plot_id, by = c("date", "plotid")) %>%
                      inner_join(., dwtree_id, by = c("plot_id", "object_id")) %>% 
                      mutate(id = row_number() + id.max) %>% select(colorder(name))
                    
                    return(data.df)
                    
                  } else {
                    
                    if(name == "parameters_plot"){
                      
                      id.max <- pull_id(name)
                      
                      data.df <- as.data.frame(data.list[name]) %>% rename_col(.) %>% 
                        mutate(id = row_number() + id.max) %>% select(colorder(name))
                      
                      return(data.df)
                      
                    } else {
                      
                      if(name == "dist_stand"){
                        
                        id.max <- pull_id(name)
                        
                        data.df <- as.data.frame(data.list[name]) %>% rename_col(.) %>%
                          arrange(stand, peakyear, npatch) %>%
                          mutate(id = row_number() + id.max) %>% select(colorder(name))
                        
                      } else {
                        
                        if(name == "dist_chrono"){
                          
                          id.max <- pull_id(name)
                          
                          dist.plot.id <- tbl(KELuser, "dist_plot") %>%
                            inner_join(., tbl(KELuser, "plot"), by = c("plot_id" = "id")) %>%
                            select(plotid, type, dist_plot_id = id) %>% collect()
                          
                          data.df <- as.data.frame(data.list[name]) %>% rename_col(.) %>%
                            inner_join(., dist.plot.id, by = c("plotid", "type")) %>% arrange(plotid, desc(type), year) %>%
                            mutate(id = row_number() + id.max) %>% select(colorder(name))
                          
                        } else {
                          
                          stop("Unknown name of the data file.")
                          
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }  
    }
  }
}

upload_data <- function(x){
  #' @description upload data into database
  #' @param x string of names of database tables into which data should be uploaded
  
  for (i in x) {
    
    data.df <- as.data.frame(data.list[i]) %>% rename_col(.)
    
    n <- length(data.df$id)
    
    dbWriteTable(conn = KELadmin, 
                 name = i,
                 value = data.df,
                 overwrite = F, append = T, row.names = F)
    
    print(paste(i, n, sep = ":"))
    
  }
}
