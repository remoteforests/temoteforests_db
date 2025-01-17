rename_col <- function(x){
  #' @description rename columns of dataframe created from list
  #' @param x dataframe to rename
  
  names(x) <- gsub("^(.*)[:.:](.*)$", "\\2", names(x))
  
  return(x)
}

read_data <- function(path, name){
  #' @description read cleaned data
  #' @param path path to the data files
  #' @param name vector of names of the corresponding database tables 
  
  data.list <- list()
  
  for (i in name) {
    
    if(!i %in% c("plot", "tree", "mortality", "microsites", "tree_quality",
                 "deadwood","regeneration", "regeneration_subplot",
                 "reg_subplot_position", "soil_profile", "vegetation", 
                 "habitat_signs", "core", "ring", "canopy_analysis"))
      
      stop(paste("Unknown data type:", i, sep = " "))
    
    df <- tibble()
    
    # plot --------------------------------------------------------------------
    
    if(i == "plot"){
      
      for(j in list.files(path, pattern = "*_plot.csv", full.names = T)){
        
        df <- bind_rows(df, read.table(j, sep = ",", header = T, stringsAsFactors = F) %>%
                          rename_col(.) %>%
                          mutate(date = as.numeric(date),
                                 plotid = as.character(plotid),
                                 census = as.numeric(census),
                                 country = as.character(country),
                                 location = as.character(location),
                                 stand = as.character(stand),
                                 plot = as.character(plot),
                                 subplot = as.numeric(subplot),
                                 lng = as.numeric(lng),
                                 lat = as.numeric(lat),
                                 plotsize = as.numeric(plotsize),
                                 dbh_min = as.numeric(dbh_min),
                                 plottype = as.numeric(plottype),
                                 foresttype = as.character(foresttype),
                                 altitude_m = as.numeric(altitude_m),
                                 slope = as.numeric(slope),
                                 aspect = as.numeric(aspect),
                                 landform = as.numeric(landform),
                                 hillform = as.numeric(hillform),
                                 ownership = as.numeric(ownership)))
      }
    }  

    # tree --------------------------------------------------------------------
    
    if(i == "tree"){
        
      for(j in list.files(path, pattern = "*_tree.csv", full.names = T)){
        
        df <- bind_rows(df, read.table(j, sep = ",", header = T, stringsAsFactors = F) %>%
                          rename_col(.) %>%
                          mutate(date = as.numeric(date),
                                 plotid = as.character(plotid),
                                 treeid = as.character(treeid),
                                 treen = as.character(treen),
                                 onplot = as.numeric(onplot),
                                 treetype = as.character(treetype),
                                 x_m = as.numeric(x_m),
                                 y_m = as.numeric(y_m),
                                 status = as.numeric(status),
                                 census = as.numeric(census),
                                 growth = as.numeric(growth),
                                 layer = as.numeric(layer),
                                 species = as.character(species),
                                 dbh_mm = as.numeric(dbh_mm),
                                 height_m = as.numeric(height_m),
                                 crownht_m = as.numeric(crownht_m),
                                 crowndiam1_m = as.numeric(crowndiam1_m),
                                 crowndiam2_m = as.numeric(crowndiam2_m),
                                 decayht = as.numeric(decayht),
                                 decay_wood = as.numeric(decay_wood),
                                 decay = as.numeric(decay)))
      }
    }

    # mortality ---------------------------------------------------------------
        
    if(i == "mortality"){
          
      for(j in list.files(path, pattern = "*_mortality.csv", full.names = T)){
        
        df <- bind_rows(df, read.table(j, sep = ",", header = T, stringsAsFactors = F) %>%
                          rename_col(.) %>%
                          mutate(date = as.numeric(date),
                                 treeid = as.character(treeid),
                                 mort_agent = as.numeric(mort_agent)))
      }
    }

    # microsites --------------------------------------------------------------
          
    if(i == "microsites"){
            
      for(j in list.files(path, pattern = "*_microsites.csv", full.names = T)){
        
        df <- bind_rows(df, read.table(j, sep = ",", header = T, stringsAsFactors = F) %>%
                          rename_col(.) %>%
                          mutate(date = as.numeric(date),
                                 treeid = as.character(treeid),
                                 microsite = as.numeric(microsite),
                                 count = as.numeric(count),
                                 method = as.numeric(method)))
      }
    }

    # tree_quality ------------------------------------------------------------
    
    if(i == "tree_quality"){
              
      for(j in list.files(path, pattern = "*_tree_quality.csv", full.names = T)){
        
        df <- bind_rows(df, read.table(j, sep = ",", header = T, stringsAsFactors = F) %>%
                          rename_col(.) %>%
                          mutate(date = as.numeric(date),
                                 treeid = as.character(treeid),
                                 quality = as.numeric(quality)))
      }
    }

    # deadwood ----------------------------------------------------------------
              
    if(i == "deadwood"){
                
      for(j in list.files(path, pattern = "*_deadwood.csv", full.names = T)){
        
        df <- bind_rows(df, read.table(j, sep = ",", header = T, stringsAsFactors = F) %>%
                          rename_col(.) %>%
                          mutate(date = as.numeric(date),
                                 plotid = as.character(plotid),
                                 transect = as.numeric(transect),
                                 species = as.character(species),
                                 dbh_mm = as.numeric(dbh_mm),
                                 decay = as.numeric(decay)))
      }
    }

    # regeneration ------------------------------------------------------------
                
    if(i == "regeneration"){
                  
      for(j in list.files(path, pattern = "*_regeneration.csv", full.names = T)){
        
        df <- bind_rows(df, read.table(j, sep = ",", header = T, stringsAsFactors = F) %>%
                          rename_col(.) %>%
                          mutate(date = as.numeric(date),
                                 plotid = as.character(plotid),
                                 species = as.character(species),
                                 htclass = as.numeric(htclass),
                                 regeneratedon = as.numeric(regeneratedon),
                                 count = as.numeric(count)))
      }
    }          
                
    # regeneration_subplot ----------------------------------------------------

    if(i == "regeneration_subplot"){
                    
      for(j in list.files(path, pattern = "*_regeneration_subplot.csv", full.names = T)){
        
        df <- bind_rows(df, read.table(j, sep = ",", header = T, stringsAsFactors = F) %>%
                          rename_col(.) %>%
                          mutate(date = as.numeric(date),
                                 plotid = as.character(plotid),
                                 subplot_n = as.numeric(subplot_n),
                                 subplotsize_m2 = as.numeric(subplotsize_m2),
                                 species = as.character(species),
                                 htclass = as.numeric(htclass),
                                 browsing = as.numeric(browsing),
                                 regeneratedon = as.numeric(regeneratedon),
                                 count = as.numeric(count)))
      }
    }
                  
    # reg_subplot_position ----------------------------------------------------
                    
    if(i == "reg_subplot_position"){
                      
      for(j in list.files(path, pattern = "*_regref.csv", full.names = T)){
        
        df <- bind_rows(df, read.table(j, sep = ",", header = T, stringsAsFactors = F) %>%
                          rename_col(.) %>%
                          mutate(date = as.numeric(date),
                                 plotid = as.character(plotid),
                                 subplot_n = as.numeric(subplot_n),
                                 x_m = as.numeric(x_m),
                                 y_m = as.numeric(y_m)))
      }
    }

    # soil_profile ------------------------------------------------------------
                    
    if(i == "soil_profile"){
                      
      for(j in list.files(path, pattern = "*_soil.csv", full.names = T)){
        
        df <- bind_rows(df, read.table(j, sep = ",", header = T, stringsAsFactors = F) %>%
                          rename_col(.) %>%
                          mutate(date = as.numeric(date),
                                 plotid = as.character(plotid),
                                 sample = as.numeric(sample),
                                 soil_horizon = as.character(soil_horizon),
                                 depth_cm = as.numeric(depth_cm)))
      }
    }

    # vegetation --------------------------------------------------------------
                    
    if(i == "vegetation"){
      
      for(j in list.files(path, pattern = "*_vegetation.csv", full.names = T)){
        
        df <- bind_rows(df, read.table(j, sep = ",", header = T, stringsAsFactors = F) %>%
                          rename_col(.) %>%
                          mutate(date = as.numeric(date),
                                 plotid = as.character(plotid),
                                 sampling_date = as.POSIXct(sampling_date),
                                 large_gap = as.numeric(large_gap),
                                 gap_distance_m = as.numeric(gap_distance_m),
                                 vegetationht = as.numeric(vegetationht),
                                 vegetation_cover = as.numeric(vegetation_cover),
                                 vaccinium_myrtillus_per = as.numeric(vaccinium_myrtillus_per),
                                 rubus_per = as.numeric(rubus_per),
                                 bryopsida_per = as.numeric(bryopsida_per),
                                 polypodiopsida_per = as.numeric(polypodiopsida_per),
                                 poaceae_per = as.numeric(poaceae_per),
                                 ericaceae_per = as.numeric(ericaceae_per),
                                 other_per = as.numeric(other_per),
                                 biotope_quality = as.numeric(biotope_quality),
                                 biotope_trend = as.numeric(biotope_trend),
                                 large_herbivore_feces = as.numeric(large_herbivore_feces)))
      }
    }

    # habitat_signs -----------------------------------------------------------
                      
    if(i == "habitat_signs"){
                        
      for(j in list.files(path, pattern = "*_habitat.csv", full.names = T)){
        
        df <- bind_rows(df, read.table(j, sep = ",", header = T, stringsAsFactors = F) %>%
                          rename_col(.) %>%
                          mutate(date = as.numeric(date),
                                 plotid = as.character(plotid),
                                 sampling_date = as.POSIXct(sampling_date),
                                 animal_species = as.character(animal_species),
                                 gender = as.numeric(gender),
                                 habitat_sign_type = as.numeric(habitat_sign_type)))
      }
    }

    # core --------------------------------------------------------------------
    
    if(i == "core"){
                          
      for(j in list.files(path, pattern = "*_core.csv", full.names = T)){
        
        df <- bind_rows(df, read.table(j, sep = ",", header = T, stringsAsFactors = F) %>%
                          rename_col(.) %>%
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
    }

    # ring --------------------------------------------------------------------

    if(i == "ring"){
      
      for(j in list.files(path, pattern = "*_ring.csv", full.names = T)){
        
        df <- bind_rows(df, read.table(j, sep = ",", header = T, stringsAsFactors = F) %>%
                          rename_col(.) %>%
                          mutate(date = as.numeric(date),
                                 treeid = as.character(treeid),
                                 year = as.numeric(year),
                                 incr_mm = as.numeric(incr_mm)))
      }
    }
    
    # canopy_analysis ---------------------------------------------------------

    if(i == "canopy_analysis"){
      
      for (j in list.files(path, pattern = "*_clean.xlsx", full.names = T)){
        
        df <- bind_rows(df, read.xlsx(j, sheet = "db") %>%
                          gather(., key = "parameter", value = "value", 3:15) %>%
                          mutate(date = as.numeric(substr(date, 1, 4)),
                                 plotid = as.character(substr(file, 1, nchar(file) - 6)),
                                 transect = as.numeric(substr(file, nchar(file) - 4, nchar(file) - 4)),
                                 parameter = as.character(parameter),
                                 value = as.numeric(value)))
      }
    }
    
    data.list[[i]] <- df
    
    n <- length(data.list[[i]]$date)
    
    print(paste("read", i, n, sep = ": "))
  }
  
  return(data.list)
}
  
pull_id <- function(name){
  #' @description pull maximum id from database table
  #' @param name name of database table from which maximum id should be pulled
  
  id <- tbl(KELuser, name) %>% summarise(id = max(id, na.rm = T)) %>% pull(id)
  
  if(is.na(id)){id <- 0}
  
  return(id)
}

colorder <- function(name){
  #'@description pull column names from database table 
  #'@param name name of database table from which column names should be pulled
  
  tbl(KELuser, name) %>% colnames()
}

prepare_data <- function(data){
  #' @description prepare data for uploading into the database
  #' @param data list of tables
  
  plot_id <- tbl(KELuser, "plot") %>% 
    select(plot_id = id, date, plotid) %>% 
    collect()
  
  tree_id <- tbl(KELuser, "tree") %>%
    inner_join(., tbl(KELuser, "plot"), by = c("plot_id" = "id")) %>%
    select(tree_id = id, date, treeid) %>%
    collect()
  
  data.list <- list()

  for (i in names(data)){
  
    id.max <- pull_id(i)
    
    # plot, parameters_plot, dist_tree ----------------------------------------

    if(i %in% c("plot", "parameters_plot", "dist_tree")){
      
      data.list[[i]] <- as.data.frame(data[[i]]) %>% 
        mutate(id = row_number() + id.max) %>% 
        select(colorder(i))
    }
  
    # plot_id -----------------------------------------------------------------

    if(i %in% c("tree", "deadwood", "regeneration", "regeneration_subplot", "reg_subplot_position", 
                "soil_profile", "vegetation", "habitat_signs", "canopy_analysis")){
      
      data.list[[i]] <- as.data.frame(data[[i]]) %>% 
        inner_join(., plot_id, by = c("date", "plotid")) %>%
        arrange(date, plotid) %>%
        mutate(id = row_number() + id.max) %>% 
        select(colorder(i))
    }
        
    # tree_id -----------------------------------------------------------------

    if(i %in% c("mortality", "microsites", "tree_quality", "core")){
        
      data.list[[i]] <- as.data.frame(data[[i]]) %>% 
        inner_join(., tree_id, by = c("date", "treeid")) %>%
        arrange(date, treeid) %>%
        mutate(id = row_number() + id.max) %>% 
        select(colorder(i))
    }
    
    # ring --------------------------------------------------------------------

    if(i == "ring"){
      
      core_id <- tbl(KELuser, "core") %>%
        inner_join(., tbl(KELuser, "tree"), by = c("tree_id" = "id")) %>%
        inner_join(., tbl(KELuser, "plot"), by = c("plot_id" = "id")) %>%
        select(core_id = id, date, treeid) %>%
        collect()
       
      data.list[[i]] <- as.data.frame(data[[i]]) %>% 
        inner_join(., core_id, by = c("date", "treeid")) %>%
        arrange(date, treeid, year) %>%
        mutate(id = row_number() + id.max) %>% 
        select(colorder(i))
    }
          
    # dist_plot ---------------------------------------------------------------

    if(i == "dist_plot"){
      
      plot_id_1st_census <- tbl(KELuser, "plot") %>% 
        filter(census %in% 1) %>% 
        select(plot_id = id, plotid) %>% 
        collect()
              
      data.list[[i]] <- as.data.frame(data[[i]]) %>%
        inner_join(., plot_id_1st_census, by = "plotid") %>% 
        arrange(plotid, desc(type)) %>%
        mutate(id = row_number() + id.max) %>% 
        select(colorder(i))
    }

    # dist_chrono -------------------------------------------------------------
    
    if(i == "dist_chrono"){
      
      dist_plot_id <- tbl(KELuser, "dist_plot") %>%
        inner_join(., tbl(KELuser, "plot"), by = c("plot_id" = "id")) %>%
        select(dist_plot_id = id, plotid, type) %>% 
        collect()
      
      data.list[[i]] <- as.data.frame(data[[i]]) %>%
        inner_join(., dist_plot_id, by = c("plotid", "type")) %>%
        arrange(plotid, desc(type), year) %>%
        mutate(id = row_number() + id.max) %>% 
        select(colorder(i))
    }
    
    # dist_event --------------------------------------------------------------

    if(i == "dist_event"){
                
      dist_chrono_id <- tbl(KELuser, "dist_chrono") %>%
        inner_join(., tbl(KELuser, "dist_plot"), by = c("dist_plot_id" = "id")) %>%
        inner_join(., tbl(KELuser, "plot"), by = c("plot_id" = "id")) %>%
        select(dist_chrono_id = id, plotid, type, year) %>%
        collect()
                
      data.list[[i]] <- as.data.frame(data[[i]]) %>%
        inner_join(., dist_chrono_id, by = c("plotid", "type", "year")) %>% 
        arrange(plotid, desc(type), year) %>%
        mutate(id = row_number() + id.max) %>% 
        select(colorder(i))
    }
                
    # dist_stand --------------------------------------------------------------

    if(i == "dist_stand"){
                        
      data.list[[i]] <- as.data.frame(data[[i]]) %>%
        arrange(stand, peakyear, npatch) %>%
        mutate(id = row_number() + id.max) %>% 
        select(colorder(i))
    }
    
    # climate -----------------------------------------------------------------

    if(i == "climate"){
      
      plot_id_1st_census <- tbl(KELuser, "plot") %>% 
        filter(census %in% 1) %>% 
        select(plot_id = id, plotid) %>% 
        collect()
      
      data.list[[i]] <- as.data.frame(data[[i]]) %>%
        inner_join(., plot_id_1st_census, by = "plotid") %>%
        arrange(plotid, year, month) %>%
        mutate(id = row_number() + id.max,
               temp_mean = round(tmean, 2),
               prec_total = round(prec, 0)) %>%
        select(colorder(i))
    }
                   
    n <- length(data.list[[i]]$id)
    
    print(paste("prepare", i, n, sep = ": "))
  }                  

  return(data.list)
}

upload_data <- function(data){
  #' @description upload data into database
  #' @param data list of tables - output of prepare_data function
  
  for (i in names(data)){
    
    df <- as.data.frame(data[[i]])
    
    n <- length(df$id)
    
    dbWriteTable(conn = KELadmin, 
                 name = i,
                 value = df,
                 row.names = F,
                 overwrite = F, 
                 append = T)
    
    print(paste("upload", i, n, sep = ": "))
  }
}

delete_data <- function(name, id){
  #' @description delete rows from database table
  #' @param name name of the database table
  #' @param id vector of ids to be deleted
  
  dbExecute(KELadmin, paste0("delete from ", name,  " where id in (", paste(id, collapse = ','), ")"))
}