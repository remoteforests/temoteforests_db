paramsGetData <- function(plot.id, params){
  #'@description collects data from database required for parameters calculation 
  #'@param plot.id vector of plot ids as in 'plot' database table
  #'@param params vector of parameters for which data should be collected
  
  data.list <- list()
  
  for (i in params){
    
    if(!i %in% c("plot","tree","core","disturbance","deadwood","deadwood_tree",
                 "regeneration","regeneration_subplot", "canopy","mortality",
                 "temperature")) stop(paste("Unknown params:", i, sep = " "))

    # plot --------------------------------------------------------------------

    if(i == "plot"){
      
      data.list$plot <- tbl(KELuser, "plot") %>% filter(id %in% plot.id) %>%
        select(plot_id = id, plotid, subplot, lng, lat, aspect) %>%
        collect()
    }

    # tree --------------------------------------------------------------------

    if(i == "tree"){
      
      data.list$tree <- tbl(KELuser, "tree") %>% 
        filter(plot_id %in% plot.id,
               !onplot %in% c(0, 99)) %>%
        inner_join(., tbl(KELuser, "plot"), by = c("plot_id" = "id")) %>%
        select(plot_id, plotsize, dbh_min, treeid, species, dbh_mm, height_m, status, decay, decayht, decay_wood) %>%
        collect()
      
      data.list$wood_density <- tbl(KELuser, "wood_density") %>% select(-id) %>% collect()
      
      data.list$biomass_eq <- tbl(KELuser, "biomass_eq") %>% select(-id) %>% collect()
      
      data.list$recent_dist <-  tbl(KELuser, "tree") %>%
        filter(plot_id %in% plot.id,
               !status %in% c(0, 10, 15, 99),
               growth %in% c(-1, 1),
               treetype %in% "0",
               onplot %in% c(1, 2),
               !species %in% c("Lians", "99"),
               decay %in% c(-1:3)) %>%
        inner_join(., tbl(KELuser, "species_fk"), by = c("species" = "id")) %>%
        inner_join(., tbl(KELuser, "dist_param") %>% select(sp_group_dist, dbhth = dbh_mm), by = "sp_group_dist") %>%
        filter(dbh_mm >= dbhth) %>%
        inner_join(., tbl(KELuser, "plot") %>% select(plot_id = id, plotid), by = "plot_id") %>%
        inner_join(., tbl(KELuser, "spatial_hierarchy"), by = "plotid") %>%
        select(plot_id, stand, subplot, plotid, sp_type, dbh_mm, decay) %>%
        collect()
    }
    
    # core --------------------------------------------------------------------
    
    if(i == "core"){
      
      data.list$core <- tbl(KELuser, "core") %>%
        filter(coretype %in% 1,
               !crossdated %in% c(12, 20:22, 99),
               corestatus %in% c(0, 1),
               !is.na(missing_mm),
               !is.na(missing_years),
               missing_mm < 30,
               missing_years < 30) %>%
        select(core_id = id, tree_id, subcore, missing_years) %>%
        inner_join(., tbl(KELuser, "tree") %>% 
                     filter(plot_id %in% plot.id,
                            dbh_mm >= 100,
                            growth %in% 1,
                            treetype %in% "0" & onplot %in% c(1, 2) | treetype %in% c("m", "x"),
                            !species %in% c("Lians", "99")) %>%
                     select(tree_id = id, treeid, plot_id),
                   by = "tree_id") %>%
        inner_join(., tbl(KELuser, "plot") %>% select(plot_id = id, date, plotid), by = "plot_id") %>%
        inner_join(., tbl(KELuser, "ring") %>% select(ring_id = id, core_id), by = "core_id") %>%
        select(date, plotid, treeid, subcore, missing_years) %>%
        collect()
    }
    
    # disturbance -------------------------------------------------------------

    if(i == "disturbance"){
      
      data.list$disturbance <- tbl(KELuser, "dist_event") %>%
        inner_join(., tbl(KELuser, "dist_chrono"), by = c("dist_chrono_id" = "id")) %>%
        inner_join(., tbl(KELuser, "dist_plot"), by = c("dist_plot_id" = "id")) %>%
        filter(plot_id %in% plot.id,
               type %in% "full") %>%
        select(plot_id, year, kde) %>%
        collect()
    }
    
    # deadwood ----------------------------------------------------------------

    if(i == "deadwood"){
      
      data.list$deadwood <- tbl(KELuser, "deadwood") %>% 
        inner_join(., tbl(KELuser, "plot") %>% filter(id %in% plot.id), by = c("plot_id" = "id")) %>%
        filter((date < 2014 & dbh_mm >= 100) | (date >= 2014 & dbh_mm >= 60)) %>%
        select(plot_id, species, dbh_mm, decay) %>%
        collect()
      
      data.list$wood_density <- tbl(KELuser, "wood_density") %>% select(-id) %>% collect()
    }
    
    # deadwood_tree -----------------------------------------------------------

    if(i == "deadwood_tree"){
      
      data.list$deadwood_tree <- tbl(KELuser, "deadwood_position") %>%
        filter(!is.na(diameter_mm)) %>%
        group_by(deadwood_id) %>%
        summarise(diam_mean = mean(diameter_mm)) %>%
        right_join(., tbl(KELuser, "deadwood_tree") %>%
                     filter(plot_id %in% plot.id,
                            volume_plot_m3 > 0), 
                   by = c("deadwood_id" = "id")) %>%
        inner_join(., tbl(KELuser, "plot") %>% filter(!is.na(plotsize)), by = c("plot_id" = "id")) %>%
        select(plot_id, plotsize, species, decay, volume_plot_m3, diam_mean) %>%
        collect()
      
      data.list$wood_density <- tbl(KELuser, "wood_density") %>% select(-id) %>% collect()
    }

    # regeneration ------------------------------------------------------------

    if(i == "regeneration"){
      
      data.list$regeneration <- tbl(KELuser, "regeneration") %>% 
        inner_join(., tbl(KELuser, "plot") %>%
                     filter(id %in% plot.id,
                            !is.na(plotsize)),
                   by = c("plot_id" = "id")) %>%
        select(plot_id, plotsize, dbh_min, htclass, count) %>%
        collect()
    }

    # regeneration_subplot ----------------------------------------------------

    if(i == "regeneration_subplot"){
      
      data.list$regeneration_subplot <- tbl(KELuser, "regeneration_subplot") %>% 
        filter(plot_id %in% plot.id) %>%
        select(plot_id, htclass, browsing, count) %>% 
        collect()
    }

    # canopy ------------------------------------------------------------------

    if(i == "canopy"){
      
      data.list$canopy <- tbl(KELuser, "canopy_analysis") %>% 
        filter(plot_id %in% plot.id,
               parameter %in% "openness") %>%
        select(plot_id, value) %>% 
        collect()
    }

    # mortality ---------------------------------------------------------------

    if(i == "mortality"){
      
      mort.plotid <- tbl(KELuser, "plot") %>% filter(id %in% plot.id) %>% distinct(., plotid) %>% pull()
      
      data.list$mort_plot <- tbl(KELuser, "plot") %>%
        filter(plotid %in% mort.plotid & !census %in% 8) %>%
        group_by(plotid) %>% filter(n() > 1) %>% ungroup() %>%
        select(plot_id = id, plotid, date, census, plottype, plotsize, dbh_min) %>%
        collect() %>%
        group_by(plotid) %>%
        arrange(date, .by_group = T) %>%
        mutate(use = ifelse(census %in% c(5, 6, 7), 0, 1),
               use = case_when(
                 row_number() == 1 & lead(use, 1) %in% 0 ~ 0,
                 row_number() == 2 & lead(use, 1) %in% 1 ~ 1,
                 .default = use)) %>%
        filter(plottype %in% c(3, 4),
               use %in% 1) %>%
        filter(n() > 1) %>%
        mutate(plotsize = min(plotsize, na.rm = T),
               plotsize = case_when(
                 plotsize %in% 500 ~ 12.62,
                 plotsize %in% 1000 ~ 17.84,
                 plotsize %in% 1500 ~ 21.85),
               dbh_min = max(dbh_min, na.rm = T), 
               int = case_when(
                 row_number() == 1 ~ NA_integer_,
                 row_number() == 2 ~ date - lag(date, 1),
                 row_number() == 3 & (date - lag(date, 1)) >= 4 ~ date - lag(date, 1),
                 row_number() == 3 & (date - lag(date, 1)) < 4 ~ date - lag(date, 2)),
               int = ifelse(int %in% NA, max(int, na.rm = T), int)) %>%
        filter(int %in% c(4:10)) %>%
        mutate(n = ifelse(row_number() == 3 & (date - lag(date, 1)) < 4, 2.5, row_number())) %>%
        select(plot_id, date, plotid, plotsize, dbh_min, int, n)
    }

    # temperature -------------------------------------------------------------

    if(i == "temperature"){
      
      data.list$temperature <- tbl(KELuser, "plot") %>% filter(id %in% plot.id) %>%
        select(plot_id = id, longitude = lng, latitude = lat, altitude = altitude_m) %>%
        collect()
    }
  }
  
  return(data.list) 
}

deg2rad <- function(x){
  #'@description converts degrees to radians
  #'@param x value in degrees
  
  (x * pi) / 180
}

volume <- function(SM, density_gCm3){
  #'@description calculates tree volume (m3)
  #'@param SM stem mass (kg)
  #'@param density_gCm3 wood density (g/cm3)
  
  (SM * 0.001) / density_gCm3
}

interQuantileRange <- function(x){
  #'@description calculates range between 0.75 and 0.25 quantiles
  #'@param x vector of numerical values
  
  quantile(x, 0.75, na.rm = T, names = F) - quantile(x, 0.25, na.rm = T, names = F)
}

paramsCalculate <- function(data, params){
  #'@description calculates plot-level parameters
  #'@param data list of tables - output of paramsGetData function
  #'@param params vector of parameters which should be calculated
  
  data.params <- list()
  
  for (i in params){
    
    if(!i %in% c("plot","tree","core","disturbance","deadwood","deadwood_tree",
                 "regeneration","regeneration_subplot", "canopy","mortality",
                 "temperature")) stop(paste("Unknown params:", i, sep = " "))
    
    # plot --------------------------------------------------------------------

    if (i == "plot"){
      
      # nearest plot ------------------------------------------------------------

      plots <- data$plot %>% distinct(., plotid, subplot, lng, lat)
      
      P <- plots %>% select(lng, lat) %>% as.matrix()
      p <- distm(P, P)
      diag(p) <- NA
      
      plot.key <- plots %>% mutate(id = row_number()) %>% select(id, plotid) %>% deframe()
      
      data.params$nearest_plot <- melt(as.matrix(p), varnames = c("plotid", "second_plot"), value.name = "nearest_plot") %>%
        filter(!nearest_plot %in% c("NaN", NA)) %>%
        mutate(plotid = plot.key[plotid],
               second_plot = plot.key[second_plot]) %>%
        left_join(., plots %>% select(plotid, subplot), by = "plotid") %>%
        group_by(plotid) %>%
        filter((subplot %in% 0 & nearest_plot %in% min(nearest_plot)) | (!subplot %in% 0 & nearest_plot %in% sort(nearest_plot)[2])) %>%
        ungroup() %>%
        mutate(nearest_plot = round(nearest_plot, 0)) %>%
        inner_join(., data$plot %>% select(plot_id, plotid), by = "plotid") %>%
        select(plot_id, nearest_plot)

      # aspect southness --------------------------------------------------------

      data.params$aspect_southness <- data$plot %>% filter(!aspect %in% NA) %>% group_by(plot_id) %>%
        summarise(aspect_southness = round(cos(deg2rad(45) - deg2rad(aspect)) + 1, 0))
    }
    
    # tree --------------------------------------------------------------------

    if(i == "tree"){
        
      data.params$tree_params <- data$tree %>%
        filter(dbh_mm >= dbh_min) %>%
        mutate(ba = pi * dbh_mm ^ 2 / 4 / 1000000,
               decay = ifelse(decay %in% 99, NA, decay)) %>%
        group_by(plot_id) %>%
        summarise(plotsize = first(plotsize),
                  n_trees_live_100 = length(treeid[dbh_mm >= 100 & status %in% c(1:4)]),
                  n_trees_live_500 = length(treeid[dbh_mm >= 500 & status %in% c(1:4)]),
                  n_trees_live_700 = length(treeid[dbh_mm >= 700 & status %in% c(1:4)]),
                  n_trees_dead_100 = length(treeid[dbh_mm >= 100 & status %in% c(11:13,15,17:23)]),
                  n_trees_dead_500 = length(treeid[dbh_mm >= 500 & status %in% c(11:13,15,17:23)]),
                  n_trees_dead_700 = length(treeid[dbh_mm >= 700 & status %in% c(11:13,15,17:23)]),
                  ba_live_100 = sum(ba[dbh_mm >= 100 & status %in% c(1:4)]),
                  ba_live_100 = ifelse(ba_live_100 %in% 0, NA, ba_live_100),
                  ba_dead_100 = sum(ba[dbh_mm >= 100 & status %in% c(11:13,15,17:23)]),
                  ba_dead_100 = ifelse(ba_dead_100 %in% 0, NA, ba_dead_100),
                  dbh_max_live_100 = max(dbh_mm[dbh_mm >= 100 & status %in% c(1:4)]),
                  dbh_max_live_100 = ifelse(dbh_max_live_100 %in% -Inf, NA, dbh_max_live_100),
                  dbh_max_dead_100 = max(dbh_mm[dbh_mm >= 100 & status %in% c(11:13,15,17:23)]),
                  dbh_max_dead_100 = ifelse(dbh_max_dead_100 %in% -Inf, NA, dbh_max_dead_100),
                  dbh_mean_live_100 = mean(dbh_mm[dbh_mm >= 100 & status %in% c(1:4)]),
                  dbh_mean_live_100 = ifelse(dbh_mean_live_100 %in% "NaN", NA, dbh_mean_live_100),
                  dbh_mean_dead_100 = mean(dbh_mm[dbh_mm >= 100 & status %in% c(11:13,15,17:23)]),
                  dbh_mean_dead_100 = ifelse(dbh_mean_dead_100 %in% "NaN", NA, dbh_mean_dead_100),
                  dbh_quadrmean_live_100 = sqrt(mean(dbh_mm[dbh_mm >= 100 & status %in% c(1:4)]^2)),
                  dbh_quadrmean_live_100 = ifelse(dbh_quadrmean_live_100 %in% "NaN", NA, dbh_quadrmean_live_100),
                  dbh_quadrmean_dead_100 = sqrt(mean(dbh_mm[dbh_mm >= 100 & status %in% c(11:13,15,17:23)]^2)),
                  dbh_quadrmean_dead_100 = ifelse(dbh_quadrmean_dead_100 %in% "NaN", NA, dbh_quadrmean_dead_100),
                  dbh_gini_live_100 = ineq(dbh_mm[dbh_mm >= 100 & status %in% c(1:4)], type = "Gini"),
                  dbh_gini_live_100 = ifelse(dbh_gini_live_100 %in% "NaN", NA, dbh_gini_live_100),
                  dbh_gini_dead_100 = ineq(dbh_mm[dbh_mm >= 100 & status %in% c(11:13,15,17:23)], type = "Gini"),
                  dbh_gini_dead_100 = ifelse(dbh_gini_dead_100 %in% "NaN", NA, dbh_gini_dead_100),
                  dbh_div_live_100 = sd(dbh_mm[dbh_mm >= 100 & status %in% c(1:4)]),
                  dbh_div_dead_100 = sd(dbh_mm[dbh_mm >= 100 & status %in% c(11:13,15,17:23)]),
                  decay_div_standing_100 = sd(decay[dbh_mm >= 100 & status %in% c(11:13,15,17:23)], na.rm = T)) %>%
        mutate_at(vars(n_trees_live_100,
                       n_trees_live_500,
                       n_trees_live_700, 
                       n_trees_dead_100,
                       n_trees_dead_500, 
                       n_trees_dead_700,
                       ba_live_100,
                       ba_dead_100), 
                  list(~ .*10000/plotsize)) %>%
        mutate_at(vars(dbh_gini_live_100,
                       dbh_gini_dead_100,
                       dbh_div_live_100,
                       dbh_div_dead_100, 
                       decay_div_standing_100),
                  list(~ round(., 2))) %>%
        mutate_at(vars(-dbh_gini_live_100,
                       -dbh_gini_dead_100,
                       -dbh_div_live_100, 
                       -dbh_div_dead_100, 
                       -decay_div_standing_100),
                  list(~ round(., 0))) %>%
        select(-plotsize)
      
      # maximal height ----------------------------------------------------------
      
      data.params$height_max <- data$tree %>% 
        group_by(plot_id) %>% 
        summarise(height_max = max(height_m, na.rm = T)) %>%
        mutate(height_max = ifelse(height_max %in% -Inf, NA, height_max),
               height_max = round(height_max, 0))
      
      # dominant species --------------------------------------------------------
      
      data.params$dominant_species <- data$tree %>%
        filter(dbh_mm >= dbh_min,
               status %in% c(1:4)) %>%
        mutate(ba = pi * dbh_mm ^ 2 / 4 / 1000000) %>%
        group_by(plot_id, species) %>%
        summarise(ba = sum(ba) * 10000 / first(plotsize)) %>%
        group_by(plot_id) %>%
        arrange(desc(ba), .by_group = T) %>%
        filter(row_number() == 1) %>%
        ungroup() %>%
        select(plot_id, dominant_species = species)
      
      # biomass and volume of live trees ---------------------------------------
        
      data.params$biomass_volume_live <- data$tree %>%
        filter(dbh_mm >= dbh_min,
               status %in% c(1:4),
               !species %in% "99") %>%
        left_join(., data$wood_density %>% distinct(., species, density_gCm3), by = "species") %>%
        left_join(., data$biomass_eq, by = "species") %>%
        left_join(., data.params$tree_params %>% select(plot_id, ba = ba_live_100), by = "plot_id") %>%
        rowwise() %>%
        mutate(RM = eval(parse(text = root_mass_f)),
               SM = eval(parse(text = stem_mass_f)),
               BM = eval(parse(text = branches_mass_f)),
               FM = eval(parse(text = foliage_mass_f)),
               biomass_underground = RM,
               biomass_aboveground = SM + BM + FM,
               volume_live = volume(SM, density_gCm3)) %>%
        group_by(plot_id) %>%
        summarise(plotsize = first(plotsize),
                  biomass_underground_100 = sum(biomass_underground[dbh_mm >= 100]),
                  biomass_underground_100 = ifelse(biomass_underground_100 %in% 0, NA, biomass_underground_100),
                  biomass_aboveground_100 = sum(biomass_aboveground[dbh_mm >= 100]),
                  biomass_aboveground_100 = ifelse(biomass_aboveground_100 %in% 0, NA, biomass_aboveground_100),
                  volume_live_100 = sum(volume_live[dbh_mm >= 100]),
                  volume_live_100 = ifelse(volume_live_100 %in% 0, NA, volume_live_100)) %>%
        mutate_at(vars(-plot_id, -plotsize), list(~ .*10000/plotsize)) %>%
        mutate_at(vars(-plot_id, -plotsize), list(~ round(., 0))) %>%
        select(-plotsize)
      
      # volume and biomass of dead standing trees -------------------------------
      
      data(SK.par.lme)
      
      data.params$volume_dead_standing <- data$tree %>%
        filter(dbh_mm >= dbh_min,
               status %in% c(11:13,15,17:23),
               !decayht %in% 99) %>%
        mutate(decayht = case_when(
          decayht %in% 0 ~ 5,
          decayht %in% 1 ~ 15,
          decayht %in% 2 ~ 25,
          decayht %in% 3 ~ 35,
          decayht %in% 4 ~ 45,
          decayht %in% 5 ~ 55)) %>%
        rowwise() %>%
        mutate(volume_snag = E_VOL_AB_HmDm_HT.f(
          Hm = 1.3,
          Dm = (dbh_mm * 0.1),
          mHt = (log(dbh_mm * 0.1) - 1.08261)^2 / 0.275541,
          sHt = 0,
          A = 0,
          B = decayht,
          iDH = "H",
          par.lme = SK.par.lme)$E_VOL) %>%
        group_by(plot_id) %>%
        summarise(volume_dead_standing_100 = sum(volume_snag[dbh_mm >= 100]) * 10000 / first(plotsize),
                  volume_dead_standing_100 = ifelse(volume_dead_standing_100 %in% 0, NA, volume_dead_standing_100)) %>%
        mutate_at(vars(-plot_id), list(~ round(., 0)))
      
      data.params$biomass_dead_standing <- data$tree %>%
        filter(dbh_mm >= dbh_min,
               status %in% c(11:13,15,17:23),
               !decayht %in% 99,
               !species %in% "99",
               (decay_wood %in% c(1:5) | decay %in% c(1:5))) %>%
        mutate(decayht = case_when(
          decayht %in% 0 ~ 5,
          decayht %in% 1 ~ 15,
          decayht %in% 2 ~ 25,
          decayht %in% 3 ~ 35,
          decayht %in% 4 ~ 45,
          decayht %in% 5 ~ 55),
          decay_class = ifelse(decay_wood %in% 99, round(0.2924953 + 0.7131269 * decay, 0), decay_wood)) %>%
        left_join(., data$wood_density, by = c("species", "decay_class")) %>%
        rowwise() %>%
        mutate(
          volume_snag = E_VOL_AB_HmDm_HT.f(
            Hm = 1.3,
            Dm = (dbh_mm * 0.1),
            mHt = (log(dbh_mm * 0.1) - 1.08261)^2 / 0.275541,
            sHt = 0,
            A = 0,
            B = decayht,
            iDH = "H",
            par.lme = SK.par.lme)$E_VOL,
          biomass = volume_snag * (density_gCm3 * relative_density * 1000)) %>%
        group_by(plot_id) %>%
        summarise(biomass_dead_standing_100 = sum(biomass[dbh_mm >= 100]) * 10000 / first(plotsize),
                  biomass_dead_standing_100 = ifelse(biomass_dead_standing_100 %in% 0, NA, biomass_dead_standing_100)) %>%
        mutate_at(vars(-plot_id), list(~ round(., 0)))
      
      # recent disturbance ------------------------------------------------------
      
      data.ca <- tbl(KELuser, "dist_data_ca") %>% select(stand, subplot, plotid, sp_type, dbh_mm, ca_m2) %>% collect()
      
      data.conif <- data.ca %>% filter(sp_type %in% "coniferous")
        
      conif <- lmer(sqrt(ca_m2) ~ dbh_mm + (1|stand) + (1|subplot) + (1|plotid), data = data.conif)
        
      data.broad <- data.ca %>% filter(sp_type %in% "broadleaved")
        
      broad <- lmer(sqrt(ca_m2) ~ dbh_mm + (1|stand) + (1|subplot) + (1|plotid), data = data.broad)
        
      data.params$disturbance_recent <- data$recent_dist %>%
        mutate(ca_m2 = case_when(
                 sp_type %in% "broadleaved" ~ predict(object = broad, newdata = ., allow.new.levels = T),
                 sp_type %in% "coniferous" ~  predict(object = conif, newdata = ., allow.new.levels = T)),
               ca_m2 = ca_m2^2) %>%
        group_by(plot_id) %>%
        summarise(ca_dist = sum(ca_m2[decay %in% c(1:3)]),
                  ca_total = sum(ca_m2[decay %in% c(-1:3)])) %>%
        mutate(disturbance_recent = ca_dist * 100 / ca_total,
               disturbance_recent = ifelse(disturbance_recent %in% c(0,"NaN"), NA, disturbance_recent),
               disturbance_recent = round(disturbance_recent, 0)) %>%
        select(plot_id, disturbance_recent)
    }

    # core --------------------------------------------------------------------
    
    if(i == "core"){
      
      data.params$core_params <- data$core %>% 
        group_by(date, plotid, treeid, subcore) %>%                            
        summarise(age = sum(n(), first(missing_years))) %>%
        group_by(treeid) %>%
        arrange(desc(age), .by_group = T) %>%
        filter(row_number() == 1) %>%
        group_by(plotid) %>%
        mutate(age = ifelse(date %in% max(date), age - (max(date) - min(date)), age),
               date = min(date)) %>%
        ungroup() %>%
        inner_join(., tbl(KELuser, "plot") %>% select(plot_id = id, date, plotid), by = c("date", "plotid"), copy = T) %>%
        group_by(plot_id) %>%
        arrange(desc(age), .by_group = T) %>%
        summarise(age_max = max(age),
                  age_5oldest = mean(age[1:5]),
                  age_90quantile = quantile(age, 0.90),
                  age_mean = mean(age),
                  age_median = median(age),
                  age_iqr = interQuantileRange(age),
                  age_gini = round(ineq(age, type = "Gini"), 2)) %>%
        mutate_at(vars(-plot_id, -age_gini), list(~ round(., 0)))
    }
    
    # disturbance -------------------------------------------------------------

    if(i == "disturbance"){
          
      data.params$disturbance_params <- data$disturbance %>%
        group_by(plot_id) %>% 
        summarise(disturbance_max_year = year[which.max(kde)],
                  disturbance_max_severity = max(kde),
                  disturbance_last_year = max(year),
                  disturbance_last_severity = kde[year %in% max(year)]) %>%
        mutate_at(vars(-plot_id), list(~ round(., 0)))

      # disturbance index -------------------------------------------------------
      
      data.params$disturbance_index <- tbl(KELuser, "dist_chrono") %>%
        inner_join(., tbl(KELuser, "dist_plot"), by = c("dist_plot_id" = "id")) %>%
        filter(plot_id %in% plot.id,
               type %in% "full",
               year_min <= 1841,
               year >= 1841,
               year_max >= 1990,
               year <= 1990) %>%
        select(plot_id, year, ca_pct) %>%
        collect() %>%
        mutate(year = case_when(
          year >= 1841 & year < 1851 ~ 1840,
          year >= 1851 & year < 1861 ~ 1850,
          year >= 1861 & year < 1871 ~ 1860,
          year >= 1871 & year < 1881 ~ 1870,
          year >= 1881 & year < 1891 ~ 1880,
          year >= 1891 & year < 1901 ~ 1890,
          year >= 1901 & year < 1911 ~ 1900,
          year >= 1911 & year < 1921 ~ 1910,
          year >= 1921 & year < 1931 ~ 1920,
          year >= 1931 & year < 1941 ~ 1930,
          year >= 1941 & year < 1951 ~ 1940,
          year >= 1951 & year < 1961 ~ 1950,
          year >= 1961 & year < 1971 ~ 1960,
          year >= 1971 & year < 1981 ~ 1970,
          year >= 1981 & year < 1991 ~ 1980
        )) %>%
        filter(!is.na(year)) %>%
        group_by(plot_id, year) %>%
        summarise(ca = sum(ca_pct)) %>%
        ungroup() %>%
        spread(., year, ca, fill = 0) %>%
        mutate(disturbance_index = diversity(.[ ,c(2:ncol(.))]),
               disturbance_index = round(disturbance_index, 2)) %>%
        select(plot_id, disturbance_index)
    }

    # deadwood ----------------------------------------------------------------

    if(i == "deadwood"){
            
      data.params$volume_dead_lying_decay_100 <- data$deadwood %>%
        filter(!decay %in% 99,
               dbh_mm >= 100) %>%
        group_by(plot_id, decay) %>%
        summarise(volume_cwd = round(((pi^2 * sum((dbh_mm * 0.001)^2)) / 800) * 10000, 0)) %>%
        ungroup() %>%
        mutate(decay = paste0("volume_dead_lying_100_decay", decay)) %>%
        spread(., decay, volume_cwd, fill = 0)
            
      data.params$volume_dead_lying <- data$deadwood %>%
        group_by(plot_id) %>%
        summarise(volume_dead_lying_100 = round(((pi^2 * sum((dbh_mm[dbh_mm >= 100] * 0.001)^2)) / 800) * 10000, 0))
      
      data.params$biomass_dead_lying_100 <- data$deadwood %>%
        filter(!decay %in% 99,
               !species %in% "99",
               dbh_mm >= 100) %>%
        left_join(., data$wood_density, by = c("species", "decay" = "decay_class")) %>%
        group_by(plot_id, decay, species) %>%
        summarise(volume_cwd = ((pi^2 * sum((dbh_mm * 0.001)^2)) / 800) * 10000,
                  biomass = volume_cwd * (first(density_gCm3) * first(relative_density) * 1000)) %>%
        group_by(plot_id) %>%
        summarise(biomass_dead_lying_100 = round(sum(biomass), 0))
        
      data.params$div_dead_lying <- data$deadwood %>%
        mutate(decay = ifelse(decay %in% 99, NA, decay)) %>%
        group_by(plot_id) %>%
        summarise(diam_div_lying_100 = sd(dbh_mm[dbh_mm >= 100]),
                  decay_div_lying_100 = sd(decay[dbh_mm >= 100], na.rm = T)) %>%
        mutate_at(vars(-plot_id), list(~ round(., 2)))
    }
    
    # deadwood_tree -----------------------------------------------------------

    if(i == "deadwood_tree"){
              
      data.params$volume_dead_tree_lying_decay <- data$deadwood_tree %>%
        filter(!decay %in% 99) %>%
        group_by(plot_id, decay) %>%
        summarise(volume_cwd = round(sum(volume_plot_m3) * 10000 / first(plotsize), 0)) %>%
        ungroup() %>%
        mutate(decay = paste0("volume_dead_tree_lying_decay", decay)) %>%
        spread(., decay, volume_cwd, fill = 0)
              
      data.params$volume_dead_tree_lying <- data$deadwood_tree %>%
        group_by(plot_id) %>%
        summarise(volume_dead_tree_lying = round(sum(volume_plot_m3) * 10000 / first(plotsize), 0))
              
      data.params$biomass_dead_tree_lying <- data$deadwood_tree %>%
        filter(!decay %in% 99,
               !species %in% "99") %>%
        left_join(., data$wood_density, by = c("species", "decay" = "decay_class")) %>%
        mutate(biomass = volume_plot_m3 * (density_gCm3 * relative_density * 1000)) %>%
        group_by(plot_id) %>%
        summarise(biomass_dead_tree_lying = round(sum(biomass) * 10000 / first(plotsize), 0))
              
      data.params$div_dead_tree_lying <- data$deadwood_tree %>%
        mutate(decay = ifelse(decay %in% 99, NA, decay)) %>%
        group_by(plot_id) %>%
        summarise(diam_div_tree_lying = sd(diam_mean, na.rm = T),
                  decay_div_tree_lying = sd(decay, na.rm = T)) %>%
        mutate_at(vars(-plot_id), list(~ round(., 2)))
    }

    # regeneration -----------------------------------------------------------

    if(i == "regeneration"){
                  
      data.params$regeneration_htclass <- data$regeneration %>%
        #mutate(plotsize = ifelse(plotsize > 1000, 1000, plotsize)) %>%
        group_by(plot_id) %>%
        summarise(regeneration_50_130 = sum(count[htclass %in% 1]) * 10000 / first(plotsize),
                  regeneration_130_250 = sum(count[htclass %in% 2]) * 10000 / first(plotsize)) %>%
        mutate_at(vars(-plot_id), list(~ round(., 0)))
                  
      data.params$regeneration_250_dbh_min <- data$regeneration %>%
        mutate(#plotsize = ifelse(plotsize > 1000, 1000, plotsize),
               dbh_min = paste("regeneration_250", dbh_min, sep = "_")) %>%
        group_by(plot_id, dbh_min) %>%
        summarise(regeneration_250_dbh_min = round(sum(count[htclass %in% 3]) * 10000 / first(plotsize), 0)) %>%
        spread(dbh_min, regeneration_250_dbh_min, fill = NA)
    }

    # regeneration_subplot ----------------------------------------------------
       
    if(i == "regeneration_subplot"){
                    
      data.params$regeneration_0_50 <- data$regeneration_subplot %>%
        group_by(plot_id) %>%
        summarise(regeneration_0_50 = round(sum(count[htclass %in% 0]) * 10000 / 20, 0))
                    
      data.params$regeneration_browsing <- data$regeneration_subplot %>%
        filter(htclass %in% 1,
              !browsing %in% c(5, 99)) %>%
        group_by(plot_id) %>%
        summarise(regeneration_browsing = round(mean(browsing), 0))
    }

    # canopy ------------------------------------------------------------------

    if(i == "canopy"){
                      
      data.params$openness <- data$canopy %>%
        group_by(plot_id) %>%
        summarise(openness_mean = mean(value),
                  openness_gini = ineq(value, type = "Gini"),
                  patchiness = sd(value)) %>%
        mutate_at(vars(-plot_id), list(~ round(., 2)))
    }

    # mortality ---------------------------------------------------------------

    if(i == "mortality"){
                          
      mort.plot.id <- data$mort_plot %>% pull(plot_id)
                          
      data.params$mortality <- tbl(KELuser, "tree") %>% 
        filter(plot_id %in% mort.plot.id) %>%
        inner_join(., tbl(KELuser, "plot") %>% select(plot_id = id, date), by = "plot_id") %>%
        select(plot_id, date, treeid, treetype, onplot, x_m, y_m, census, status, dbh_mm) %>%
        collect() %>%
        group_by(treeid) %>%
        arrange(desc(date), .by_group = T) %>%
        mutate(treetype = first(treetype),
               onplot = first(onplot[!onplot %in% 99]),
               x_m = first(x_m[!is.na(x_m)]),
               y_m = first(y_m[!is.na(y_m)]),
               status_group = ifelse(status %in% c(1:4), 1, 0),
               status_group = ifelse(status %in% 99, NA, status_group),
               status_group = case_when(
                 is.na(status_group) & lead(status_group, 1) %in% 0 ~ 0,
                 is.na(status_group) & lag(status_group, 1) %in% 1 ~ 1,
                 .default = status_group),
               status_group = case_when(
                 is.na(status_group) & lead(status_group, 1) %in% 0 ~ 0,
                 is.na(status_group) & lag(status_group, 1) %in% 1 ~ 1,
                 .default = status_group),
               status_group = ifelse(row_number() == 2 & status_group %in% c(0, NA) & lag(status_group, 1) %in% 1, 1, status_group),
               status_group = ifelse(row_number() == 3 & status_group %in% c(0, NA) & lag(status_group, 1) %in% 1, 1, status_group),
               status_na = ifelse(is.na(status_group), 1, 0),
               status_na = max(status_na, na.rm = T),
               dbh_mm = case_when(
                 is.na(dbh_mm) & row_number() == 1 ~ first(dbh_mm[!is.na(dbh_mm)]),
                 is.na(dbh_mm) & row_number() == 2 ~ as.integer(round(mean(dbh_mm, na.rm = T), 0)),
                 is.na(dbh_mm) & row_number() == 3 ~ last(dbh_mm[!is.na(dbh_mm)]),
                 .default = dbh_mm)) %>%
        filter(!status_na %in% 1,
               !is.na(dbh_mm)) %>%
        mutate(
          dbh_mm = case_when(
            row_number() == 2 & status_group %in% 0 & lag(status_group, 1) %in% 0 & lag(dbh_mm, 1) > dbh_mm ~ lag(dbh_mm, 1),
            row_number() == 2 & status_group %in% 1 & lag(status_group, 1) %in% 1 & lag(dbh_mm, 1) < dbh_mm ~ lag(dbh_mm, 1),
            .default = dbh_mm),
          dbh_mm = case_when(
            row_number() == 3 & status_group %in% 0 & lag(status_group, 1) %in% 0 & lag(dbh_mm, 1) > dbh_mm ~ lag(dbh_mm, 1),
            row_number() == 3 & status_group %in% 1 & lag(status_group, 1) %in% 1 & lag(dbh_mm, 1) < dbh_mm ~ lag(dbh_mm, 1),
            .default = dbh_mm)) %>%
        filter(treetype %in% "0",
               !onplot %in% c(0, NA),
               status_group %in% 1) %>% 
        inner_join(., data$mort_plot, by = c("plot_id", "date")) %>%
        mutate(distance_m = sqrt(abs(x_m^2) + abs(y_m^2)),
               dbh_threshold = ifelse(dbh_mm >= dbh_min, 1, 0),
               dbh_threshold = min(dbh_threshold)) %>%
        filter(dbh_threshold %in% 1,
               distance_m <= plotsize) %>%
        ungroup() %>%
        do({
                              
          x <- .
                              
          bind_rows(
            x %>% filter(n %in% c(1,2), census %in% 0) %>%
              group_by(plot_id, date, plotid, int) %>%
              summarise(n = n()) %>% 
              group_by(plotid) %>%
              filter(n() > 1) %>%
              arrange(date, .by_group = T) %>%
              mutate(mortality = ifelse(row_number() == 2, 1 - ((n/lag(n, 1))^(1/int)), NA),
                     mortality = round(mortality * 100, 2)) %>%
              ungroup() %>%
              filter(!is.na(mortality) & plot_id %in% plot.id) %>%
              select(plot_id, mortality),
            x %>% filter(n %in% c(1,2,2.5)) %>%
              group_by(treeid) %>%
              mutate(census = max(census)) %>%
              filter(census %in% 0,
                     !n %in% 2) %>%
              group_by(plot_id, date, plotid, int) %>%
              summarise(n = n()) %>% 
              group_by(plotid) %>%
              filter(n() > 1) %>%
              arrange(date, .by_group = T) %>%
              mutate(mortality = ifelse(row_number() == 2, 1 - ((n/lag(n, 1))^(1/int)), NA),
                     mortality = round(mortality * 100, 2)) %>%
              ungroup() %>%
              filter(!is.na(mortality) & plot_id %in% plot.id) %>%
              select(plot_id, mortality),
            x %>% filter(n %in% c(2,3)) %>%
              group_by(treeid) %>%
              arrange(desc(date), .by_group = T) %>%
              mutate(census = first(census)) %>%
              filter(census %in% 0) %>%
              group_by(plot_id, date, plotid, int) %>%
              summarise(n = n()) %>% 
              group_by(plotid) %>%
              filter(n() > 1) %>%
              arrange(date, .by_group = T) %>%
              mutate(mortality = ifelse(row_number() == 2, 1 - ((n/lag(n, 1))^(1/int)), NA),
                     mortality = round(mortality * 100, 2)) %>%
              ungroup() %>%
              filter(!is.na(mortality) & plot_id %in% plot.id) %>%
              select(plot_id, mortality)
          )
        })
    }
    
    # temperature -------------------------------------------------------------
    
    if(i == "temperature"){
      
      load("C:/Users/Ondrej_Vostarek/Desktop/KEL/db/scripts/database/model_annual.rda")
      load("C:/Users/Ondrej_Vostarek/Desktop/KEL/db/scripts/database/model_vegetation.rda")
      
      plots <- data$temperature
      
      plots$temperature_annual <- predict(model_annual,plots)^2-100
      plots$temperature_vegetation <- predict(model_vegetation,plots)^2-100
      
      data.params$temperature <- plots %>% 
        select(plot_id, temp_mean_year = temperature_annual, temp_mean_vegetseason = temperature_vegetation) %>%
        mutate_at(vars(-plot_id), list(~ round(., 0)))
    }
  }
  
  return(data.params)
}

paramsCollect <- function(data){
  #'@description collects list of tables into one dataframe
  #'@param data list of tables - output of paramsCalculate function
  
  data.col <- data.frame(plot_id = NA)
  
  for(i in names(data)){
    
    data.col <- full_join(data.col, data[[i]], by = "plot_id")
  }
  
  data.col <- data.col %>%
    gather(., parameter, value, 2:ncol(.)) %>%
    filter(!is.na(value)) %>%
    mutate(value = as.character(value))
  
  return(data.col)
}
