read_structural_data <- function(file){
  #' @description read the structural data from .xlsx
  #' @param file name of the .xlsx file
  
  data.list <- list()
  
  # plot
  
  data.list$plot <- openxlsx::read.xlsx(paste0(file), sheet = 1)
  
  if(!identical(c("date", "plotid", "census",	"country", "location", "stand",
                  "standshort",	"plot",	"subplot", "lng", "lat", "plotsize",
                  "dbh_min", "plottype", "foresttype", "altitude_m", "slope",	
                  "aspect", "landform",	"hillform"), 
                names(data.list$plot))) 
    
    stop('The plot data do not match with required table format.')
  
  data.list$plot <- data.list$plot %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           census = as.numeric(census),
           country = as.character(country),
           location = as.character(location),
           stand = as.character(stand),
           standshort = as.character(standshort),
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
           hillform = as.numeric(hillform))
  
  # tree
  
  data.list$tree <- openxlsx::read.xlsx(paste0(file), sheet = 2)
  
  if(!identical(c("date", "plotid", "treeid",	"treen", "onplot", "treetype",
                  "x_m",	"y_m",	"status", "census", "growth", "layer", "species",
                  "dbh_mm", "height_m", "crownht_m", "crowndiam1_m", "crowndiam2_m",	
                  "decay", "decay_wood", "decayht"), 
                names(data.list$tree))) 
    
    stop('The tree data do not match with required table format.')
  
  data.list$tree <- data.list$tree %>%
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
           decay = as.numeric(decay),
           decay_wood = as.numeric(decay_wood),
           decayht = as.numeric(decayht))
  
  # microsites

  data.list$microsites <- openxlsx::read.xlsx(paste0(file), sheet = 3)

  if(!identical(c("date", "treeid", "microsite", "count"),
                names(data.list$microsites)))

    stop('The microsites data do not match with required table format.')

  data.list$microsites <- data.list$microsites %>%
    mutate(date = as.numeric(date),
           treeid = as.character(treeid),
           microsite = as.numeric(microsite),
           count = as.numeric(count))

  # deadwood

  data.list$deadwood <- openxlsx::read.xlsx(paste0(file), sheet = 4)

  if(!identical(c("date", "plotid", "transect",	"species", "dbh_mm", "decay"),
                names(data.list$deadwood)))

    stop('The deadwood data do not match with required table format.')

  data.list$deadwood <- data.list$deadwood %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           transect = as.numeric(transect),
           species = as.character(species),
           dbh_mm = as.numeric(dbh_mm),
           decay = as.numeric(decay))
  
  # regeneration
  
  data.list$regeneration <- openxlsx::read.xlsx(paste0(file), sheet = 5)
  
  if(!identical(c("date", "plotid", "species", "htclass", "regeneratedon", "count"), 
                names(data.list$regeneration))) 
    
    stop('The regeneration data do not match with required table format.')
  
  data.list$regeneration <- data.list$regeneration %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           species = as.character(species),
           htclass = as.numeric(htclass),
           regeneratedon = as.numeric(regeneratedon),
           count = as.numeric(count))
  
  # regeneration_subplot
  
  data.list$regeneration_subplot <- openxlsx::read.xlsx(paste0(file), sheet = 6)
  
  if(!identical(c("date", "plotid", "subplot_n", "subplotsize_m2", "species", "htclass",
                  "browsing",	"regeneratedon", "count"), 
                names(data.list$regeneration_subplot))) 
    
    stop('The regeneration_subplot data do not match with required table format.')
  
  data.list$regeneration_subplot <- data.list$regeneration_subplot %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           subplot_n = as.numeric(subplot_n),
           subplotsize_m2 = as.numeric(subplotsize_m2),
           species = as.character(species),
           htclass = as.numeric(htclass),
           browsing = as.numeric(browsing),
           regeneratedon = as.numeric(regeneratedon),
           count = as.numeric(count))
  
  # mortality
  
  data.list$mortality <- openxlsx::read.xlsx(paste0(file), sheet = 7)

  if(!identical(c("date", "treeid", "mort_agent"),
                names(data.list$mortality)))

    stop('The mortality data do not match with required table format.')

  data.list$mortality <- data.list$mortality %>%
    mutate(date = as.numeric(date),
           treeid = as.character(treeid),
           mort_agent = as.numeric(mort_agent))
  
  
  return(data.list)
  
}

check_structural_data <- function(data, fk) {
  #' @description check the structural data for possible mistakes
  #' @param data list containing the structural data as produced by read_structural_data()
  #' @param fk list containing the _fk encoding as stored in the database
  
  error.list <- list()
  
  plot.check <- tree.db %>%
    select(date_old = date, plotid, country_old = country, location_old = location, stand_old = stand, plottype_old = plottype, foresttype_old = foresttype) %>%
    right_join(., data$plot, by = "plotid") %>%
    distinct(., .keep_all = TRUE) %>%
    mutate(plot_test = case_when(
      nchar(plot) == 1 ~ paste0("00", plot),
      nchar(plot) == 2 ~ paste0("0", plot),
      nchar(plot) == 3 ~ plot),
      B = case_when(
        nchar(plotid) == 10 ~ substr(plotid, 5, 6),
        nchar(plotid) == 11 | nchar(plotid) == 13 ~ substr(plotid, 5, 7),
        nchar(plotid) == 12 | nchar(plotid) == 14 ~ substr(plotid, 5, 8)),
      C = case_when(
        nchar(plotid) == 10 ~ substr(plotid, 8, 10),
        nchar(plotid) == 11 | nchar(plotid) == 13 ~ substr(plotid, 9, 11),
        nchar(plotid) == 12 | nchar(plotid) == 14 ~ substr(plotid, 10, 12)),
      D = ifelse(nchar(plotid) == 13 | nchar(plotid) == 14, as.numeric(substr(plotid, nchar(plotid), nchar(plotid))), 0))

  tree.check <- tree.db %>%
    select(treeid, status_old = status, dbh_old = dbh_mm) %>%
    right_join(., data$tree, by = "treeid")
    
  # plot
  
  error.list$P_date <- plot.check %>% filter(date <= date_old)
  error.list$P_census <- data$plot %>% filter(!census %in% fk$plot_census_fk)
  error.list$P_country <- data$plot %>% filter(!country %in% fk$country_fk)
  error.list$P_country_change <- plot.check %>% filter(!country %in% country_old)
  error.list$P_location <- data$plot %>% filter(!location %in% fk$location_fk)
  error.list$P_location_change <- plot.check %>% filter(!location %in% location_old)
  error.list$P_stand <- plot.check %>% filter(!stand %in% stand_old)
  error.list$P_standshort <- plot.check %>% filter(!standshort %in% B)
  error.list$P_plot <- plot.check %>% filter(!plot_test %in% C)
  error.list$P_subplot <- plot.check %>% filter(!subplot %in% D)
  error.list$P_plotsize <- data$plot %>% filter(!plotsize %in% c(1000, 1500))
  error.list$P_dbh_min <- data$plot %>% filter(!dbh_min %in% c(60, 100))
  error.list$P_plottype <- data$plot %>% filter(!plottype %in% fk$plottype_fk)
  error.list$P_plottype_change <- plot.check %>% filter(!plottype %in% plottype_old)
  error.list$P_foresttype <- data$plot %>% filter(!foresttype %in% fk$foresttype_fk)
  error.list$P_foresttype_change <- plot.check %>% filter(!foresttype %in% foresttype_old)
  
  # tree
  
  error.list$T_not_in_plot <- anti_join(data$tree, data$plot, by = c("date", "plotid"))
  error.list$T_onplot <- data$tree %>% filter(!onplot %in% fk$onplot_fk)
  error.list$T_treetype <- data$tree %>% filter(!treetype %in% fk$treetype_fk)
  error.list$T_treetype_onplot <- data$tree %>% filter(!onplot %in% 0 & !treetype %in% 0)
  error.list$T_status <- data$tree %>% filter(!status %in% fk$status_fk)
  error.list$T_growth <- data$tree %>% filter(!growth %in% fk$growth_fk)
  error.list$T_growth_alive <- data$tree %>% filter(status %in% c(1:4) & growth %in% -1)
  error.list$T_growth_dead <- data$tree %>% filter(!status %in% c(1:4) & !growth %in% -1)
  error.list$T_layer <- data$tree %>% filter(!layer %in% fk$layer_fk)
  error.list$T_layer_alive <- data$tree %>% filter(status %in% c(1:4) & layer %in% -1)
  error.list$T_layer_dead <- data$tree %>% filter(!status %in% c(1:4) & !layer %in% -1)
  error.list$T_species <- data$tree %>% filter(!species %in% fk$species_fk)
  error.list$T_dbh <- data$tree %>% filter(dbh_mm < unique(data$plot$dbh_min))
  error.list$T_dbh_alive <- tree.check %>% filter(dbh_mm < dbh_old) 
  error.list$T_dbh_dead <- tree.check %>% filter(!status %in% c(1:4) & !status_old %in% c(1:4) & dbh_mm > dbh_old)
  error.list$T_height <- data$tree %>% filter(crownht_m > height_m)
  error.list$T_decay <- data$tree %>% filter(!decay %in% fk$decay_fk)
  error.list$T_decay_alive <- data$tree %>% filter(status %in% c(1:4) & !decay %in% -1)
  error.list$T_decay_dead <- data$tree %>% filter(!status %in% c(1:4) & decay %in% -1)
  error.list$T_decay_wood <- data$tree %>% filter(!decay_wood %in% fk$decay_wood_fk)
  error.list$T_decay_wood_alive <- data$tree %>% filter(status %in% c(1:4) & !decay_wood %in% -1)
  error.list$T_decay_wood_dead <- data$tree %>% filter(!status %in% c(1:4) & decay_wood %in% -1)
  error.list$T_decayht <- data$tree %>% filter(!decayht %in% fk$decayheight_fk)
  error.list$T_decayht_alive <- data$tree %>% filter(status %in% c(1:4) & !decayht %in% -1)
  error.list$T_decayht_dead <- data$tree %>% filter(!status %in% c(1:4) & decayht %in% -1)
  
  # mortality
  
  error.list$Mo_mort_agent <- data$mortality %>% filter(!mort_agent %in% fk$mort_agent_fk)
  error.list$Mo_alive <- data$tree %>% filter(status %in% c(1:4)) %>% inner_join(., data$mortality, by = "treeid")
  error.list$Mo_dead <- data$tree %>% select(treeid, status_new = status) %>%
    inner_join(., 
               tree.db %>% select(treeid, status_old = status), 
               by = "treeid") %>% 
    filter(!status_old %in% c(1:4) & !status_new %in% c(1:4)) %>%
    inner_join(., data$mortality, by = "treeid")
  error.list$Mo_0_51 <- data$mortality %>% filter(mort_agent %in% c(0, 51)) %>% 
    mutate(plotid = substr(treeid, 1, nchar(treeid) - 4)) %>% 
    group_by(plotid, mort_agent) %>% 
    summarise(n = n()) %>%
    filter(mort_agent %in% 0 & n > 2 |
             mort_agent %in% 51 & n < 3)
  
  # microsites
  
  error.list$Mi_not_in_tree <- anti_join(data$microsites, data$tree, by = c("date", "treeid"))
  error.list$Mi_microsite <- data$microsites %>% filter(!microsite %in% fk$microsite_fk)
  error.list$Mi_count <- data$microsites %>% filter(microsite %in% c(11, 20, 23, 25, 26, 29, 32:41, 44:47) & count > 1)
  
  # deadwood
  
  error.list$D_not_in_plot <- anti_join(data$deadwood, data$plot, by = c("date", "plotid"))
  error.list$D_transect <- data$deadwood %>% filter(!transect %in% fk$transect_fk)
  error.list$D_species <- data$deadwood %>% filter(!species %in% fk$species_fk)
  error.list$D_dbh_mm <- data$deadwood %>% filter(dbh_mm %in% 0)
  error.list$D_decay <- data$deadwood %>% filter(!decay %in% fk$decay_wood_fk)
  
  # regeneration
  
  error.list$R_not_in_plot <- anti_join(data$regeneration, data$plot, by = c("date", "plotid"))
  error.list$R_species <- data$regeneration %>% filter(!species %in% fk$species_fk)
  error.list$R_htclass <- data$regeneration %>% filter(!htclass %in% fk$htclass_fk)
  error.list$R_regeneratedon <- data$regeneration %>% filter(!regeneratedon %in% fk$regeneratedon_fk)
  error.list$R_count <- data$regeneration %>% filter(count %in% 0)
  
  # regeneration_subplot
  
  error.list$RS_not_in_plot <- anti_join(data$regeneration_subplot, data$plot, by = c("date", "plotid"))
  error.list$RS_subplot_n <- data$regeneration_subplot %>% filter(!subplot_n %in% fk$transect_fk)
  error.list$RS_subplotsize_m2 <- data$regeneration_subplot %>% filter(!subplotsize_m2 %in% 4)
  error.list$RS_species <- data$regeneration_subplot %>% filter(!species %in% fk$species_fk)
  error.list$RS_htclass <- data$regeneration_subplot %>% filter(!htclass %in% fk$htclass_fk)
  error.list$RS_browsing <- data$regeneration_subplot %>% filter(!browsing %in% fk$browsing_fk)
  error.list$RS_regeneratedon <- data$regeneration_subplot %>% filter(!regeneratedon %in% fk$regeneratedon_fk)
  error.list$RS_count <- data$regeneration_subplot %>% filter(count %in% 0)
  
  return(error.list)
  
}

clean_structural_data <- function(data){
  #' @description clean and prepare the structural data
  #' @param data list containing the structural data as produced by read_structural_data()
  
  data.clean <- list()
  
  # plot
  
  detach(package:tidyverse, unload = TRUE)
  
  library(rgdal);library(raster)
  
  DEM <- raster("C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/Aster_DEM/Europe/DEM.tif")
  
  data$plot$altitude_m <- extract(DEM, data$plot[,c("lng","lat")], method = "bilinear")
  
  detach(package:rgdal, unload = TRUE); detach(package:raster, unload = TRUE)
  
  library(tidyverse)
  
  census <- tbl(KELuser, "plot") %>% 
    filter(plotid %in% plot.id & !date %in% date.id) %>%
    group_by(plotid) %>% 
    arrange(desc(date)) %>%
    filter(row_number() == 1) %>%
    select(plot_id = id, plotid, plotsize_old = plotsize) %>%
    inner_join(., tbl(KELuser, "tree") %>% filter(!onplot %in% 0), by = "plot_id") %>%
    mutate(n_pos = ifelse(is.na(x_m), 0, 1)) %>%
    collect() %>%
    group_by(plotid, plotsize_old) %>%
    summarise(n_pos = sum(n_pos),
              n_trees = n(),
              coef_old = (n_pos/n_trees) * 100) %>%
    right_join(.,
               data$plot %>%
                 select(date, plotid, plotsize) %>%
                 inner_join(., data$tree %>% filter(!onplot %in% 0), by = c("date", "plotid")) %>%
                 mutate(n_pos = ifelse(is.na(x_m), 0, 1)) %>%
                 group_by(plotid, plotsize) %>%
                 summarise(n_pos = sum(n_pos),
                           n_trees = n(),
                           coef_new = (n_pos/n_trees) * 100),
               by = "plotid") %>%
    mutate(census_new = case_when(
      is.na(plotsize_old) ~ 1,
      !is.na(plotsize_old) & plotsize %in% plotsize_old & coef_new >= 75 & coef_old >= 75 ~ 2,
      !is.na(plotsize_old) & plotsize > plotsize_old & coef_new >= 75 & coef_old >= 75 ~ 3,
      !is.na(plotsize_old) & plotsize < plotsize_old & coef_new >= 75 & coef_old >= 75 ~ 4,
      !is.na(plotsize_old) & plotsize %in% plotsize_old & (coef_new < 75 | coef_old < 75) ~ 5,
      !is.na(plotsize_old) & plotsize > plotsize_old & (coef_new < 75 | coef_old < 75) ~ 6,
      !is.na(plotsize_old) & plotsize < plotsize_old & (coef_new < 75 | coef_old < 75) ~ 7)) 
  
  data$plot$census <- census$census_new[data$plot$plotid %in% census$plotid]
  
  data.clean$plot <- data$plot %>% mutate(altitude_m = round(altitude_m, 0), slope = round(slope, 0), ownership = 1)
  
  # mortality
  
  data.clean$mortality <- data$tree %>% 
    select(treeid, status_new = status) %>%
    inner_join(., 
               tree.db %>% select(treeid, status_old = status), 
               by = "treeid") %>% 
    filter(status_old %in% c(1:4) & !status_new %in% c(1:4)) %>%
    left_join(., data$mortality, by = "treeid") %>%
    mutate(date = ifelse(date %in% NA, date.id, date),
           mort_agent = ifelse(mort_agent %in% NA, 99, mort_agent),
           mort_agent = case_when(
             mort_agent %in% 99 & status_new %in% c(21:23) ~ 411,
             mort_agent %in% 99 & status_new %in% 15 ~ 21,
             TRUE ~ mort_agent)) %>%
    distinct(., date, treeid, mort_agent)
  
  # tree
  
  data.clean$tree <- data$tree %>%
    inner_join(., 
               data.clean$plot %>% select(plotid, foresttype),
               by = "plotid") %>%
    left_join(.,
              tree.db %>% select(treeid, old_x = x_m),
              by = "treeid") %>%
    left_join(., 
              data.clean$mortality %>% filter(mort_agent %in% c(111:113, 121:133, 141:143, 411:413)),
              by = c("date", "treeid")) %>%
    mutate(dbh_mm = ifelse(is.na(dbh_mm), 0, dbh_mm),
           distance_m = sqrt(abs(x_m^2 + y_m^2)) + dbh_mm/1000/2,
           dbh_mm = ifelse(dbh_mm %in% 0, NA, dbh_mm),
           onplot = case_when(
             foresttype %in% "spruce" & distance_m <= 17.84 ~ 1,
             foresttype %in% "beech" & distance_m <= 7.99 ~ 1,
             foresttype %in% "beech" & distance_m > 7.99 & distance_m <= 17.84 ~ 2,
             foresttype %in% "beech" & distance_m > 17.84 & distance_m <= 21.85 ~ 3,
             distance_m %in% NA ~ onplot,
             TRUE ~ 0),
           census = case_when(
             !treeid %in% tree.db$treeid & plotid %in% data.clean$plot$plotid[census %in% c(3, 6)] & distance_m > 12.62 ~ 3,
             !treeid %in% tree.db$treeid & plotid %in% data.clean$plot$plotid[census %in% c(3, 6)] & distance_m <= 12.62 & dbh_mm < 100 ~ 3,
             !treeid %in% tree.db$treeid & plotid %in% data.clean$plot$plotid[census %in% c(3, 6)] & distance_m <= 12.62 & dbh_mm >= 100 & dbh_mm <= 150 ~ 1,
             !treeid %in% tree.db$treeid & plotid %in% data.clean$plot$plotid[census %in% c(3, 6)] & distance_m <= 12.62 & dbh_mm > 150 ~ 2,
             !treeid %in% tree.db$treeid & !plotid %in% data.clean$plot$plotid[census %in% c(3, 6)] & dbh_mm < 100 ~ 3,
             !treeid %in% tree.db$treeid & !plotid %in% data.clean$plot$plotid[census %in% c(3, 6)] & dbh_mm >= 100 & dbh_mm <= 150 ~ 1,
             !treeid %in% tree.db$treeid & !plotid %in% data.clean$plot$plotid[census %in% c(3, 6)] & dbh_mm > 150 ~ 2,
             treeid %in% tree.db$treeid & is.na(old_x) ~ 3,
             TRUE ~ 0),
           status = case_when(
             mort_agent %in% c(111:113) ~ 12,
             mort_agent %in% c(121:133) ~ 13,
             mort_agent %in% c(141:143) ~ 14,
             mort_agent %in% 411 & status %in% 11 ~ 21,
             mort_agent %in% 411 & status %in% 12 ~ 22,
             mort_agent %in% 411 & status %in% 13 ~ 23,
             mort_agent %in% 412 & status %in% 11 ~ 21,
             mort_agent %in% 412 & status %in% 12 ~ 22,
             mort_agent %in% 412 & status %in% 13 ~ 23,
             mort_agent %in% 413 & status %in% 11 ~ 21,
             mort_agent %in% 413 & status %in% 12 ~ 22,
             mort_agent %in% 413 & status %in% 13 ~ 23,
             mort_agent %in% 21 ~ 15,
             TRUE ~ as.numeric(status))) %>%
    group_by(treeid) %>%
    arrange(desc(status)) %>%
    filter(row_number() == 1) %>%
    select(-distance_m, -foresttype, -old_x, -mort_agent)
  
  # tree_quality
  
  data.clean$tree_quality <- data.clean$tree %>%
    inner_join(., tree.db %>% select(treeid, onplot, treetype, x_m, y_m, status, growth, layer, species, dbh_mm, height_m, decay, decayht), by = "treeid") %>%
    mutate(x_m_diff = ifelse(!x_m.x %in% NA & !x_m.y %in% NA, abs(x_m.x - x_m.y), 0),
           y_m_diff = ifelse(!y_m.x %in% NA & !y_m.y %in% NA, abs(y_m.x - y_m.y), 0),
           diff_m = sqrt(x_m_diff^2 + y_m_diff^2),
           quality1 = ifelse(!species.x %in% species.y, 1, NA),
           quality2 = case_when(  
             status.x %in% c(1:4) & !status.y %in% c(1:4, 99) ~ 2,
             status.x %in% c(1:4) & status.y %in% c(1:4) & status.x < status.y ~ 2,
             status.x %in% 0 & status.y %in% 10 ~ 2,
             status.x %in% 10 & status.y %in% 0 ~ 2,
             status.x %in% c(11:14) & status.y %in% c(11:14) & status.x < status.y ~ 2,
             status.x %in% c(21:23) & status.y %in% c(21:23) & status.x < status.y ~ 2,
             status.x %in% 17 & !status.y %in% c(1, 11, 99) ~ 2,
             status.x %in% 11 & status.y %in% 17 ~ 2,
             status.x %in% c(21:23) & !status.y %in% c(1:4, 21:23) ~ 2,
             status.x %in% 16 & status.y %in% c(0, 4, 10, 14) ~ 2,
             status.x %in% c(0, 10:14) & status.y %in% 16 ~ 2,
             status.x %in% 15 & !status.y %in% c(1, 11, 99) ~ 2),
           quality3 = ifelse(growth.x %in% c(0, 1, 99) & growth.y %in% -1, 3, NA),
           quality4 = case_when(
             dbh_mm.x < dbh_mm.y ~ 4,
             !status.x %in% c(1:4) & !status.y %in% c(1:4) & dbh_mm.x > dbh_mm.y ~ 4),
           quality5 = case_when(
             decay.x %in% c(1:5) & decay.y %in% (1:5) & decay.x < decay.y ~ 5,
             decay.x %in% -1 & !decay.y %in% -1 ~ 5),
           quality6 = ifelse(!onplot.x %in% onplot.y, 6, NA),
           quality7 = ifelse(diff_m > 0.75, 7, NA),
           quality8 = ifelse(layer.x %in% c(11:13, 99) & layer.y %in% -1, 8, NA),
           quality9 = case_when(
             decayht.x %in% -1 & !decayht.y %in% -1 ~ 9,
             decayht.x %in% c(1:5) & decayht.y %in% c(1:5) & decayht.x < decayht.y ~ 9),
           quality10 = case_when(
             status.x %in% 1 & height_m.x < height_m.y ~ 10,
             status.x %in% c(11, 15, 21) & height_m.x < height_m.y ~ 10),
           quality11 = ifelse(!treetype.x %in% treetype.y, 11, NA)) %>%
    select(date, treeid, quality1:quality11) %>%
    gather(., key, quality, quality1:quality11) %>%
    filter(!quality %in% NA) %>%
    select(-key)
  
  # microsites
  
  data.clean$microsites <- data$microsites %>% 
    mutate(count = ifelse(microsite %in% c(11, 20, 23, 25, 26, 29, 32:41, 44:47), NA, count),
           method = 2) %>%
    distinct(., .keep_all = T)
  
  # deadwood
  
  data.clean$deadwood <- distinct(data$deadwood, .keep_all = T)
  
  # regeneration
  
  data.clean$regeneration <- distinct(data$regeneration, .keep_all = T)

  # regeneration_subplot

  data.clean$regeneration_subplot <- distinct(data$regeneration_subplot, .keep_all = T)
    
  return(data.clean)
  
}

read_dendro_data <- function(file){
  #' @description read the dendrochronological data from .csv
  #' @param file name of the .csv file
  
  data.list <- list()
  
  # core
  
  data.list$core <- read.table(paste0(file), sep = ",", header = T, stringsAsFactors = F)
  
  if(!identical(c("date", "treeid", "coreht_m",	"missing_mm", "missing_years", "corestatus",
                  "crossdated",	"cormach"), 
                names(data.list$core))) 
    
    stop('The core data do not match with required table format.')
  
  data.list$core <- data.list$core %>%
    mutate(date = as.numeric(date),
           treeid = as.character(treeid),
           subcore = "a",
           coreht_m = as.numeric(coreht_m),
           missing_mm = as.numeric(missing_mm),
           missing_years = as.numeric(missing_years),
           corestatus = as.numeric(corestatus),
           crossdated = as.numeric(crossdated),
           cormach = as.numeric(cormach))
  
  # ring
  
  data.list$ring <- read.table(paste0(file), sep = ",", header = T, stringsAsFactors = F)
  
  if(!identical(c("date", "treeid", "year",	"incr_mm"), 
                names(data.list$ring))) 
    
    stop('The ring data do not match with required table format.')
  
  data.list$ring <- data.list$ring %>%
    mutate(date = as.numeric(date),
           treeid = as.character(treeid),
           year = as.numeric(year),
           incr_mm = as.numeric(incr_mm))
  
  return(data.list)
  
}

check_dendro_data <- function(data, fk) {
  #' @description check the dendrochronological data for possible mistakes
  #' @param data list containing the dendrochronological data as produced by read_dendro_data()
  #' @param fk list containing the _fk encoding as stored in the database

  error.list <- list()
  
  # core
  
  error.list$C_not_in_tree <- anti_join(data$core, tree.db, by = c("date", "treeid"))
  error.list$C_not_in_ring <- anti_join(data$core, data$ring, by = c("date", "treeid"))
  error.list$C_corestatus <- data$core %>% filter(!corestatus %in% fk$corestatus_fk)
  error.list$C_crossdated <- data$core %>% filter(!crossdated %in% fk$crossdated_fk)
  error.list$C_missing <- data$core %>% mutate(err = case_when(
    corestatus %in% 0 & !missing_years %in% 0 ~ 1,
    corestatus %in% 0 & !missing_mm %in% 0 ~ 1,
    corestatus %in% 1 & missing_years %in% c(NA, 0) ~ 1,
    corestatus %in% 1 & missing_mm %in% c(NA, 0) ~ 1,
    corestatus %in% c(2, 3) & !is.na(missing_years) ~ 1,
    corestatus %in% c(2, 3) & !is.na(missing_mm) ~ 1)) %>% filter(err %in% 1)
  
  # ring
  
  error.list$R_not_in_core <- anti_join(data$ring, data$core, by = c("date", "treeid"))
  error.list$R_year <- data$ring %>% filter(year %in% min(year))
  error.list$R_incr_mm <- data$ring %>% filter(is.na(incr_mm))
  
  return(error.list)
  
}

colorder <- function(name){tbl(KELuser, name) %>% colnames()}

movingSum <- function(x, windowLength = 11){
  rollapply( x, 
             width = windowLength,
             FUN = sum,
             fill = NA,
             align = "center",
             na.rm = T,
             partial = TRUE)
}

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
                                                    crossdated = as.numeric(crossdated)))
                      
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
       name == "parameters_plot" | name == "canopy_analysis") {
      
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
            inner_join(., core_id, by = c("date", "plotid")) %>% 
            mutate(id = row_number() + id.max) %>% select(colorder(name))
          
          return(data.df)
          
        } else {
         
          if(name == "dist_tree"){
            
            id.max <- pull_id(name)
            
            data.df <- as.data.frame(data.list[name]) %>% rename_col(.) %>%
              mutate(dbh_mm = round(dbh_mm, digits = 0), id = row_number() + id.max) %>% 
              select(colorder(name))
            
            return(data.df)
          
          } else {
            
            if(name == "dist_plot"){
              
              id.max <- pull_id(name)
              
              plot_id <- tbl(KELuser, "plot") %>% filter(census %in% 1) %>% 
                select(plotid, plot_id = id) %>% collect()
              
              data.df <- as.data.frame(data.list[name]) %>% 
                rename_col(.) %>%
                separate(., 
                         plotid, 
                         c('foresttype','country', 'location', 'stand', 'plotid'), 
                         sep = "/") %>%
                inner_join(., plot_id, by = c("plotid")) %>%
                group_by(plot_id) %>%
                mutate(severity = movingSum(ca)) %>%
                ungroup() %>%
                mutate(ca = round(ca, digits = 2),
                       value = round(value, digits = 5),
                       id = row_number() + id.max) %>%
                select(id, plot_id, year, ca_per = ca, kde = value, severity)
                
              return(data.df)
              
            } else {
              
              if(name == "dist_plot_event"){
                
                id.max <- pull_id(name)
                
                event_id <- tbl(KELuser, "dist_plot") %>%
                  inner_join(., tbl(KELuser, "plot"), by = c("plot_id" = "id")) %>%
                  select(dist_plot_id = id, plotid, year) %>%
                  collect()
                
                data.df <- as.data.frame(data.list[name]) %>%
                  rename_col(.) %>%
                  filter(method %in% "10_10_5") %>%
                  separate(., 
                           plotid,
                           c('foresttype','country', 'location', 'stand', 'plotid'), 
                           sep = "/") %>%
                  left_join(., event_id, by = c("plotid", "year")) %>%
                  mutate(method = 1,
                         id = row_number() + id.max) %>%
                  select(id, dist_plot_id, dist_plot_method_fk_id = method)
                
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
                    
                    dwtree_id <- data.list$deadwood_tree %>% select(deadwood_tree_id = id, plotid, ID)
                    
                    data.df <- as.data.frame(data.list[name]) %>% rename_col(.) %>%
                      inner_join(., dwtree_id, by = c("plotid", "IDT")) %>% 
                      mutate(id = row_number() + id.max) %>% select(colorder(name))
                    
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

upload_data <- function(x){
  #' @description upload the data into database
  #' @param x string of names of the database tables into which the data should be uploaded
  
  for (i in x) {
    
    data.df <- as.data.frame(data.list[i]) %>% rename_col(.)
    
    dbWriteTable(conn = KELadmin, 
                 name = i,
                 value = data.df,
                 overwrite = F, append = T, row.names = F)
    
  }
}

rename_col <- function(x){
  #' @description rename columns
  #' @param x data table to rename
  
  names(x) <- gsub("^(.*)[:.:](.*)$", "\\2", names(x))
  
  return(x)
  
}

pull_id <- function(name){
  #' @description pull the maximal id from a database table
  #' @param name name of the database table from which the id should be pulled
  
  id <- tbl(KELuser, name) %>% filter(id == max(id, na.rm = TRUE)) %>% pull(id)
  
}