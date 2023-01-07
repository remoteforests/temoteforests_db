# setup -------------------------------------------------------------------

library(pool);library(tidyverse)

source("pw.R")

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
  
  if(!identical(c("date", "plotid", "treeid",	"treen", "stem", "onplot", "treetype",
                  "x_m",	"y_m",	"status", "growth", "layer", "species",
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
  
  if(!identical(c("date", "plotid", "species", "htclass", "browsing", "regeneratedon", "count"),
                names(data.list$regeneration)))
    
    stop('The regeneration data do not match with required table format.')
  
  data.list$regeneration <- data.list$regeneration %>%
    select(-browsing) %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           species = as.character(species),
           htclass = as.numeric(htclass),
           regeneratedon = as.numeric(regeneratedon),
           count = as.numeric(count))
  
  # soil
  
  data.list$soil <- openxlsx::read.xlsx(paste0(file), sheet = 8)
  
  if(!identical(c("date", "plotid", "sample",	"soil_horizon", "depth_cm"), 
                names(data.list$soil))) 
    
    stop('The soil data do not match with required table format.')
  
  data.list$soil <- data.list$soil %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           sample = as.numeric(sample),
           soil_horizon = as.character(soil_horizon),
           depth_cm = as.numeric(depth_cm))
  
  
  return(data.list)
  
}

check_structural_data <- function(data, fk) {
  #' @description check the structural data for possible mistakes
  #' @param data list containing the structural data as produced by read_structural_data()
  #' @param fk list containing the _fk encoding as stored in the database
  
  error.list <- list()
  
  # plot
  
  # error.list$P_date <- plot.check %>% filter(date <= date_old)
  error.list$P_census <- data$plot %>% filter(!census %in% fk$plot_census_fk)
  error.list$P_country <- data$plot %>% filter(!country %in% fk$country_fk)
  # error.list$P_country_change <- plot.check %>% rowwise() %>% filter(!country %in% country_old)
  error.list$P_location <- data$plot %>% filter(!location %in% fk$location_fk)
  # error.list$P_location_change <- plot.check %>% rowwise() %>% filter(!location %in% location_old)
  # error.list$P_stand <- plot.check %>% rowwise() %>% filter(!stand %in% stand_old)
  # error.list$P_standshort <- plot.check %>% rowwise() %>% filter(!standshort %in% B)
  # error.list$P_plot <- plot.check %>% rowwise() %>% filter(!plot_test %in% C)
  # error.list$P_subplot <- plot.check %>% rowwise() %>% filter(!subplot %in% D)
  # error.list$P_lng <- plot.check %>% rowwise() %>% filter(!lng %in% lng_old)
  # error.list$P_lat <- plot.check %>% rowwise() %>% filter(!lat %in% lat_old)
  error.list$P_plotsize <- data$plot %>% filter(!plotsize %in% c(500, 1000, 1500))
  error.list$P_dbh_min <- data$plot %>% filter(!dbh_min %in% c(50, 60, 100))
  error.list$P_plottype <- data$plot %>% filter(!plottype %in% fk$plottype_fk)
  # error.list$P_plottype_change <- plot.check %>% rowwise() %>% filter(!plottype %in% plottype_old)
  error.list$P_foresttype <- data$plot %>% filter(!foresttype %in% fk$foresttype_fk)
  # error.list$P_foresttype_change <- plot.check %>% rowwise() %>% filter(!foresttype %in% foresttype_old)
  error.list$P_aspect <- data$plot %>% filter(!is.na(aspect) & !aspect %in% c(0:360))
  
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
  # error.list$T_dbh_alive <- tree.check %>% filter(dbh_mm < dbh_old) 
  # error.list$T_dbh_dead <- tree.check %>% filter(!status %in% c(1:4) & !status_old %in% c(1:4) & dbh_mm > dbh_old)
  error.list$T_height <- data$tree %>% filter(crownht_m > height_m)
  error.list$T_decay <- data$tree %>% filter(!decay %in% fk$decay_fk)
  error.list$T_decay_alive <- data$tree %>% filter(status %in% c(1:4) & !decay %in% -1)
  error.list$T_decay_dead <- data$tree %>% filter(!status %in% c(1:4) & decay %in% -1)
  error.list$T_decay_stump <- data$tree %>% filter(status %in% c(0, 10) & !decay %in% 5)
  error.list$T_decay_wood <- data$tree %>% filter(!decay_wood %in% fk$decay_wood_fk)
  error.list$T_decay_wood_alive <- data$tree %>% filter(status %in% c(1:4) & !decay_wood %in% -1)
  error.list$T_decay_wood_dead <- data$tree %>% filter(!status %in% c(1:4) & decay_wood %in% -1)
  error.list$T_decayht <- data$tree %>% filter(!decayht %in% fk$decayheight_fk)
  error.list$T_decayht_alive <- data$tree %>% filter(status %in% c(1:4) & !decayht %in% -1)
  error.list$T_decayht_dead <- data$tree %>% filter(!status %in% c(1:4) & decayht %in% -1)
  error.list$T_decayht_stump <- data$tree %>% filter(status %in% c(0, 10) & !decayht %in% 0)
  error.list$T_decayht_decay <- data$tree %>% filter(decay %in% 5 & !decayht %in% 0)
  error.list$T_decayht_height <- data$tree %>% filter(!status %in% c(1:4) & !is.na(height_m))
  
  # microsites
  
  error.list$Mi_not_in_tree <- anti_join(data$microsites, data$tree, by = c("date", "treeid"))
  error.list$Mi_microsite <- data$microsites %>% filter(!microsite %in% fk$microsite_fk)
  error.list$Mi_count <- data$microsites %>% filter(microsite %in% c(11, 20, 23, 25, 26, 29, 32:41, 44:47) & count > 1)
  error.list$Mi_duplicates <- as.tibble(duplicated(data$microsites)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$microsites %>% rownames_to_column("id"), by = "id")
  
  # deadwood
  
  error.list$D_not_in_plot <- anti_join(data$deadwood, data$plot, by = c("date", "plotid"))
  error.list$D_transect <- data$deadwood %>% filter(!transect %in% fk$transect_fk)
  error.list$D_species <- data$deadwood %>% filter(!species %in% fk$species_fk)
  error.list$D_dbh_mm <- data$deadwood %>% filter(dbh_mm %in% 0)
  error.list$D_dbh_min <- data$deadwood %>% filter(dbh_mm < 60)
  error.list$D_decay <- data$deadwood %>% filter(!decay %in% fk$decay_wood_fk)
  error.list$D_duplicates <- as.tibble(duplicated(data$deadwood)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$deadwood %>% rownames_to_column("id"), by = "id")
  
  # regeneration
  
  error.list$R_not_in_plot <- anti_join(data$regeneration, data$plot, by = c("date", "plotid"))
  error.list$R_species <- data$regeneration %>% filter(!species %in% fk$species_fk)
  error.list$R_htclass <- data$regeneration %>% filter(!htclass %in% fk$htclass_fk)
  error.list$R_regeneratedon <- data$regeneration %>% filter(!regeneratedon %in% fk$regeneratedon_fk)
  error.list$R_count <- data$regeneration %>% filter(count %in% 0)
  error.list$R_duplicates <- as.tibble(duplicated(data$regeneration)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$regeneration %>% rownames_to_column("id"), by = "id")
  
  # soil
  
  error.list$S_not_in_plot <- anti_join(data$soil, data$plot, by = c("date", "plotid"))
  error.list$S_sample <- data$soil %>% filter(!sample %in% c(1:5))
  error.list$S_soil_horizon <- data$soil %>% filter(!soil_horizon %in% fk$soil_horizon_fk)
  error.list$S_bedrock <- data$soil %>% mutate(n = ifelse(soil_horizon %in% "R", 1, 0)) %>% 
    group_by(date, plotid, sample) %>% summarise(n = sum(n)) %>% filter(!n %in% 1)
  error.list$S_bedrock_depth <- data$soil %>% filter(soil_horizon %in% "R" & !depth_cm %in% c(-1, 0, 1))
  error.list$S_depth_cm <- data$soil %>% filter(!soil_horizon %in% "R") %>% filter(depth_cm %in% c(NA, 0) | depth_cm < 0)
  
  # vegetation
  
  error.list$V_not_in_plot <- anti_join(data$vegetation, data$plot, by = c("date", "plotid"))
  error.list$V_sampling_date <- data$vegetation %>% filter(is.na(sampling_date))
  error.list$V_large_gap <- data$vegetation %>% filter(!large_gap %in% fk$large_gap_fk)
  error.list$V_vegetationht <- data$vegetation %>% filter(!vegetationht %in% fk$vegetationheight_fk)
  error.list$V_biotope_quality <- data$vegetation %>% filter(!biotope_quality %in% fk$biotope_quality_fk)
  error.list$V_biotope_trend <- data$vegetation %>% filter(!biotope_trend %in% fk$biotope_trend_fk)
  error.list$V_vegetation_cover <- data$vegetation %>% 
    mutate(cover = vaccinium_myrtillus_per + rubus_per + bryopsida_per + polypodiopsida_per + poaceae_per + ericaceae_per + other_per) %>%
    gather(., family, value, vaccinium_myrtillus_per, rubus_per, bryopsida_per, polypodiopsida_per, poaceae_per, ericaceae_per, other_per) %>%
    group_by(plotid, vegetation_cover, cover) %>%
    summarise(value = max(value)) %>%
    filter(vegetation_cover > cover | vegetation_cover < value)
  
  # habitat
  
  error.list$H_not_in_plot <- anti_join(data$habitat, data$plot, by = c("date", "plotid"))
  error.list$H_sampling_date <- data$habitat %>% filter(is.na(sampling_date))
  error.list$H_species <- data$habitat %>% filter(!animal_species %in% fk$animal_species_fk)
  error.list$H_gender <- data$habitat %>% filter(!gender %in% fk$gender_fk)
  error.list$H_habitat_sign_type <- data$habitat %>% filter(!habitat_sign_type %in% fk$habitat_sign_type_fk)
  
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
  
  for (i in data$plot$plotid) {
    
    plot <- data$plot %>% filter(plotid %in% i)
    
    coord <- plot[, c("lng", "lat")]
    
    data$plot <- data$plot %>% mutate(altitude_m = ifelse(plotid %in% i, extract(DEM, coord, method = "bilinear"), altitude_m))
    
  }
  
  detach(package:rgdal, unload = TRUE); detach(package:raster, unload = TRUE)
  
  library(tidyverse)
  
  data$plot$census <- 1
  
  data.clean$plot <- data$plot %>% 
    mutate(altitude_m = round(altitude_m, 0), slope = round(slope, 0), aspect = round(aspect, 0), ownership = 1)
  
  # tree
  
  data.clean$tree <- data$tree %>%
    inner_join(., 
               data$plot %>% select(plotid, foresttype),
               by = "plotid") %>%
    mutate(distance_m = sqrt(abs(x_m^2) + abs(y_m^2)),
           onplot = case_when(
             foresttype %in% "thermophilic" & distance_m <= 12.62 ~ 1,
             distance_m %in% NA ~ 99,
             TRUE ~ 0),
           census = 0) %>%
    group_by(treeid) %>%
    select(-distance_m, -foresttype)
  
  # microsites
  
  data.clean$microsites <- data$microsites %>% 
    mutate(count = ifelse(microsite %in% c(11, 20, 23, 25, 26, 29, 32:41, 44:47), NA, count),
           method = 2) %>%
    distinct(., .keep_all = T)
  
  # deadwood
  
  data.clean$deadwood <- distinct(data$deadwood, .keep_all = T)
  
  # regeneration
  
  data.clean$regeneration <- distinct(data$regeneration, .keep_all = T)
  
  
  # soil
  
  data.clean$soil <- data$soil %>% distinct(., .keep_all = T) %>% 
    group_by(date, plotid, sample, soil_horizon) %>% summarise(depth_cm = sum(depth_cm))
  
  # vegetation
  
  data.clean$vegetation <- data$vegetation %>%
    mutate(gap_distance_m = NA) %>%
    mutate_at(vars(vegetation_cover, vaccinium_myrtillus_per, rubus_per, bryopsida_per,
                   polypodiopsida_per, poaceae_per, ericaceae_per, other_per), funs(round(., 0)))
  
  # habitat
  
  data.clean$habitat <- data$habitat
  
  return(data.clean)
  
}

# 0. _fk ------------------------------------------------------------------

fk <- dbGetQuery(KELuser, "SELECT tablename FROM pg_tables WHERE schemaname = 'public' AND tablename LIKE '%fk'")

fk.list <- list()

for (i in fk$tablename) {
  
  fk.list[i] <- tbl(KELuser, paste(i)) %>% collect()
  
}

# STRUCTURAL DATA ---------------------------------------------------------

setwd("C:/Users/Ondrej_Vostarek/Downloads")

# 1. reading --------------------------------------------------------------

file <- list.files(pattern = '.xlsx')

data.raw <- list()

for (i in file) {
  
  data.new <- read_structural_data(i)
  
  data.raw$plot <- bind_rows(data.raw$plot, data.new$plot)
  data.raw$tree <- bind_rows(data.raw$tree, data.new$tree)
  data.raw$microsites <- bind_rows(data.raw$microsites, data.new$microsites)
  data.raw$deadwood <- bind_rows(data.raw$deadwood, data.new$deadwood)
  data.raw$regeneration <- bind_rows(data.raw$regeneration, data.new$regeneration)
  data.raw$soil <- bind_rows(data.raw$soil, data.new$soil)
  
}

date.id <- unique(data.raw$plot$date)

fm.list <- list.files("C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2021/fieldmap", # path to the directory
                      pattern = 'FM*', recursive = F, full.names = T)

fm.plotids <- openxlsx::read.xlsx("C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2021/fieldmap/plotids2021.xlsx")

tree.pos <- tibble()

for (FM in fm.list) {
  
  fm <- substr(FM, 66, nchar(FM) - 5)
  
  fmid <- fm.plotids %>% filter(project %in% fm) %>% pull(fmid)
  
  tree.pos <- bind_rows(
    tree.pos,
    openxlsx::read.xlsx(FM, sheet = "Trees_1") %>%
      filter(IDPlots %in% fmid) %>%
      select(plot_id = IDPlots, x = `X,m`, y = `Y,m`, treeid = ID)
  )
}

data.raw$tree <- fm.plotids %>%
  filter(plotid %in% c("AUT_ALT_001_1",
                       "AUT_ALT_001_2",
                       "AUT_ALT_002_1",
                       "AUT_ALT_002_2",
                       "AUT_ALT_003_1",
                       "AUT_ALT_003_2",
                       "AUT_ALT_004_1",
                       "AUT_ALT_004_2",
                       "AUT_ALT_005_1",
                       "AUT_ALT_005_2")) %>%
  inner_join(., tree.pos, by = c("fmid" = "plot_id")) %>%
  mutate(treeid = as.character(treeid),
         treeid = case_when(
           nchar(treeid) == 1 ~ paste0("00", treeid),
           nchar(treeid) == 2 ~ paste0("0", treeid),
           nchar(treeid) == 3 ~ treeid),
         treeid = paste(plotid, treeid, sep = "_")) %>%
  select(treeid, x, y) %>%
  right_join(., data.raw$tree, by = "treeid") %>%
  mutate(x_m = x, y_m = y) %>%
  select(-x, -y)

### check the number of NAs in x_m, y_m

data.raw$tree %>% filter(is.na(x_m))

## VEGETATION

vegetation.file <- list.files(pattern = '_vegetation.csv')

vegetation.df <- tibble()

for (i in vegetation.file) {

  data.df <- read.table(i, sep = ";", header = T)
  data.df$sampling_date <- as.POSIXct(strptime(data.df$sampling_date,"%d.%m.%Y", tz = "UTC"))

  vegetation.df <- bind_rows(vegetation.df, data.df)

}

data.raw$vegetation <- vegetation.df

## HABITAT

habitat.file <- list.files(pattern = '_habitat.csv')

habitat.df <- tibble()

for (i in habitat.file) {

  data.df <- read.table(i, sep = ";", header = T)
  data.df$sampling_date <- as.POSIXct(strptime(data.df$sampling_date,"%d.%m.%Y", tz = "UTC"))

  habitat.df <- bind_rows(habitat.df, data.df)

}

data.raw$habitat <- habitat.df

# 2. cleaning -------------------------------------------------------------

## check data

### 'plottype' in PLOT needs to be checked/edited manually

data.raw$tree <- data.raw$tree %>% mutate(treeid = paste(treeid, stem, sep = "_")) %>% select(-stem)
data.raw$regeneration <- data.raw$regeneration %>% group_by(date, plotid, species, htclass, regeneratedon) %>% summarise(count = sum(count)) %>% ungroup()

error.list <- check_structural_data(data = data.raw, fk = fk.list)

## correct data

data.clean <- clean_structural_data(data = data.raw)

# 3. exporting ------------------------------------------------------------

setwd("C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2020/clean")

for (i in names(data.clean)) {
  
  name <- paste(date.id, i, sep = "_")
  
  write.table(data.clean[i], paste0(name, ".csv"), sep = ",", row.names = F, na = "")
  
}
