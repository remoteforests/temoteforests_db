read_fm_data <- function(file){
  #' @description read structural data exported from fieldmap (.xlsx)
  #' @param file path to the .xlsx file
  
  data.list <- list()
  
  # plot
  
  data.list$plot <- openxlsx::read.xlsx(paste0(file), sheet = "Plots", detectDates = T) %>%
    filter(!EDIT_USER %in% "SYSTEM") %>%
    select(pid = ID, Date, Plotid, plotsize = Area_m2, Slope, Aspect, Landform, Hillform) %>%
    mutate(pid = as.numeric(pid),
           Date = as.numeric(substr(as.character(Date), 1, 4)),
           Date = first(Date[!is.na(Date)]),
           Plotid = as.character(Plotid),
           plotsize = as.numeric(plotsize),
           Slope = as.numeric(Slope),
           Aspect = as.numeric(Aspect),
           Landform = as.numeric(Landform),
           Hillform = as.numeric(Hillform))
             
  names(data.list$plot) <- tolower(names(data.list$plot))
  
  # tree
  
  pid <- data.list$plot %>% pull(pid)
  
  sp <- openxlsx::read.xlsx(paste0(file), sheet = "lookup_species") %>% select(Species = ID, species = VALUE1)
  
  data.list$tree <- openxlsx::read.xlsx(paste0(file), sheet = "Trees") %>%
    filter(IDPlots %in% pid & Measured %in% 1 & is.na(Note)) %>%
    left_join(., sp, by = "Species") %>%
    select(pid = IDPlots, tid = ID, x_m, y_m, Status, Growth, Layer, species, 
           DBH_mm, decayht = DecayHeight, decay_wood = DecayWood, Decay, Forked) %>%
    mutate(pid = as.numeric(pid),
           tid = as.numeric(tid),
           x_m = as.numeric(x_m),
           y_m = as.numeric(y_m),
           Status = as.numeric(Status),
           Growth = as.numeric(Growth),
           Layer = as.numeric(Layer),
           species = as.character(species),
           DBH_mm = as.numeric(DBH_mm),
           height_m = NA_real_,
           crownht_m = NA_real_,
           crowndiam1_m = NA_real_,
           crowndiam2_m = NA_real_,
           decayht = as.numeric(decayht),
           decay_wood = as.numeric(decay_wood),
           Decay = as.numeric(Decay),
           Forked = as.numeric(Forked))
  
  names(data.list$tree) <- tolower(names(data.list$tree))
   
  # mortality
  
  data.list$mortality <- openxlsx::read.xlsx(paste0(file), sheet = "Mortality") %>%
    select(pid = IDPlots, tid = IDTrees, mort_agent = Agent) %>%
    mutate(pid = as.numeric(pid), 
           tid = as.numeric(tid), 
           mort_agent = as.numeric(mort_agent))
  
  # output
  
  data.list$plot <- data.list$plot %>% inner_join(., data.list$tree %>% distinct(., pid), by = "pid")
  
  data.list$tree <- data.list$tree %>% inner_join(., data.list$plot %>% select(pid, date, plotid), by = "pid") %>%
    mutate(treen = as.character(tid),
           treen = case_when(
             nchar(treen) == 1 ~ paste0("00", treen),
             nchar(treen) == 2 ~ paste0("0", treen),
             nchar(treen) == 3 ~ treen),
           treeid = paste(plotid, treen, sep = "_"))
  
  data.list$mortality <- data.list$mortality %>% left_join(., data.list$tree %>% select(pid, tid, date, treeid), by = c("pid", "tid"))
  
  return(data.list)
}

read_fr_data <- function(file){
  #' @description read structural data from rewritten forms (.xlsx)
  #' @param file path to the .xlsx file
  
  data.list <- list()
  
  # microsites
  
  data.list$microsites <- openxlsx::read.xlsx(paste0(file), sheet = "microsites")
  
  if(!identical(c("date", "treeid", "microsite", "count"),
                names(data.list$microsites)))
    
    stop("Microsites data do not match with required table format.")
  
  data.list$microsites <- data.list$microsites %>%
    mutate(date = as.numeric(date),
           treeid = as.character(treeid),
           microsite = as.numeric(microsite),
           count = as.numeric(count))
  
  # deadwood
  
  data.list$deadwood <- openxlsx::read.xlsx(paste0(file), sheet = "deadwood")
  
  if(!identical(c("date", "plotid", "transect",	"species", "dbh_mm", "decay"),
                names(data.list$deadwood)))
    
    stop("Deadwood data do not match with required table format.")
  
  data.list$deadwood <- data.list$deadwood %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           transect = as.numeric(transect),
           species = as.character(species),
           dbh_mm = as.numeric(dbh_mm),
           decay = as.numeric(decay))
  
  # regeneration
  
  data.list$regeneration <- openxlsx::read.xlsx(paste0(file), sheet = "regeneration")
  
  if(!identical(c("date", "plotid", "species", "htclass", "regeneratedon", "count"),
                names(data.list$regeneration)))
    
    stop("Regeneration data do not match with required table format.")
  
  data.list$regeneration <- data.list$regeneration %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           species = as.character(species),
           htclass = as.numeric(htclass),
           regeneratedon = as.numeric(regeneratedon),
           count = as.numeric(count))
  
  # regeneration_subplot
  
  data.list$regeneration_subplot <- openxlsx::read.xlsx(paste0(file), sheet = "regeneration_subplot")
  
  if(!identical(c("date", "plotid", "subplot_n", "subplotsize_m2", "species", "htclass",
                  "browsing",	"regeneratedon", "count"), 
                names(data.list$regeneration_subplot))) 
    
    stop("Regeneration_subplot data do not match with required table format.")
  
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
  
  # soil
  
  data.list$soil <- openxlsx::read.xlsx(paste0(file), sheet = "soil_profile")
  
  if(!identical(c("date", "plotid", "sample",	"soil_horizon", "depth_cm"), 
                names(data.list$soil))) 
    
    stop("Soil data do not match with required table format.")
  
  data.list$soil <- data.list$soil %>%
    mutate(date = as.numeric(date),
           plotid = as.character(plotid),
           sample = as.numeric(sample),
           soil_horizon = as.character(soil_horizon),
           depth_cm = as.numeric(depth_cm))
  
  # vegetation
  
  data.list$vegetation <- openxlsx::read.xlsx(paste0(file), sheet = "vegetation", detectDates = T)
  
  if(!identical(c("date", "sampling_date", "plotid", "large_gap",	"vegetationht", 
                  "vegetation_cover", "vaccinium_myrtillus_per", "rubus_per", 
                  "bryopsida_per", "polypodiopsida_per", "poaceae_per", "ericaceae_per",
                  "other_per", "biotope_quality", "biotope_trend", "large_herbivore_feces"), 
                names(data.list$vegetation))) 
    
    stop("Vegetation data do not match with required table format.")

   data.list$vegetation <- data.list$vegetation %>%
     mutate(date = as.numeric(date),
            sampling_date = as.POSIXlt(sampling_date, tz = "UTC", format = "%Y-%m-%d"),
            plotid = as.character(plotid),
            large_gap = as.numeric(large_gap),
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
            large_herbivore_feces = as.numeric(large_herbivore_feces))

   # habitat
   
   data.list$habitat <- openxlsx::read.xlsx(paste0(file), sheet = "habitat", detectDates = T)
   
   if(!identical(c("date", "sampling_date", "plotid",	"animal_species", "gender", "habitat_sign_type"), 
                 names(data.list$habitat))) 
     
     stop("Habitat data do not match with required table format.")
   
   data.list$habitat <- data.list$habitat %>%
     mutate(date = as.numeric(date),
            sampling_date = as.POSIXlt(sampling_date, tz = "UTC", format = "%Y-%m-%d"),
            plotid = as.character(plotid),
            animal_species = as.character(animal_species),
            gender = as.numeric(gender),
            habitat_sign_type = as.numeric(habitat_sign_type))
            
  return(data.list)
}

check_structural_data <- function(data, fk) {
  #' @description check structural data for possible errors
  #' @param data list of structural datasets (data.raw)
  #' @param fk list of fk encodings extracted from database
  
  error.list <- list()
  
  plot.check <- tbl(KELuser, "plot") %>% 
    filter(plotid %in% local(unique(data$plot$plotid)) & !date %in% local(unique(data$plot$date))) %>% 
    arrange(plotid, desc(date)) %>%
    group_by(plotid) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(plotid, date_old = date, plotsize_old = plotsize) %>%
    collect() %>%
    right_join(., data$plot, by = "plotid")
  
  tree.check <- tree.db %>%
    select(treeid, status_old = status, dbh_old = dbh_mm) %>%
    right_join(., data$tree, by = "treeid")
  
  # plot
  
  error.list$P_date <- plot.check %>% filter(is.na(date) | !date > date_old)
  error.list$P_plotid <- anti_join(data$plot, tbl(KELuser, "plot") %>% distinct(., plotid) %>% collect(), by = "plotid")
  error.list$P_plotsize <- data$plot %>% filter(!plotsize %in% c(500, 1000, 1500)) 
  error.list$P_plotsize_change <- plot.check %>% rowwise() %>% filter(!plotsize %in% plotsize_old)
  error.list$P_slope <- data$plot %>% filter(!slope %in% c(0:90))
  error.list$P_aspect <- data$plot %>% filter(!aspect %in% c(0:360))
  error.list$P_landform <- data$plot %>% filter(!landform %in% c(1:5))
  error.list$P_hillform <- data$plot %>% filter(!hillform %in% c(1:3))
  
  # tree
  
  error.list$T_not_in_plot <- anti_join(data$tree, data$plot, by = c("date", "plotid"))
  error.list$T_treen <- data$tree %>% mutate(treen = as.character(treen),
                                             treen = case_when(
                                               nchar(treen) == 1 ~ paste0("00", treen),
                                               nchar(treen) == 2 ~ paste0("0", treen),
                                               nchar(treen) == 3 ~ treen,
                                               TRUE ~ treen),
                                             n = substr(treeid, nchar(treeid) - 2 , nchar(treeid))) %>%
    rowwise() %>% filter(!n %in% treen)
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
  
  # regeneration_subplot
  
  error.list$RS_not_in_plot <- anti_join(data$regeneration_subplot, data$plot, by = c("date", "plotid"))
  error.list$RS_subplot_n <- data$regeneration_subplot %>% filter(!subplot_n %in% fk$transect_fk)
  error.list$RS_subplotsize_m2 <- data$regeneration_subplot %>% filter(!subplotsize_m2 %in% 4)
  error.list$RS_species <- data$regeneration_subplot %>% filter(!species %in% fk$species_fk)
  error.list$RS_htclass <- data$regeneration_subplot %>% filter(!htclass %in% fk$htclass_fk)
  error.list$RS_browsing <- data$regeneration_subplot %>% filter(!browsing %in% fk$browsing_fk)
  error.list$RS_regeneratedon <- data$regeneration_subplot %>% filter(!regeneratedon %in% fk$regeneratedon_fk)
  error.list$RS_count <- data$regeneration_subplot %>% filter(count %in% 0)
  error.list$RS_duplicates <- as.tibble(duplicated(data$regeneration_subplot)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$regeneration_subplot %>% rownames_to_column("id"), by = "id")
  
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