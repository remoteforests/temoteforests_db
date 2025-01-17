read_fm_data <- function(file){
  #' @description read structural data exported from fieldmap (.xlsx)
  #' @param file path to the .xlsx file
  
  data.list <- list()
  
  # plot
  
  data.list$plot <- read.xlsx(file, sheet = "Plots", detectDates = T) %>%
    filter(!is.na(Date)) %>%
    select(pid = ID, Date, Plotid, plotsize = Area_m2, Slope, Aspect, Landform, Hillform) %>%
    mutate(pid = as.numeric(pid),
           Date = as.numeric(substr(as.character(Date), 1, 4)),
           Plotid = as.character(Plotid),
           plotsize = as.numeric(plotsize),
           Slope = as.numeric(Slope),
           Aspect = as.numeric(Aspect),
           Landform = as.numeric(Landform),
           Hillform = as.numeric(Hillform))
             
  names(data.list$plot) <- tolower(names(data.list$plot))
  
  # tms

  data.list$tms <- read.xlsx(file, sheet = "Plots", detectDates = T) %>%
    select(pid = ID, Plotid, Date) %>%
    right_join(., read.xlsx(file, sheet = "TMS"), by = c("pid" = "IDPlots")) %>%
    select(pid, Plotid, Date, Soil_Old_ID, Soil_New_ID, Distance_m, 
           Azimuth, Air_Old_ID, Air_New_ID, TreeNumber, Note) %>%
    mutate(pid = as.numeric(pid),
           Plotid = as.character(Plotid),
           Date = as.character(Date),
           Soil_Old_ID = as.numeric(Soil_Old_ID),
           Soil_New_ID = as.numeric(Soil_New_ID),
           Distance_m = as.numeric(Distance_m),
           Azimuth = as.numeric(Azimuth),
           Air_Old_ID = as.numeric(Air_Old_ID),
           Air_New_ID = as.numeric(Air_New_ID),
           TreeNumber = as.numeric(TreeNumber),
           Note = as.character(Note),
           FM = substr(file, nchar(file) - 21, nchar(file) - 5))
  
  # tree
  
  pid <- data.list$plot %>% pull(pid)
    
  sp <- read.xlsx(file, sheet = "lookup_species") %>% select(Species = ID, species = VALUE1)
  
  data.list$tree <- read.xlsx(file, sheet = "Trees") %>%
    filter(IDPlots %in% pid & !is.na(Measured)) %>%
    left_join(., sp, by = "Species") %>%
    select(pid = IDPlots, tid = ID, x_m, y_m, Status, Growth, Layer, species, 
           DBH_mm, Height_m, crownht_m = CrownHeight_m, CrownDiam1_m, CrownDiam2_m,
           decayht = DecayHeight, decay_wood = DecayWood, Decay, Forked) %>%
    mutate(pid = as.numeric(pid),
           tid = as.numeric(tid),
           x_m = as.numeric(x_m),
           y_m = as.numeric(y_m),
           Status = as.numeric(Status),
           Growth = as.numeric(Growth),
           Layer = as.numeric(Layer),
           species = as.character(species),
           DBH_mm = as.numeric(DBH_mm),
           Height_m = as.numeric(Height_m),
           crownht_m = as.numeric(crownht_m),
           CrownDiam1_m = as.numeric(CrownDiam1_m),
           CrownDiam2_m = as.numeric(CrownDiam2_m),
           decayht = as.numeric(decayht),
           decay_wood = as.numeric(decay_wood),
           Decay = as.numeric(Decay),
           Forked = as.numeric(Forked))
  
  names(data.list$tree) <- tolower(names(data.list$tree))
   
  # mortality
  
  data.list$mortality <- read.xlsx(file, sheet = "Mortality") %>%
    select(pid = IDPlots, tid = IDTrees, mort_agent = Agent) %>%
    mutate(pid = as.numeric(pid), 
           tid = as.numeric(tid), 
           mort_agent = as.numeric(mort_agent))
  
  # microsites
  
  data.list$microsites <- read.xlsx(file, sheet = "Microsites") %>%
    select(pid = IDPlots, tid = IDTrees, Microsite, Count) %>%
    mutate(pid = as.numeric(pid),
           tid = as.numeric(tid),
           Microsite = as.numeric(Microsite),
           Count = as.numeric(Count))

  names(data.list$microsites) <- tolower(names(data.list$microsites))
  
  # regref
  
  data.list$regref <- read.xlsx(file, sheet = "RegRefPoints") %>%
    filter(IDPlots %in% pid) %>% 
    select(pid = IDPlots, subplot_n = ID, x_m, y_m)
  
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
  
  data.list$microsites <- data.list$microsites %>% left_join(., data.list$tree %>% select(pid, tid, date, treeid), by = c("pid", "tid"))
  
  data.list$regref <- data.list$regref %>% inner_join(., data.list$plot %>% select(pid, date, plotid), by = "pid")
  
  return(data.list)
}

read_fr_data <- function(file){
  #' @description read structural data from rewritten forms (.xlsx)
  #' @param file path to the .xlsx file
  
  data.list <- list()
  
  # deadwood
  
  data.list$deadwood <- read.xlsx(file, sheet = "deadwood")
  
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
  
  # data.list$regeneration <- read.xlsx(file, sheet = "regeneration")
  # 
  # if(!identical(c("date", "plotid", "species", "htclass", "regeneratedon", "count"),
  #               names(data.list$regeneration)))
  #   
  #   stop("Regeneration data do not match with required table format.")
  # 
  # data.list$regeneration <- data.list$regeneration %>%
  #   mutate(date = as.numeric(date),
  #          plotid = as.character(plotid),
  #          species = as.character(species),
  #          htclass = as.numeric(htclass),
  #          regeneratedon = as.numeric(regeneratedon),
  #          count = as.numeric(count))
  
  # regeneration_subplot
  
  data.list$regeneration_subplot <- read.xlsx(file, sheet = "regeneration_subplot")
  
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
  
  # data.list$soil <- read.xlsx(file, sheet = "soil_profile")
  # 
  # if(!identical(c("date", "plotid", "sample",	"soil_horizon", "depth_cm"), 
  #               names(data.list$soil))) 
  #   
  #   stop("Soil data do not match with required table format.")
  # 
  # data.list$soil <- data.list$soil %>%
  #   mutate(date = as.numeric(date),
  #          plotid = as.character(plotid),
  #          sample = as.numeric(sample),
  #          soil_horizon = as.character(soil_horizon),
  #          depth_cm = as.numeric(depth_cm))
  
  # vegetation
  
  # data.list$vegetation <- read.xlsx(file, sheet = "vegetation", detectDates = T)
  # 
  # if(!identical(c("date", "sampling_date", "plotid", "large_gap",	"vegetationht", 
  #                 "vegetation_cover", "vaccinium_myrtillus_per", "rubus_per", 
  #                 "bryopsida_per", "polypodiopsida_per", "poaceae_per", "ericaceae_per",
  #                 "other_per", "biotope_quality", "biotope_trend", "large_herbivore_feces"), 
  #               names(data.list$vegetation))) 
  #   
  #   stop("Vegetation data do not match with required table format.")
  # 
  #  data.list$vegetation <- data.list$vegetation %>%
  #    mutate(date = as.numeric(date),
  #           sampling_date = as.POSIXlt(sampling_date, tz = "UTC", format = "%Y-%m-%d"),
  #           plotid = as.character(plotid),
  #           large_gap = as.numeric(large_gap),
  #           vegetationht = as.numeric(vegetationht),
  #           vegetation_cover = as.numeric(vegetation_cover),
  #           vaccinium_myrtillus_per = as.numeric(vaccinium_myrtillus_per),
  #           rubus_per = as.numeric(rubus_per),
  #           bryopsida_per = as.numeric(bryopsida_per),
  #           polypodiopsida_per = as.numeric(polypodiopsida_per),
  #           poaceae_per = as.numeric(poaceae_per),
  #           ericaceae_per = as.numeric(ericaceae_per),
  #           other_per = as.numeric(other_per),
  #           biotope_quality = as.numeric(biotope_quality),
  #           biotope_trend = as.numeric(biotope_trend),
  #           large_herbivore_feces = as.numeric(large_herbivore_feces))

  # habitat
   
  # data.list$habitat <- read.xlsx(file, sheet = "habitat", detectDates = T)
  # 
  # if(!identical(c("date", "sampling_date", "plotid",	"animal_species", "gender", "habitat_sign_type"), 
  #               names(data.list$habitat))) 
  #   
  #   stop("Habitat data do not match with required table format.")
  # 
  # data.list$habitat <- data.list$habitat %>%
  #   mutate(date = as.numeric(date),
  #          sampling_date = as.POSIXlt(sampling_date, tz = "UTC", format = "%Y-%m-%d"),
  #          plotid = as.character(plotid),
  #          animal_species = as.character(animal_species),
  #          gender = as.numeric(gender),
  #          habitat_sign_type = as.numeric(habitat_sign_type))
            
  return(data.list)
}

check_structural_data <- function(data, fk){
  #' @description check structural data for possible errors
  #' @param data list of structural datasets (data.raw)
  #' @param fk list of fk encodings extracted from database
  
  error.list <- list()

  plot.check <- tbl(KELuser, "plot") %>% 
    filter(id %in% old.plot.id) %>%
    select(plotid, date_old = date, plotsize_old = plotsize) %>%
    collect() %>%
    right_join(., data$plot, by = "plotid")
  
  tree.check <- tbl(KELuser, "tree") %>%
    filter(plot_id %in% old.plot.id) %>%
    select(treeid, status_old = status) %>%
    collect() %>%
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
  error.list$P_duplicates <- as_tibble(duplicated(data$plot)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$plot %>% rownames_to_column("id"), by = "id")
  error.list$P_ak <- data$plot %>% group_by(date, plotid) %>% filter(n() > 1)
  
  # tree
  
  error.list$T_status <- data$tree %>% filter(!status %in% fk$status_fk)
  error.list$T_growth <- data$tree %>% filter(!growth %in% fk$growth_fk & !is.na(growth))
  error.list$T_growth_dead <- data$tree %>% filter(!status %in% c(1:4) & !is.na(growth))
  error.list$T_layer <- data$tree %>% filter(!layer %in% fk$layer_fk & !is.na(layer))
  error.list$T_layer_dead <- data$tree %>% filter(!status %in% c(1:4) & !is.na(layer))
  error.list$T_species <- data$tree %>% filter(!species %in% fk$species_fk)
  error.list$T_height <- data$tree %>% filter(crownht_m >= height_m)
  error.list$T_decayht_height <- data$tree %>% filter(!status %in% c(1:4) & !is.na(height_m))
  error.list$T_decayht <- data$tree %>% filter(!decayht %in% fk$decayheight_fk & !is.na(decayht))
  error.list$T_decayht_alive <- data$tree %>% filter(status %in% c(1:4) & !is.na(decayht))
  error.list$T_decayht_stump <- data$tree %>% filter(status %in% c(0, 10) & !decayht %in% 0)
  error.list$T_decayht_decay <- data$tree %>% filter(decay %in% 5 & !decayht %in% 0)
  error.list$T_decay_wood <- data$tree %>% filter(!decay_wood %in% fk$decay_wood_fk & !is.na(decay_wood))
  error.list$T_decay_wood_alive <- data$tree %>% filter(status %in% c(1:4) & !is.na(decay_wood))
  error.list$T_decay <- data$tree %>% filter(!decay %in% fk$decay_fk & !is.na(decay))
  error.list$T_decay_alive <- data$tree %>% filter(status %in% c(1:4) & !is.na(decay))
  error.list$T_decay_stump <- data$tree %>% filter(status %in% c(0, 10) & !decay %in% 5)
  error.list$T_forked <- data$tree %>% filter(!forked %in% c(0, 1))
  error.list$T_duplicates <- as_tibble(duplicated(data$tree)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$tree %>% rownames_to_column("id"), by = "id")
  error.list$T_ak <- data$tree %>% group_by(date, plotid, treeid) %>% filter(n() > 1)

  # mortality
  
  error.list$Mo_not_in_tree <- anti_join(data$mortality, data$tree, by = c("date", "treeid"))
  error.list$Mo_alive <- data$tree %>% filter(status %in% c(1:4)) %>% inner_join(., data$mortality, by = c("pid", "tid", "date","treeid"))
  error.list$Mo_dead <- tree.check %>% filter(!status %in% c(1:4) & !status_old %in% c(1:4)) %>% inner_join(., data$mortality, by = c("pid", "tid", "date","treeid"))
  error.list$Mo_mort_agent <- data$mortality %>% filter(!mort_agent %in% fk$mort_agent_fk)
  error.list$Mo_0_51 <- data$mortality %>% filter(mort_agent %in% c(0, 51)) %>%
    mutate(plotid = substr(treeid, 1, nchar(treeid) - 4)) %>%
    group_by(plotid, mort_agent) %>%
    filter((mort_agent %in% 0 & n() > 2) | (mort_agent %in% 51 & n() < 3))
  error.list$Mo_duplicates <- as_tibble(duplicated(data$mortality)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$mortality %>% rownames_to_column("id"), by = "id")
  error.list$Mo_ak <- data$mortality %>% group_by(date, treeid, mort_agent) %>% filter(n() > 1)
  
  # microsites
  
  error.list$Mi_not_in_tree <- anti_join(data$microsites, data$tree, by = c("date", "treeid"))
  error.list$Mi_microsite <- data$microsites %>% filter(!microsite %in% fk$microsite_fk)
  error.list$Mi_count <- data$microsites %>% filter(count < 1)
  error.list$Mi_countable <- data$microsites %>% filter(microsite %in% c(11, 20, 23, 25, 26, 29, 32:41, 44:47) & !is.na(count))
  error.list$Mi_duplicates <- as_tibble(duplicated(data$microsites)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$microsites %>% rownames_to_column("id"), by = "id")
  error.list$Mi_ak <- data$microsites %>% group_by(date, treeid, microsite) %>% filter(n() > 1)
  
  # deadwood
  
  error.list$D_not_in_plot <- anti_join(data$deadwood, data$plot, by = c("date", "plotid"))
  error.list$D_transect <- data$deadwood %>% filter(!transect %in% fk$transect_fk)
  error.list$D_species <- data$deadwood %>% filter(!species %in% fk$species_fk)
  error.list$D_dbh_mm <- data$deadwood %>% filter(is.na(dbh_mm) | dbh_mm < 1)
  error.list$D_dbh_min <- data$deadwood %>% filter(dbh_mm < 60)
  error.list$D_decay <- data$deadwood %>% filter(!decay %in% fk$decay_wood_fk)
  error.list$D_duplicates <- as_tibble(duplicated(data$deadwood)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$deadwood %>% rownames_to_column("id"), by = "id")
  error.list$D_ak <- data$deadwood %>% group_by(date, plotid, transect, species, dbh_mm, decay) %>% filter(n() > 1)
  
  # regeneration
  
  # error.list$R_not_in_plot <- anti_join(data$regeneration, data$plot, by = c("date", "plotid"))
  # error.list$R_species <- data$regeneration %>% filter(!species %in% fk$species_fk)
  # error.list$R_htclass <- data$regeneration %>% filter(!htclass %in% fk$htclass_fk)
  # error.list$R_regeneratedon <- data$regeneration %>% filter(!regeneratedon %in% fk$regeneratedon_fk)
  # error.list$R_count <- data$regeneration %>% filter(is.na(count) | count < 1)
  # error.list$R_duplicates <- as_tibble(duplicated(data$regeneration)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
  #   inner_join(., data$regeneration %>% rownames_to_column("id"), by = "id")
  # error.list$R_ak <- data$regeneration %>% group_by(date, plotid, species, htclass, regeneratedon) %>% filter(n() > 1)
  
  # regeneration_subplot
  
  error.list$RS_not_in_plot <- anti_join(data$regeneration_subplot, data$plot, by = c("date", "plotid"))
  error.list$RS_subplot_n <- data$regeneration_subplot %>% filter(!subplot_n %in% fk$transect_fk)
  error.list$RS_subplotsize_m2 <- data$regeneration_subplot %>% filter(!subplotsize_m2 %in% 4)
  error.list$RS_species <- data$regeneration_subplot %>% filter(!species %in% fk$species_fk)
  error.list$RS_htclass <- data$regeneration_subplot %>% filter(!htclass %in% fk$htclass_fk)
  error.list$RS_browsing <- data$regeneration_subplot %>% filter(!browsing %in% fk$browsing_fk)
  error.list$RS_regeneratedon <- data$regeneration_subplot %>% filter(!regeneratedon %in% fk$regeneratedon_fk)
  error.list$RS_count <- data$regeneration_subplot %>% filter(is.na(count) | count < 1)
  error.list$RS_duplicates <- as_tibble(duplicated(data$regeneration_subplot)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$regeneration_subplot %>% rownames_to_column("id"), by = "id")
  error.list$RS_ak <- data$regeneration_subplot %>% group_by(date, plotid, subplot_n, species, htclass, regeneratedon, browsing) %>% filter(n() > 1)
  
  # soil
  
  # error.list$S_not_in_plot <- anti_join(data$soil, data$plot, by = c("date", "plotid"))
  # error.list$S_sample <- data$soil %>% filter(!sample %in% c(1:5))
  # error.list$S_soil_horizon <- data$soil %>% filter(!soil_horizon %in% fk$soil_horizon_fk)
  # error.list$S_bedrock <- data$soil %>% mutate(n = ifelse(soil_horizon %in% "R", 1, 0)) %>% 
  #   group_by(date, plotid, sample) %>% summarise(n = sum(n)) %>% filter(!n %in% 1)
  # error.list$S_bedrock_depth <- data$soil %>% filter(soil_horizon %in% "R" & !depth_cm %in% c(-1, 0, 1))
  # error.list$S_depth_cm <- data$soil %>% filter(!soil_horizon %in% "R") %>% filter(is.na(depth_cm) | depth_cm <= 0)
  # error.list$S_duplicates <- as_tibble(duplicated(data$soil)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
  #   inner_join(., data$soil %>% rownames_to_column("id"), by = "id")
  # error.list$S_ak <- data$soil %>% group_by(date, plotid, sample, soil_horizon) %>% filter(n() > 1)
  
  # vegetation
  
  # error.list$V_not_in_plot <- anti_join(data$vegetation, data$plot, by = c("date", "plotid"))
  # error.list$V_sampling_date <- data$vegetation %>% filter(is.na(sampling_date))
  # error.list$V_sampling_date_date <- data$vegetation %>% mutate(sampling = as.numeric(substr(sampling_date, 1, 4))) %>% rowwise() %>% filter(!sampling %in% date)
  # error.list$V_large_gap <- data$vegetation %>% filter(!large_gap %in% fk$large_gap_fk)
  # error.list$V_vegetationht <- data$vegetation %>% filter(!vegetationht %in% fk$vegetationheight_fk)
  # error.list$V_vegetation_cover <- data$vegetation %>% select(-sampling_date) %>%
  #   mutate(cover = vaccinium_myrtillus_per + rubus_per + bryopsida_per + polypodiopsida_per + poaceae_per + ericaceae_per + other_per) %>%
  #   gather(., family, value, vaccinium_myrtillus_per, rubus_per, bryopsida_per, polypodiopsida_per, poaceae_per, ericaceae_per, other_per) %>%
  #   group_by(plotid, vegetation_cover, cover) %>%
  #   summarise(value = max(value)) %>%
  #   filter(vegetation_cover > cover | vegetation_cover < value)
  # error.list$V_biotope_quality <- data$vegetation %>% filter(!biotope_quality %in% fk$biotope_quality_fk)
  # error.list$V_biotope_trend <- data$vegetation %>% filter(!biotope_trend %in% fk$biotope_trend_fk)
  # error.list$V_large_herbivore_feces <- data$vegetation %>% filter(large_herbivore_feces < 0)
  # error.list$V_duplicates <- as_tibble(duplicated(data$vegetation)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
  #   inner_join(., data$vegetation %>% rownames_to_column("id"), by = "id")
  # error.list$V_ak <- data$vegetation %>% group_by(date, plotid, sampling_date) %>% filter(n() > 1)
  
  # habitat
  
  # error.list$H_not_in_plot <- anti_join(data$habitat, data$plot, by = c("date", "plotid"))
  # error.list$H_sampling_date <- data$habitat %>% filter(is.na(sampling_date))
  # error.list$H_sampling_date_date <- data$habitat %>% mutate(sampling = as.numeric(substr(sampling_date, 1, 4))) %>% rowwise() %>% filter(!sampling %in% date)
  # error.list$H_animal_species <- data$habitat %>% filter(!animal_species %in% fk$animal_species_fk)
  # error.list$H_gender <- data$habitat %>% filter(!gender %in% fk$gender_fk)
  # error.list$H_habitat_sign_type <- data$habitat %>% filter(!habitat_sign_type %in% fk$habitat_sign_type_fk)
  # error.list$H_duplicates <- as_tibble(duplicated(data$habitat)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
  #   inner_join(., data$habitat %>% rownames_to_column("id"), by = "id")
  # error.list$H_ak <- data$habitat %>% group_by(date, plotid, sampling_date, animal_species, gender, habitat_sign_type) %>% filter(n() > 1)
  
  return(error.list)
}

circleFun <- function(r){
  #' @description create circle with X and Y coordinates
  #' @param r circle radius
  
  tt <- seq(0, 2 * pi, length.out = 100)
  xx <- r * cos(tt)
  yy <- r * sin(tt)
  
  return(data.frame(X = xx, Y = yy))
}

plotTree <- function(PL){
  #' @description create plot map with tree position, size, status, and species
  #' @param PL unique plotid

  data.gg <- data.map %>% filter(plotid %in% PL)
  
  ggplot(data.gg) +
    geom_point(aes(x_m, y_m, 
                   size = dbh_mm, 
                   shape = status,
                   color = species)) +
    scale_size_continuous("DBH (mm)", 
                          limits = c(0, 1400),
                          breaks = c(0, 200, 400, 600, 800, 1000, 1200, 1400),
                          range = c(2, 9)) +
    scale_shape_manual("Status", values = c("dead" = 17,
                                            "alive" = 19,
                                            "99" = 18),
                       drop = FALSE) +
    scale_color_manual("Species", values = c("Picea abies" = "lightgreen",
                                             "Fagus sylvatica" = "lightblue",
                                             "Abies alba" = "darkgreen",
                                             "Acer" = "orange",
                                             "Sorbus" =  "darkblue",
                                             "Fraxinus" = "turquoise",
                                             "Quercus" = "yellow",
                                             "Ostrya carpinifolia" = "violet", 
                                             "Pinus" = "brown",
                                             "Others" = "grey"),
                       drop = FALSE) +
    annotate("point", x = 0, y = 0, shape = 3, color = "red", size = 3) +
    geom_path(data = circleFun(r = 7.99), aes(x = X, y = Y), color = "black", linewidth = 0.3) +
    geom_path(data = circleFun(r = 12.62), aes(x = X, y = Y), color = "black", linewidth = 0.3) +
    geom_path(data = circleFun(r = 17.84), aes(x = X, y = Y), color = "black", linewidth = 0.3) +
    geom_path(data = circleFun(r = 21.85), aes(x = X, y = Y), color = "black", linewidth = 0.3) +
    theme_bw() +
    geom_text(aes(x_m + 0.5, y_m + 0.5, label = treen), size = 3, color = "grey20") +
    ggtitle(PL)
}

clean_structural_data <- function(data){
  #' @description clean and prepare structural data
  #' @param data list of structural datasets (data.raw)
  
  data.clean <- list()
  
  # plot
  
  data.clean$plot <- tbl(KELuser, "plot") %>%
    filter(id %in% old.plot.id) %>%
    select(plotid, country, location, stand, plot, subplot, lng, lat, plotsize_old = plotsize, dbh_min, plottype, foresttype, altitude_m, ownership) %>%
    collect() %>%
    right_join(., data$plot, by = "plotid") %>%
    mutate(census = case_when(
      !is.na(plotsize_old) & plotsize %in% plotsize_old  ~ 2,
      !is.na(plotsize_old) & plotsize > plotsize_old ~ 3,
      !is.na(plotsize_old) & plotsize < plotsize_old ~ 4)) %>%
    select(-pid, -plotsize_old)
  
  # mortality
  
  data.clean$mortality <- tbl(KELuser, "tree") %>%
    filter(plot_id %in% old.plot.id) %>%
    select(treeid, status_old = status) %>%
    collect() %>%
    inner_join(., data$tree %>% select(treeid, species, status_new = status), by = "treeid") %>%
    filter(status_old %in% c(1:4) & !status_new %in% c(1:4)) %>%
    left_join(., data$mortality, by = "treeid") %>%
    mutate(date = ifelse(is.na(date), unique(data$plot$date), date),
           mort_agent = ifelse(is.na(mort_agent), 99, mort_agent),
           mort_agent = case_when(
             mort_agent %in% 99 & status_new %in% c(21:23) & species %in% "Picea abies" ~ 411,
             mort_agent %in% 99 & status_new %in% 0 ~ 71,
             mort_agent %in% 99 & status_new %in% 15 ~ 21,
             .default = mort_agent)) %>%
    distinct(., date, treeid, mort_agent)
  
  # tree
  
  data.clean$tree <- tbl(KELuser, "tree") %>%
    filter(plot_id %in% old.plot.id) %>%
    select(treeid, old_treen = treen, old_x = x_m) %>%
    collect() %>%
    right_join(., data$tree, by = "treeid") %>%
    inner_join(., data.clean$plot %>% select(plotid, plotsize, foresttype), by = "plotid") %>%
    inner_join(., tbl(KELuser, "plot") %>%
                 filter(id %in% old.plot.id) %>%
                 select(plotid, plotsize_old = plotsize, dbh_min_old = dbh_min) %>%
                 collect(),
               by = "plotid") %>%
    mutate(treen = as.character(as.numeric(treen)),
           distance_m = sqrt(abs(x_m^2) + abs(y_m^2)),
           onplot = case_when(
             plotsize %in% 500 & distance_m <= 12.62 ~ 1,
             plotsize %in% 1000 & foresttype %in% c("spruce", "managed") & distance_m <= 17.84 ~ 1,
             plotsize %in% 1000 & foresttype %in% "beech" & distance_m <= 7.99 ~ 1,
             plotsize %in% 1000 & foresttype %in% "beech" & distance_m > 7.99 & distance_m <= 17.84 ~ 2,
             plotsize %in% 1500 & distance_m <= 7.99 ~ 1,
             plotsize %in% 1500 & distance_m > 7.99 & distance_m <= 17.84 ~ 2,
             plotsize %in% 1500 & distance_m > 17.84 & distance_m <= 21.85 ~ 3,
             .default = 0),
           treetype = "0",
      census = case_when(
             !treetype %in% "0" ~ 0,
             plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% 1] ~ 0,
             !is.na(old_treen) & is.na(old_x) ~ 3,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% c(3, 6)] & is.na(distance_m) ~ 99,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% c(3, 6)] & plotsize_old %in% 500 & distance_m > 12.62 ~ 3,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% c(3, 6)] & plotsize_old %in% 1000 & distance_m > 17.84 ~ 3,
             is.na(old_treen) & is.na(dbh_mm) ~ 99,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% c(3, 6)] & plotsize_old %in% 500 & distance_m <= 12.62 & dbh_mm < dbh_min_old ~ 3,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% c(3, 6)] & plotsize_old %in% 1000 & distance_m <= 17.84 & dbh_mm < dbh_min_old ~ 3,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% c(3, 6)] & plotsize_old %in% 500 & distance_m <= 12.62 & dbh_mm >= dbh_min_old & dbh_mm <= dbh_min_old + 50 ~ 1,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% c(3, 6)] & plotsize_old %in% 1000 & distance_m <= 17.84 & dbh_mm >= dbh_min_old & dbh_mm <= dbh_min_old + 50 ~ 1,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% c(3, 6)] & plotsize_old %in% 500 & distance_m <= 12.62 & dbh_mm > dbh_min_old + 50 ~ 2,
             is.na(old_treen) & plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% c(3, 6)] & plotsize_old %in% 1000 & distance_m <= 17.84 & dbh_mm > dbh_min_old + 50 ~ 2,
             is.na(old_treen) & !plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% c(3, 6)] & dbh_mm < dbh_min_old ~ 3,
             is.na(old_treen) & !plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% c(3, 6)] & dbh_mm >= dbh_min_old & dbh_mm <= dbh_min_old + 50 ~ 1,
             is.na(old_treen) & !plotid %in% data.clean$plot$plotid[data.clean$plot$census %in% c(3, 6)] & dbh_mm > dbh_min_old + 50 ~ 2,
             .default = 0),
      growth = ifelse(!status %in% c(1:4), -1, growth),
      growth = ifelse(status %in% c(1:4) & is.na(growth), 99, growth),
      layer = ifelse(!status %in% c(1:4), -1, layer),
      layer = ifelse(status %in% c(1:4) & is.na(layer), 99, layer),
      decay = ifelse(status %in% c(0, 10), 5, decay),
      decay = ifelse(status %in% c(1:4), -1, decay),
      decay = ifelse(!status %in% c(1:4) & is.na(decay), 99, decay),
      decay_wood = ifelse(status %in% c(1:4), -1, decay_wood),
      decay_wood = ifelse(!status %in% c(1:4) & is.na(decay_wood), 99, decay_wood),
      decayht = ifelse(status %in% c(0, 10) | decay %in% 5, 0, decayht),
      decayht = ifelse(status %in% c(1:4), -1, decayht),
      decayht = ifelse(!status %in% c(1:4) & is.na(decayht), 99, decayht)) %>%
    select(-old_treen, -old_x, -pid, -tid, -plotsize, -foresttype, -plotsize_old, -dbh_min_old, -distance_m)
  
  # tree_quality
  
  # tree.db <- tbl(KELuser, "tree") %>%
  #   filter(plot_id %in% old.plot.id) %>%
  #   select(treeid, onplot, treetype, x_m, y_m, status, growth, layer, species, dbh_mm, height_m, decay, decayht) %>%
  #   collect()
  #   
  # data.clean$tree_quality <- data.clean$tree %>%
  #   inner_join(., tree.db, by = "treeid") %>%
  #   mutate(x_m_diff = ifelse(!x_m.x %in% NA & !x_m.y %in% NA, abs(x_m.x - x_m.y), 0),
  #          y_m_diff = ifelse(!y_m.x %in% NA & !y_m.y %in% NA, abs(y_m.x - y_m.y), 0),
  #          diff_m = sqrt(x_m_diff^2 + y_m_diff^2),
  #          quality1 = ifelse(species.x != species.y, 1, NA),
  #          quality2 = case_when(
  #            status.x %in% c(1:4) & !status.y %in% c(1:4, 99) ~ 2,
  #            status.x %in% c(1:4) & status.y %in% c(1:4) & status.x < status.y ~ 2,
  #            status.x %in% 0 & status.y %in% 10 ~ 2,
  #            status.x %in% 10 & status.y %in% 0 ~ 2,
  #            !status.x %in% c(0, 10) & status.y %in% c(0, 10) ~ 2, 
  #            status.x %in% c(11:14) & status.y %in% c(11:14) & status.x < status.y ~ 2,
  #            status.x %in% 15 & !status.y %in% c(1, 11, 99) ~ 2,
  #            status.x %in% 11 & status.y %in% 15 ~ 2,
  #            status.x %in% 16 & status.y %in% c(4, 14) ~ 2,
  #            !status.x %in% c(0, 10, 16) & status.y %in% 16 ~ 2,
  #            status.x %in% 17 & !status.y %in% c(1, 11, 17, 99) ~ 2,
  #            status.x %in% 11 & status.y %in% 17 ~ 2,
  #            status.x %in% c(21:23) & status.y %in% c(21:23) & status.x < status.y ~ 2,
  #            status.x %in% c(21:23) & !status.y %in% c(1:4, 21:23) ~ 2),
  #          quality3 = ifelse(growth.x %in% c(0, 1, 99) & growth.y %in% -1, 3, NA),
  #          quality4 = case_when(
  #            status.x %in% c(1:4) & dbh_mm.x < dbh_mm.y ~ 4,
  #            !status.x %in% c(1:4) & !status.y %in% c(1:4) & dbh_mm.x > dbh_mm.y ~ 4),
  #          quality5 = case_when(
  #            decay.x %in% c(1:5) & decay.y %in% (1:5) & decay.x < decay.y ~ 5,
  #            decay.x %in% -1 & !decay.y %in% -1 ~ 5),
  #          quality6 = ifelse(onplot.x != onplot.y, 6, NA),
  #          quality7 = ifelse(diff_m > 0.75, 7, NA),
  #          quality8 = ifelse(layer.x %in% c(11:13, 99) & layer.y %in% -1, 8, NA),
  #          quality9 = case_when(
  #            decayht.x %in% -1 & !decayht.y %in% -1 ~ 9,
  #            decayht.x %in% c(0:5) & decayht.y %in% c(0:5) & decayht.x > decayht.y ~ 9),
  #          quality10 = case_when(
  #            status.x %in% 1 & height_m.x < height_m.y ~ 10,
  #            status.x %in% c(11, 15, 21) & height_m.x < height_m.y ~ 10),
  #          quality11 = ifelse(treetype.x != treetype.y, 11, NA)) %>%
  #   select(date, treeid, quality1:quality11) %>%
  #   gather(., key, quality, quality1:quality11) %>%
  #   filter(!quality %in% NA) %>%
  #   select(-key)
  
  # microsites
  
  data.clean$microsites <- data$microsites %>%
    filter(!is.na(microsite)) %>%
    mutate(count = ifelse(is.na(count), 1, count),
           count = ifelse(microsite %in% c(11, 20, 23, 25, 26, 29, 32:41, 44:47), NA, count),
           method = 2) %>%
    select(-pid, -tid)
  
  # deadwood
  
  data.clean$deadwood <- data$deadwood
  
  # regeneration
  
  # data.clean$regeneration <- data$regeneration
  
  # regeneration_subplot
  
  data.clean$regeneration_subplot <- data$regeneration_subplot
  
  # regref
  
  data.clean$regref <- data$regref %>% select(-pid)
  
  # soil
  
  # data.clean$soil <- data$soil
  
  # vegetation
  
  # data.clean$vegetation <- data$vegetation %>% mutate(gap_distance_m = NA)
  
  # habitat
  
  # data.clean$habitat <- data$habitat
  
  return(data.clean)
}
