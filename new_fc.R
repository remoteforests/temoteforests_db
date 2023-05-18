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