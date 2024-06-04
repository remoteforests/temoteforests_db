read_core_data <- function(file){
  #' @description read dendrochronological core data (.xlsx)
  #' @param file path to the .xlsx file
  
  core <- read.xlsx(file)
    
  if(!identical(c("treeid", "species", "corestatus", "circle", "n_circle", "crossdated", "cormach", "mortality_date"), 
                names(core))) 
    
    stop("Core data do not match with required table format.")
  
  sp <- tbl(KELuser, "species_fk") %>% distinct(sp_code, id) %>% collect() %>% deframe()
  mm <- tbl(KELuser, "distance_to_pith") %>% select(-id) %>% collect()
  
  core <- core %>%
    mutate_at(vars(treeid, species, corestatus, crossdated), list(~ trimws(.))) %>%
    mutate_at(vars(treeid, species, corestatus, crossdated), list(~ toupper(.))) %>%
    mutate(date = as.numeric(sampling.date),
           treeid = as.character(paste(country, treeid, sep = "_")),
           species = as.character(sp[species]),
           corestatus = case_when(
             corestatus %in% "P" ~ 0,
             corestatus %in% "N" ~ 1,
             corestatus %in% "R" ~ 2,
             corestatus %in% "M" ~ 3),
           corestatus = as.numeric(corestatus),
           circle = as.numeric(circle),
           n_circle = as.numeric(n_circle),
           crossdated = case_when(
             crossdated %in% "A0" ~ 0,
             crossdated %in% "A1" ~ 1,
             crossdated %in% "A2" ~ 2,
             crossdated %in% "A3" ~ 3,
             crossdated %in% "A4" ~ 4,
             crossdated %in% "B0" ~ 10,
             crossdated %in% "B1" ~ 11,
             crossdated %in% "B2" ~ 12,
             crossdated %in% "B3" ~ 13,
             crossdated %in% "C0" ~ 20,
             crossdated %in% "C1" ~ 21,
             crossdated %in% "C2" ~ 22),
           crossdated = as.numeric(crossdated),
           cormach = as.numeric(cormach),
           mortality_date = as.character(mortality_date)) %>%
    left_join(., mm, by = c("circle", "n_circle")) %>%
    mutate(missing_mm = ifelse(is.na(missing_mm) & corestatus %in% 0, 0, missing_mm))

  return(core)
}
  
read_ring_data <- function(file){
  #' @description read dendrochronological ring data (.fh)
  #' @param file path to the .fh file
  
  ring <- read.fh(fh) %>%
    rownames_to_column("year") %>%
    gather(., key = "treeid", value = "incr_mm", 2:ncol(.)) %>%
    filter(!is.na(incr_mm)) %>%
    mutate(date = as.numeric(sampling.date),
           treeid = as.character(paste(country, treeid, sep = "_")),
           year = as.numeric(year),
           incr_mm = as.numeric(incr_mm)) %>%
    select(date, treeid, year, incr_mm)

  return(ring)  
}  
 
check_dendrochronological_data <- function(data, fk){
  #' @description check dendrochronological data for possible errors
  #' @param data list of dendrochronological datasets (data.raw)
  #' @param fk list of fk encodings extracted from database
  
  error.list <- list()
  
  # core
  
  error.list$C_not_in_tree <- anti_join(data$core, tree.db, by = c("date", "treeid"))
  error.list$C_not_in_ring <- anti_join(data$core, data$ring, by = c("date", "treeid"))
  error.list$C_species <- anti_join(data$core, tree.db, by = c("date", "treeid", "species"))
  error.list$C_corestatus <- data$core %>% filter(!corestatus %in% fk$corestatus_fk)
  error.list$C_crossdated <- data$core %>% filter(!crossdated %in% fk$crossdated_fk)
  error.list$C_missing_mm <- data$core %>% 
    mutate(error = case_when(
      corestatus %in% 0 & !missing_mm %in% 0 ~ 1,
      corestatus %in% 1 & missing_mm %in% c(NA, 0) ~ 1,
      corestatus %in% c(2, 3) & !is.na(missing_mm) ~ 1)) %>% 
    filter(error %in% 1)
  error.list$C_duplicates <- as_tibble(duplicated(data$core)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$core %>% rownames_to_column("id"), by = "id")
  error.list$C_ak <- data$core %>% group_by(date, treeid) %>% filter(n() > 1)
  
  # ring
  
  error.list$R_not_in_core <- anti_join(data$ring, data$core, by = c("date", "treeid"))
  error.list$R_year <- data$ring %>% filter(is.na(year))
  error.list$R_year_date <- data$ring %>% filter(year > date)
  error.list$R_incr_mm <- data$ring %>% filter(is.na(incr_mm) | incr_mm > 10)
  error.list$R_duplicates <- as_tibble(duplicated(data$ring)) %>% rownames_to_column("id") %>% filter(value %in% T) %>% 
    inner_join(., data$ring %>% rownames_to_column("id"), by = "id")
  error.list$R_ak <- data$ring %>% group_by(date, treeid, year) %>% filter(n() > 1)
  
  return(error.list)
}

clean_dendrochronological_data <- function(data, coretype){
  #' @description clean and prepare dendrochronological data
  #' @param data list of dendrochronological datasets (data.raw)
  #' @param coretype type of cores: 1 = "regular", 2 = "mortality"
  
  if(!coretype %in% c(1, 2)) 
    
    stop("Parameter coretype must have value of 1 (regular) or 2 (mortality).")
  
  data.clean <- list()
  
  # core  
  
  data.clean$core <- data$ring %>%
    group_by(date, treeid) %>%
    slice_min(., order_by = year, n = 5) %>%
    summarise(incr_mean = mean(incr_mm)) %>%
    ungroup() %>%
    right_join(., data$core, by = c("date", "treeid")) %>%
    mutate(subcore = "a",
           coreht_m = 1,
           missing_years = round(missing_mm / incr_mean, 0),
           coretype = coretype) %>%
    select(date, treeid, subcore, coreht_m, missing_mm, missing_years, 
           corestatus, crossdated, coretype, cormach, mortality_date)
  
  # ring
  
  data.clean$ring <- data$ring
  
  return(data.clean)
}