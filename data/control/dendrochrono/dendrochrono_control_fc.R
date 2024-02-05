read_dendro_data <- function(st){
  #' @description read the dendrochronological data from .csv
  #' @param st name of the stand
  
  data.list <- list()
  
  # core
  
  data.list$core <- read.table(paste("core", st, "csv", sep = "."), sep = ",", header = T, stringsAsFactors = F)
  
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
  
  data.list$ring <- read.table(paste("ring", st, "csv", sep = "."), sep = ",", header = T, stringsAsFactors = F)
  
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
  error.list$R_year_min <- data$ring %>% filter(year %in% min(year))
  error.list$R_year_date <- data$ring %>% filter(year > date)
  error.list$R_incr_mm <- data$ring %>% filter(is.na(incr_mm))
  
  return(error.list)
  
}