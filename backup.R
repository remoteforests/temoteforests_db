# 0. setup ----------------------------------------------------------------

# R 4.2.3 (2023-03-15) "Shortstop Beagle"

# data.table 1.15.4
library(pool) # 1.0.3
library(tidyverse) # 2.0.0 (dplyr 1.1.4, forcats 1.0.0, ggplot2 3.5.1, lubridate 1.9.3, purrr 1.0.2, readr 2.1.5, stringr 1.5.1, tibble 3.2.1, tidyr 1.3.1)
library(RPostgreSQL) # 0.7-6 (DBI 1.2.3)

source("pw.R")

path <- "C:/Users/Ondrej_Vostarek/Desktop/KEL/db/backup/"

date <- gsub(pattern = "-", replacement = "", x = Sys.Date())

if(!dir.exists(paste0(path, date))){dir.create(paste0(path, date))}

# 1. DATABASE BACKUP ------------------------------------------------------

tables <- c(
  "plot","parameters_plot", "spatial_hierarchy",
  "tree","species_fk","wood_density","biomass_eq",
  "tree_quality","mortality","microsites","lichen", 
  "core", "distance_to_pith", "ring","pointyear", 
  "dist_data_ca", "dist_data_ai", "dist_data_gap", "dist_data_dbh", "dist_data_age",
  "dist_param", "dist_tree", "dist_plot", "dist_chrono", "dist_event", "dist_polygons", "dist_stand",
  "deadwood","deadwood_tree","deadwood_position",
  "regeneration","regeneration_subplot","reg_subplot_position",
  "canopy_analysis",
  "soil_type", "soil_profile", "soil_chemistry",
  "vegetation","habitat_signs",
  "climate","wind"
)

for(i in tables){
  
  data.table::fwrite(dbGetQuery(KELuser, paste0("SELECT * from ", i)),
                     paste0(path, date, "/", date, "_", i, "_backup.csv"))
  
  print(paste0("saved: ", i, " (", match(i, tables), "/", length(tables), ")"))
}

# ! close database connection ---------------------------------------------

poolClose(KELadmin);poolClose(KELuser)
