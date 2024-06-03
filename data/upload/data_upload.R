# 0. setup ----------------------------------------------------------------

# R 4.2.3 (2023-03-15) "Shortstop Beagle"

library(openxlsx) # 4.2.5.2
library(pool) # 1.0.3
library(RPostgreSQL) # 0.7-6 (DBI 1.2.2)
library(tidyverse) # 2.0.0 (dplyr 1.1.4, forcats 1.0.0, ggplot2 3.5.0, lubridate 1.9.3, purr 1.0.2, readr 2.1.5, stringr 1.5.1, tibble 3.2.1, tidyr 1.3.1)
library(sf) # 1.0-15 (GEOS 3.9.3, GDAL 3.5.2, PROJ 8.2.1)

source("pw.R")

source("data/upload/data_upload_fc.R")

# 1. STRUCTURAL DATA ------------------------------------------------------

path <- "C:/Users/Ondrej_Vostarek/Desktop/KEL/db/data/fieldwork/2023/clean"

# 1. 1. plot --------------------------------------------------------------

data.read <- read_data(path, "plot")

data.prepared <- prepare_data(data.read)

upload_data(data.prepared)

# 1. 2. plot_id -----------------------------------------------------------

name <- c(
  "tree",
  "deadwood",
  "regeneration",
  "regeneration_subplot",
  "reg_subplot_position",
  "soil_profile",
  "vegetation",
  "habitat_signs"
)

data.read <- read_data(path, name)

data.prepared <- prepare_data(data.read)

upload_data(data.prepared)

# 1. 3. tree_id -----------------------------------------------------------

name <- c(
  "mortality",
  "microsites",
  "tree_quality"
)

data.read <- read_data(path, name)

data.prepared <- prepare_data(data.read)

upload_data(data.prepared)

# 2. DENDROCHRONOLOGICAL DATA ---------------------------------------------

path <- "C:/Users/Ondrej_Vostarek/Desktop/KEL/db/data/dendrolab/new/clean"

# 2. 1. core --------------------------------------------------------------

data.read <- read_data(path, "core")

data.prepared <- prepare_data(data.read)

upload_data(data.prepared)

# 2. 2. ring --------------------------------------------------------------

data.read <- read_data(path, "ring")

data.prepared <- prepare_data(data.read)

upload_data(data.prepared)

# 3. DISTURBANCE DATA -----------------------------------------------------

# 3. 1. dist_tree ---------------------------------------------------------

data.read <- list(dist_tree = data.event)

data.prepared <- prepare_data(data.read)

upload_data(data.prepared)

# 3. 2. dist_plot ---------------------------------------------------------

data.read <- list(dist_plot = data.kde.all %>% distinct(., plotid, type, year_min, year_max, ncores))

data.prepared <- prepare_data(data.read)

upload_data(data.prepared)

# 3. 3. dist_chrono -------------------------------------------------------

data.read <- list(dist_chrono = data.kde.all %>% select(plotid, type, year, ca_pct, kde))

data.prepared <- prepare_data(data.read)

upload_data(data.prepared)

# 3. 4. dist_event --------------------------------------------------------

data.read <- list(dist_event = data.peaks.all %>% select(plotid, type, year))

data.prepared <- prepare_data(data.read)

upload_data(data.prepared)

# 3. 5. dist_polygons -----------------------------------------------------

path <- "C:/Users/Ondrej_Vostarek/Desktop/KEL/db/data/polygons/dist/new/stands/tp/laea"

for (i in list.files(path, pattern = ".shp$", full.names = T)){
  
  df <- st_read(dsn = i)[, c("plotid", "stand_new", "geometry")] 
  
  names(df) <- c("plotid", "stand", "geometry")
  
  dbWriteTable(conn = KELadmin, 
               name = "dist_polygons",
               value = df,
               row.names = F,
               overwrite = F, 
               append = T,
               binary = F)
  
}

# 3. 6. dist_stand --------------------------------------------------------

data.read <- list(dist_stand = dist.stand)

data.prepared <- prepare_data(data.read)

upload_data(data.prepared)

# 4. PARAMETERS -----------------------------------------------------------

data.read <- list(parameters_plot = data.col)

data.prepared <- prepare_data(data.read)

upload_data(data.prepared)

# 5. WINSCANOPY -----------------------------------------------------------

path <- ""

# 5. 1. clean -------------------------------------------------------------

## Structure of exported data needs to be checked and adjusted manually.

# for (i in list.files(path, pattern = "*_raw.txt", full.names = T)){
# 
#   df <- read_delim(i, delim = "\t", skip = 6, col_names = F)
# 
#   col.names <- read_delim(i, delim = "\t", skip = 1, n_max = 1, col_names = F) %>% select(1:ncol(df))
# 
#   colnames(df) <- col.names[1, ]
# 
#   write.xlsx(x = df, file = paste0(substr(i, 1, nchar(i) - 7), "clean.xlsx"))
#
# }

# 5. 2. upload ------------------------------------------------------------

data.read <- read_data(path, "canopy_analysis")

data.prepared <- prepare_data(data.read)

upload_data(data.prepared)

# 6. EOBS -----------------------------------------------------------------

load("C:/Users/Ondrej_Vostarek/Desktop/KEL/db/data/external/eobs/EOBS_data.RData")

data.read <- list(climate = plot_clim)

data.prepared <- prepare_data(data.read)
  
upload_data(data.prepared)  

# ! close database connection ---------------------------------------------

poolClose(KELadmin);poolClose(KELuser)
