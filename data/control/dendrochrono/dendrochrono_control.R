# 0. setup ----------------------------------------------------------------

# R 4.2.3 (2023-03-15) "Shortstop Beagle"

library(dplR) # 1.7.6
library(openxlsx) # 4.2.5.2
library(pool) # 1.0.3
library(RPostgreSQL) # 0.7-6 (DBI 1.2.3)
library(tidyverse) # 2.0.0 (dplyr 1.1.4, forcats 1.0.0, ggplot2 3.5.1, lubridate 1.9.3, purr 1.0.2, readr 2.1.5, stringr 1.5.1, tibble 3.2.1, tidyr 1.3.1)

source("pw.R")

source("data/control/dendrochrono/dendrochrono_control_fc.R")

fk <- dbGetQuery(KELuser, "SELECT tablename FROM pg_tables WHERE schemaname = 'public' AND tablename LIKE '%fk'")

fk.list <- list()

for (i in fk$tablename){
  
  fk.list[i] <- tbl(KELuser, paste(i)) %>% collect()
  
}

path <- "C:/Users/Ondrej_Vostarek/Desktop/KEL/db/data/dendrolab/new/raw"

sampling.date <- 2021
country <- "ROM"
coretype <- 2 # 1 = "regular", 2 = "mortality"

# 1. DENDROCHRONOLOGICAL DATA ---------------------------------------------

# 1. 1. read --------------------------------------------------------------

data.raw <- list()

# 1. 1. 1. core -----------------------------------------------------------

xl <- list.files(path, pattern = ".xlsx", full.names = T)

for (i in xl){
  
  core <- read_core_data(i)
  
  data.raw$core <- bind_rows(data.raw$core, core)
  
  remove(core)
  
}

# 1. 1. 2. ring -----------------------------------------------------------

fh <- list.files(path, pattern = ".fh", full.names = T)

for (i in fh){
  
  ring <- read_ring_data(i)
  
  data.raw$ring <- bind_rows(data.raw$ring, ring)
  
  remove(ring)
  
}

# 1. 1. 3. database -------------------------------------------------------

tree.db <- tbl(KELuser, "tree") %>%
  filter(treeid %in% local(unique(data.raw$core$treeid))) %>%
  inner_join(., tbl(KELuser, "plot") %>%
               filter(date %in% sampling.date),
             by = c("plot_id" = "id")) %>%
  select(date, treeid, species) %>%
  collect()
  
# 1. 2. check -------------------------------------------------------------

error.list <- check_dendrochronological_data(data = data.raw, fk = fk.list)

## additional chronology check

data.raw$core %>%
  distinct(., treeid) %>%
  slice_sample(., n = 9, replace = FALSE) %>%
  inner_join(., data.raw$ring, by = "treeid") %>%
  ggplot() +
  geom_line(aes(x = year, y = incr_mm)) +
  facet_wrap(~treeid) 

# 1. 3. clean -------------------------------------------------------------

data.clean <- clean_dendrochronological_data(data = data.raw, coretype)

# 1. 4. export ------------------------------------------------------------

path <- "C:/Users/Ondrej_Vostarek/Desktop/KEL/db/data/dendrolab/new/clean/"

for (i in names(data.clean)){
  
  if(coretype %in% 2){
    
    name <- paste(sampling.date, country, "mortality", i, "clean", sep = "_")
    
  } else {
    
    name <- paste(sampling.date, country, i, "clean", sep = "_")
  
    }
  
  write.table(data.clean[[i]], paste0(path, name, ".csv"), sep = ",", row.names = F, na = "")
  
}

# ! close database connection ---------------------------------------------

poolClose(KELadmin);poolClose(KELuser)
