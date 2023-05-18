# setup -------------------------------------------------------------------

library(pool);library(tidyverse)

source("pw.R")
source("new_fc.R")

fk <- dbGetQuery(KELuser, "SELECT tablename FROM pg_tables WHERE schemaname = 'public' AND tablename LIKE '%fk'")

fk.list <- list()

for (i in fk$tablename) {
  
  fk.list[i] <- tbl(KELuser, paste(i)) %>% collect()
  
}

# STRUCTURAL DATA ---------------------------------------------------------

# 1. read -----------------------------------------------------------------

data.raw <- list()

# 1. 1. fieldmap ----------------------------------------------------------

## first manually convert 'Date' column in 'Plots' sheet to 'Date (short)' format;
## second check 'Measured' and 'Note' columns in 'Trees' sheet for inconsistencies

fm <- list.files(path = "C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2022/fieldmap/new", pattern = ".xlsx", full.names = T)

for (i in fm) {
  
  data <- read_fm_data(i)
  
  data.raw$plot <- bind_rows(data.raw$plot, data$plot)
  data.raw$tree <- bind_rows(data.raw$tree, data$tree)
  data.raw$mortality <- bind_rows(data.raw$mortality, data$mortality)
  
  remove(data)
  
}

# 1. 2. forms -------------------------------------------------------------

fr <- list.files(path = "C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2022/raw2", pattern = ".xlsx", full.names = T)

for (i in fr) {
  
  data <- read_fr_data(i)
  
  data.raw$microsites <- bind_rows(data.raw$microsites, data$microsites)
  data.raw$deadwood <- bind_rows(data.raw$deadwood, data$deadwood)
  data.raw$regeneration <- bind_rows(data.raw$regeneration, data$regeneration)
  data.raw$regeneration_subplot <- bind_rows(data.raw$regeneration_subplot, data$regeneration_subplot)
  data.raw$soil <- bind_rows(data.raw$soil, data$soil)
  data.raw$vegetation <- bind_rows(data.raw$vegetation, data$vegetation)
  data.raw$habitat <- bind_rows(data.raw$habitat, data$habitat)
  
  remove(data)
  
}

# date.id <- unique(data.raw$plot$date)
# 
# plot.id <- unique(data.raw$plot$plotid)
# 
# plot.db <- tbl(KELuser, "plot") %>% 
#   filter(plotid %in% plot.id & !date %in% date.id) %>%
#   group_by(plotid) %>% 
#   arrange(desc(date)) %>%
#   filter(row_number() == 1) %>%
#   select(plot_id = id, plotid, lng_old = lng, lat_old = lat, plotsize_old = plotsize, dbh_min_old = dbh_min) %>%
#   inner_join(., tbl(KELuser, "tree") %>% filter(!onplot %in% 0), by = "plot_id") %>%
#   mutate(n_pos = ifelse(is.na(x_m), 0, 1)) %>%
#   collect() %>%
#   group_by(plotid, lng_old, lat_old, plotsize_old, dbh_min_old) %>%
#   summarise(n_pos = sum(n_pos),
#             n_trees = n(),
#             coef_old = (n_pos/n_trees) * 100) %>%
#   ungroup()
# 
# tree.db <- tbl(KELuser, "tree") %>% 
#   inner_join(., 
#              tbl(KELuser, "plot") %>% 
#                filter(plotid %in% plot.id & !date %in% date.id) %>%
#                group_by(plotid) %>%
#                arrange(desc(date)) %>%
#                filter(row_number() == 1), 
#              by = c("plot_id" = "id")) %>% 
#   collect()

# 2. clean ----------------------------------------------------------------

## check all data for switched tree positions!

treetype = 0,


# 3. export ---------------------------------------------------------------


# disconnect --------------------------------------------------------------

poolClose(KELadmin);poolClose(KELuser)
