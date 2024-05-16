# 0. setup ----------------------------------------------------------------

# R 4.2.3 (2023-03-15) "Shortstop Beagle"

library(openxlsx) # 4.2.5.2
library(pool) # 1.0.3
library(RPostgreSQL) # 0.7-6 (DBI 1.2.2)
library(tidyverse) # 2.0.0 (dplyr 1.1.4, forcats 1.0.0, ggplot2 3.5.0, lubridate 1.9.3, purr 1.0.2, readr 2.1.5, stringr 1.5.1, tibble 3.2.1, tidyr 1.3.1)

source("pw.R")

## JIZ 2024
plot.id <- tbl(KELuser, "plot") %>%
  filter(standshort %in% "JIZ",
         !is.na(lng), !is.na(lat)) %>%
  collect() %>%
  group_by(plotid) %>%
  arrange(desc(date), .by_group = T) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  distinct(., id) %>% 
  pull(id)

## ROM 2024
plot.id <- tbl(KELuser, "plot") %>%
  filter(location %in% c("Fagaras", "Semenic"),
         foresttype %in% "beech",
         !is.na(lng), !is.na(lat)) %>%
  collect() %>%
  group_by(plotid) %>%
  arrange(desc(date), .by_group = T) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  distinct(., id) %>% 
  pull(id)

## SLO 2024
plot.id <- tbl(KELuser, "plot") %>%
  filter(location %in% c("Great Fatra", "Little Fatra", "Low Tatras", "Polana", "Vepor Hills"),
         (foresttype %in% "beech" | location %in% "Polana") & !foresttype %in% "managed",
         !is.na(lng), !is.na(lat),
         !census %in% 8,
         !plottype %in% 2,
         !plotid %in% c("SLO_SUT_005_1", "SLO_SUT_005_2")) %>%
  collect() %>%
  group_by(plotid) %>%
  arrange(desc(date), .by_group = T) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  distinct(., id) %>% 
  pull(id)
  
# 1. DATA -----------------------------------------------------------------

# 1. 1. plot --------------------------------------------------------------

plot <- tbl(KELuser, "plot") %>%
  filter(id %in% plot.id) %>%
  mutate(radius_m = sqrt(plotsize/pi)) %>%
  select(id, plotid, slope, aspect, landform, hillform, radius_m) %>%
  arrange(plotid) %>%
  collect()

# 1. 2. tree --------------------------------------------------------------

sp.lookup <- read.xlsx("remeasurements/fieldmap/lookups.xlsx", sheet = "lookup_species")

tree <- tbl(KELuser, "tree") %>%
  filter(plot_id %in% plot.id,
         !is.na(x_m), !is.na(y_m),
         treetype %in% "0") %>%
  mutate(treen = as.numeric(treen),
         x_m = round(x_m, 3),
         y_m = round(y_m, 3),
         status = ifelse(status %in% 99, NA, status),
         growth = ifelse(growth %in% c(-1, 99), NA, growth),
         layer = ifelse(layer %in% c(-1, 99), NA, layer),
         decay = ifelse(decay %in% c(-1, 99), NA, decay),
         decay_wood = ifelse(decay_wood %in% c(-1, 99), NA, decay_wood),
         decayht = ifelse(decayht %in% c(-1, 99), NA, decayht)) %>%
  select(plot_id, treen, x_m, y_m, dbh_mm, species, status, growth, layer, decay, decay_wood, decayht) %>%
  collect() %>%
  inner_join(., sp.lookup, by = c("species" = "value")) %>%
  mutate(species = id) %>%
  select(-id) %>%
  arrange(plot_id, treen) # %>%
  # group_by(plot_id) %>%
  # complete(treen = 1:max(treen), fill = list(plot_id = first(.$plot_id))) %>%
  # ungroup()

# 1. 3. regRefPoints ------------------------------------------------------

regRefPoints <- tibble(plot_id = NA, subplot_n = NA, x_m = NA, y_m = NA)

regRefPoints <- bind_rows(
  regRefPoints,
  tbl(KELuser, "reg_subplot_position") %>% filter(plot_id %in% plot.id) %>% select(-id) %>% collect()
)

coords <- data.frame(subplot_n = c(0:5),
                     x_m = c(0.000, 0.000, 11.536, 7.130, -7.130, -11.536), 
                     y_m = c(0.000, 12.130, 3.748, -9.813, -9.813, 3.748))

x <- plot.id[!plot.id %in% regRefPoints$plot_id]

subplots <- data.frame(plot_id = c(rep(x, 6))) %>% 
  group_by(plot_id) %>%
  mutate(subplot_n = row_number() - 1) %>%
  inner_join(., coords, by = "subplot_n")

regRefPoints <- bind_rows(regRefPoints, subplots) %>% 
  filter(!is.na(plot_id)) %>% 
  mutate(x_m = round(x_m, 3), 
         y_m = round(y_m, 3))

# 1. 4. export ------------------------------------------------------------

data_import <- list("plot" = plot, "tree" = tree, "regRefPoints" = regRefPoints)         

write.xlsx(data_import, "data_import.xlsx")

# ! close database connection ---------------------------------------------

poolClose(KELadmin);poolClose(KELuser)
