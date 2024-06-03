# 0. setup ----------------------------------------------------------------

# R 4.2.3 (2023-03-15) "Shortstop Beagle"

library(openxlsx) # 4.2.5.2
library(pool) # 1.0.3
library(RPostgreSQL) # 0.7-6 (DBI 1.2.2)
library(tidyverse) # 2.0.0 (dplyr 1.1.4, forcats 1.0.0, ggplot2 3.5.0, lubridate 1.9.3, purr 1.0.2, readr 2.1.5, stringr 1.5.1, tibble 3.2.1, tidyr 1.3.1)

source("pw.R")

year <- "" # insert year of remeasurement
area <- "" # insert abbreviation of remeasured area

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

# 1. FORM -----------------------------------------------------------------

# 1. 1. data --------------------------------------------------------------

data.form <- tbl(KELuser, "tree") %>%
  filter(plot_id %in% plot.id,
         !treetype %in% c("m", "x", "t", "g", "r")) %>% # distinct(., species) # %>% collect()
  inner_join(., tbl(KELuser, "plot") %>% select(plot_id = id, plotid), by = "plot_id") %>%
  mutate(treen = as.numeric(treen),
         species = case_when(
           species %in% "Fagus sylvatica" ~ "FASY",
           species %in% "Abies alba" ~ "ABAL",
           species %in% "Picea abies" ~ "PIAB",
           species %in% "Acer pseudoplatanus" ~ "ACPS",
           species %in% "Salix caprea" ~ "SACA",
           species %in% "Fraxinus excelsior" ~ "FREX",
           species %in% "Acer platanoides" ~ "FASY",
           species %in% "Ulmus glabra" ~ "ULGL",
           species %in% "Sambucus racemosa" ~ "SARA",
           species %in% "Sorbus aucuparia" ~ "SOAU",
           species %in% "Taxus baccata" ~ "TABA",
           species %in% "Betula pendula" ~ "BEPE",
           species %in% "Carpinus betulus" ~ "CABE",
           species %in% "Corylus avellana" ~ "COAV",
           species %in% "Populus tremula" ~ "POTR",
           species %in% "Sambucus nigra" ~ "SANI",
           species %in% "Laburnum anagyroides" ~ "LAAN",
           species %in% "Acer obtusatum" ~ "ACOB",
           species %in% "Salix nigra" ~ "SANI",
           species %in% "Tilia cordata" ~ "TICO",
           species %in% "Sorbus aria" ~ "SOAR",
           species %in% "Fraxinus ornus" ~ "FROR",
           species %in% "Ulmus laevis" ~ "ULLA",
           species %in% "Quercus petraea" ~ "QUPE",
           species %in% "Ostrya carpinifolia" ~ "OSCA",
           species %in% "Tilia platyphyllos" ~ "TIPL",
           species %in% "Cotinus coggygria" ~ "COCO",
           species %in% "Cornus mas" ~ "COMA",
           species %in% "Quercus cerris" ~ "QUCE",
           species %in% "Pyrus pyraster" ~ "PYPY",
           species %in% "Alnus glutinosa" ~ "ALGL",
           species %in% "99" ~ "",
           .default = species),
         mortality = "",
         microsites = "") %>%
  select(plotid, treen, status, growth, layer, species, dbh_mm, decay, decay_wood, decayht, mortality, microsites) %>%
  arrange(plotid, treen) %>%
  collect()

# 1. 2. export ------------------------------------------------------------

wb <- createWorkbook()

for(PL in unique(data.form$plotid)){
  
  header <- data.frame(treen = c("plotid", PL, "","treen"),
                       status = c("", "", "status", "/new"),
                       growth = c("", "", "growth", "/new"),
                       layer = c("date (d/m/y)", "group", "layer", "/new"),
                       species = c("", "", "", "species"),
                       dbh_mm = c("", "", "dbh_mm", "/new"),
                       decay = c("", "", "decay", "/new"),
                       decay_wood = c("slope", "aspect", "decay_wood", "/new"),
                       decayht = c("", "", "decayht","/new"),
                       mortality = c("landform", "hillform", "", "mortality"),
                       microsites = c("", "", "", "microsites"),
                       stringsAsFactors = F)
  
  addWorksheet(wb, PL)
  writeData(wb, sheet = PL, 
            data.form %>% 
              filter(plotid %in% PL) %>% 
              select(-plotid) %>% 
              arrange(treen) %>% 
              rbind(header, .))

}

saveWorkbook(wb, paste(year, area, "forms.xlsx", sep = "_"))

# ! close database connection ---------------------------------------------

poolClose(KELadmin);poolClose(KELuser)
