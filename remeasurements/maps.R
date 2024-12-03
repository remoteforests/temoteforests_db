# 0. setup ----------------------------------------------------------------

# R 4.2.3 (2023-03-15) "Shortstop Beagle"

library(pool) # 1.0.3
library(RPostgreSQL) # 0.7-6 (DBI 1.2.3)
library(tidyverse) # 2.0.0 (dplyr 1.1.4, forcats 1.0.0, ggplot2 3.5.1, lubridate 1.9.3, purr 1.0.2, readr 2.1.5, stringr 1.5.1, tibble 3.2.1, tidyr 1.3.1)

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

# 1. MAP ------------------------------------------------------------------

# 1. 1. functions ---------------------------------------------------------

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
    ggtitle(PL) +
    geom_point(data = data.gg %>% filter(Cored %in% "yes"),
               aes(x_m, y_m, fill = Cored), shape = 21, size = 1, color = "black") 
}

# 1. 2. data --------------------------------------------------------------

data.map <- tbl(KELuser, "core") %>%
  select(tree_id, Cored = subcore) %>%
  inner_join(., tbl(KELuser, "tree"), by = c("tree_id" = "id")) %>%
  select(treeid, Cored) %>%
  right_join(., tbl(KELuser, "tree") %>% 
               filter(plot_id %in% plot.id,
                      !is.na(x_m), !is.na(y_m),
                      !treetype %in% c("m", "x", "t", "g", "r")), 
             by = "treeid") %>%
  inner_join(., tbl(KELuser, "plot"), by = c("plot_id" = "id")) %>%
  select(plotid, treen, x_m, y_m, species, status, dbh_mm, Cored) %>%
  arrange(plotid, treen) %>%
  collect() %>%
  mutate(status = ifelse(status %in% c(1:4), "alive", status),
         status = ifelse(status %in% c(0, 10:30), "dead", status),
         species = ifelse(
           species %in% c("Acer",
                          "Acer campestre",
                          "Acer heldreichii",
                          "Acer obtusatum",
                          "Acer platanoides",
                          "Acer pseudoplatanus"),
           "Acer", species),
         species = ifelse(
           species %in% c("Sorbus",
                          "Sorbus aria",
                          "Sorbus aucuparia",
                          "Sorbus torminalis"),
           "Sorbus", species),
         species = ifelse(
           species %in% c("Fraxinus",
                          "Fraxinus excelsior",
                          "Fraxinus ornus"),
           "Fraxinus", species),
         species = ifelse(
           species %in% c("Quercus cerris",
                          "Quercus petraea"),
           "Quercus", species),
         species = ifelse(
           species %in% c("Pinus cembra",
                          "Pinus sylvestris"),
           "Pinus", species),
         species = ifelse(
           !species %in% c("Picea abies",
                           "Fagus sylvatica",
                           "Abies alba",
                           "Acer",
                           "Sorbus",
                           "Fraxinus",
                           "Quercus",
                           "Ostrya carpinifolia", 
                           "Pinus"),
           "Others", species),
          status = as.factor(status),
          species = as.factor(species),
          Cored = ifelse(is.na(Cored), "no", "yes"))

# 1. 3. export ------------------------------------------------------------

pdf(paste(year, area, "maps.pdf", sep = "_"), width = 9.2, height = 8, pointsize = 12, onefile = T)

for(PL in unique(data.map$plotid)){
  
  print(plotTree(PL))
  
}

dev.off()

# ! close database connection ---------------------------------------------

poolClose(KELadmin);poolClose(KELuser)
