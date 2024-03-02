# 0. setup ----------------------------------------------------------------

# R 3.6.3 (2020-02-29)

library(openxlsx) # 4.1.4
library(pool) # 0.1.4.3
library(RPostgreSQL) # 0.6-2 (DBI 1.1.0)
library(tidyverse) # 1.3.0 (dplyr 1.0.7, forcats 0.5.0, ggplot2 3.3.5, purr 0.3.4, readr 1.3.1, stringr 1.4.0, tibble 3.0.0, tidyr 1.0.2)

source("pw.R")

source("data/control/structure/structure_control_fc.R")

fk <- dbGetQuery(KELuser, "SELECT tablename FROM pg_tables WHERE schemaname = 'public' AND tablename LIKE '%fk'")

fk.list <- list()

for (i in fk$tablename) {
  
  fk.list[i] <- tbl(KELuser, paste(i)) %>% collect()
  
}

# 1. STRUCTURAL DATA ------------------------------------------------------

# 1. 1. read --------------------------------------------------------------

data.raw <- list()

# 1. 1. 1. fieldmap -------------------------------------------------------

## first: check all 'Note' columns for additional information;
## second: remove duplicate 'Height_m' column in 'Trees' sheet;
## third: manually convert 'Date' column in 'Plots' sheet to 'Date (short)' format and check it against 'EDIT_USER' & 'EDIT_DATE' columns -> fill in if necessary;
## fourth: check 'Measured' column in 'Trees' sheet against 'EDIT_USER' & 'EDIT_DATE' columns -> fill in if necessary;

path <- "C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2023/fieldmap/new"

fm <- list.files(path, pattern = ".xlsx", full.names = T)

for (i in fm) {
  
  data <- read_fm_data(i)
  
  data.raw$plot <- bind_rows(data.raw$plot, data$plot)
  data.raw$tms <- bind_rows(data.raw$tms, data$tms)
  data.raw$tree <- bind_rows(data.raw$tree, data$tree)
  data.raw$mortality <- bind_rows(data.raw$mortality, data$mortality)
  data.raw$microsites <- bind_rows(data.raw$microsites, data$microsites)
  data.raw$regref <- bind_rows(data.raw$regref, data$regref)
  
  remove(data)
  
}

# write.xlsx(data.raw$tms, "data/control/structure/TMS.xlsx")

# 1. 1. 2. forms ----------------------------------------------------------

path <- "C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2023/raw"

fr <- list.files(path, pattern = ".xlsx", full.names = T)

for (i in fr) {
  
  data <- read_fr_data(i)
  
  data.raw$deadwood <- bind_rows(data.raw$deadwood, data$deadwood)
  data.raw$regeneration <- bind_rows(data.raw$regeneration, data$regeneration)
  data.raw$regeneration_subplot <- bind_rows(data.raw$regeneration_subplot, data$regeneration_subplot)
  # data.raw$soil <- bind_rows(data.raw$soil, data$soil)
  # data.raw$vegetation <- bind_rows(data.raw$vegetation, data$vegetation)
  # data.raw$habitat <- bind_rows(data.raw$habitat, data$habitat)
  
  remove(data)
  
}

# 1. 2. check -------------------------------------------------------------

error.list <- check_structural_data(data = data.raw, fk = fk.list)

## additional tree position check

p.check <- tbl(KELuser, "tree") %>%
  inner_join(., tbl(KELuser, "plot") %>% 
               filter(plotid %in% local(unique(data.raw$plot$plotid)) & !date %in% local(unique(data.raw$plot$date))) %>% 
               arrange(plotid, desc(date)) %>%
               group_by(plotid) %>%
               filter(row_number() == 1) %>%
               ungroup(),
             by = c("plot_id" = "id")) %>%
  select(treeid, x_m, y_m) %>%
  collect() %>%
  inner_join(., data.raw$tree %>% select(plotid, treeid, x_m, y_m), by = "treeid") %>%
  mutate(diff_m = sqrt(abs(x_m.x - x_m.y)^2 + abs(y_m.x - y_m.y)^2)) %>%
  group_by(plotid) %>%
  summarise(total = n(),
            shift = length(treeid[diff_m > 0.5]),
            sus = shift / total * 100) %>%
  filter(sus > 33) %>%
  pull(plotid)

data.map <- tbl(KELuser, "tree") %>%
  inner_join(., tbl(KELuser, "plot") %>% 
               filter(plotid %in% p.check & !date %in% local(unique(data.raw$plot$date))) %>%
               arrange(plotid, desc(date)) %>%
               group_by(plotid) %>%
               filter(row_number() == 1) %>%
               ungroup(),
             by = c("plot_id" = "id")) %>%
  filter(!is.na(x_m), !is.na(y_m),
         !treetype %in% c("m", "x", "t", "g", "r")) %>%
  select(date, plotid, treen, x_m, y_m, species, status, dbh_mm) %>%
  collect() %>%
  bind_rows(., data.raw$tree %>% filter(plotid %in% p.check) %>%
              select(date, plotid, treen, x_m, y_m, species, status, dbh_mm)) %>%
  mutate(status = ifelse(status %in% c(1:4), "alive", status),
         status = ifelse(status %in% c(0, 10:23), "dead", status),
         species = ifelse(!species %in% c("Abies alba",
                                          "Picea abies",
                                          "Fagus sylvatica",
                                          "Acer pseudoplatanus",
                                          "Acer",
                                          "Betula pendula",
                                          "Fraxinus excelsior",
                                          "Salix caprea",
                                          "Ulmus glabra",
                                          "Corylus avellana"), "Others", species),
         status = as.factor(status),
         species = as.factor(species)) %>%
  arrange(plotid, date) %>%
  unite(., plotid, c(date, plotid), sep = "-")

pdf("data/control/structure/treePosCheck.pdf", width = 9.2, height = 8, pointsize = 12, onefile = T)

for(PL in unique(data.map$plotid)){
  
  print(plotTree(PL))
  
}

dev.off()

## regref

### check strange subplot numbers

error <- data.raw$regref %>% filter(!subplot_n %in% c(0:5)) %>% distinct(plotid) %>% pull(plotid)

pdf("data/control/structure/subplotPosCheck.pdf", width = 9.2, height = 8, pointsize = 12, onefile = T)

for (e in error) {
  
  print(
    ggplot(data.raw$regref %>% filter(plotid %in% e)) +
      geom_point(aes(x = x_m, y = y_m)) +
      geom_point(aes(0, 0), shape = 3, color = "red", size = 3) +
      geom_text(aes(x_m + 0.5, y_m + 0.5, label = subplot_n), size = 3, color = "grey20") +
      ggtitle(e)
  )
  
}

dev.off()

### check all plots if order of subplots is correct (remove previously corrected strange subplots - 99)

pdf("data/control/structure/subplotPosCheck.pdf", width = 9.2, height = 8, pointsize = 12, onefile = T)

for (PL in unique(data.raw$regref$plotid)) {
  
  print(
    ggplot(data.raw$regref %>% filter(plotid %in% PL & subplot_n %in% c(0:5))) +
      geom_point(aes(x = x_m, y = y_m)) +
      geom_point(aes(0, 0), shape = 3, color = "red", size = 3) +
      geom_text(aes(x_m + 0.5, y_m + 0.5, label = subplot_n), size = 3, color = "grey20") +
      ggtitle(PL)
  )
  
}

dev.off()

# 1. 3. clean -------------------------------------------------------------

## 'plottype' & 'dbh_min' needs to be checked/edited manually
## plot 'census' too in case of exceptions (missing trees/positions)

tbl(KELuser, "plot") %>%
  filter(plotid %in% local(unique(data.raw$plot$plotid)) & !date %in% local(unique(data.raw$plot$date))) %>% 
  arrange(plotid, desc(date)) %>%
  group_by(plotid) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  distinct(., plottype, dbh_min, census, plotsize)

ggplot(data.raw$tree) + 
  geom_histogram(aes(x = dbh_mm), binwidth = 1) + 
  scale_x_continuous(breaks = seq(0, 2000, 10)) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90))

## double check tree census! (case_when() malfunctioning)

data.clean <- clean_structural_data(data = data.raw)

# 1. 4. export ------------------------------------------------------------

path <- "C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2023/clean/"

for (i in names(data.clean)) {
  
  name <- paste(unique(data.clean$plot$date), i, sep = "_")
  
  write.table(data.clean[i], paste0(path, name, ".csv"), sep = ",", row.names = F, na = "")
  
}

# ! close database connection ---------------------------------------------

poolClose(KELadmin);poolClose(KELuser)
