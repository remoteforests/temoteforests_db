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

# 2. check ----------------------------------------------------------------

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

pdf("treePosCheck.pdf", width = 9.2, height = 8, pointsize = 12, onefile = T)

for(PL in unique(data.map$plotid)){
  
  print(plotTree(PL))
  
}

dev.off()

# 3. clean ----------------------------------------------------------------

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

# 4. export ---------------------------------------------------------------


# disconnect --------------------------------------------------------------

poolClose(KELadmin);poolClose(KELuser)
