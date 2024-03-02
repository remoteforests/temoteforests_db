# 0. setup ----------------------------------------------------------------

# R 3.6.3 (2020-02-29)

library(pool) # 0.1.4.3
library(RPostgreSQL) # 0.6-2 (DBI 1.1.0)
library(tidyverse) # 1.3.0 (dplyr 1.0.7, forcats 0.5.0, ggplot2 3.3.5, purr 0.3.4, readr 1.3.1, stringr 1.4.0, tibble 3.0.0, tidyr 1.0.2)

source("pw.R")

source("data/control/dendrochrono/dendrochrono_control_fc.R")

fk <- dbGetQuery(KELuser, "SELECT tablename FROM pg_tables WHERE schemaname = 'public' AND tablename LIKE '%fk'")

fk.list <- list()

for (i in fk$tablename) {
  
  fk.list[i] <- tbl(KELuser, paste(i)) %>% collect()
  
}

# 1. DENDROCHRONOLOGICAL DATA ---------------------------------------------

setwd("C:/Users/Ondrej_Vostarek/Downloads")

# 1. 1. reading -----------------------------------------------------------

st <- unique(gsub("^(.*)[:.:](.*)[:.:](.*)$", "\\2", list.files(pattern = '.csv')))

data.raw <- list()

for (i in st) {
  
  data.new <- read_dendro_data(i)
  
  data.raw$core <- bind_rows(data.raw$core, data.new$core)
  data.raw$ring <- bind_rows(data.raw$ring, data.new$ring)
  
}

tree.db <- tbl(KELuser, "tree") %>% 
  inner_join(.,
             tbl(KELuser, "plot") %>% 
               filter(standshort %in% st) %>% 
               select(date, plot_id = id),
             by = "plot_id") %>%
  collect()

# 1. 2. cleaning ----------------------------------------------------------

## check data

error.list <- check_dendro_data(data = data.raw, fk = fk.list)

## correct data

data.clean <- list()

data.clean$core <- data.raw$core %>% distinct(., .keep_all = T) %>% mutate(coretype = 1) # 2 - mortality cores
data.clean$ring <- data.raw$ring %>% distinct(., .keep_all = T)

# 1. 3. exporting ---------------------------------------------------------

name <- paste(unique(data.clean$core$date), substr(first(data.clean$core$treeid), 1, 3), st, sep = "_")

write.table(data.clean$core, paste(name, "core.csv", sep = "_"), sep = ",", row.names = F, na = "")
write.table(data.clean$ring, paste(name, "ring.csv", sep = "_"), sep = ",", row.names = F, na = "")

# 1. 4. change species ----------------------------------------------------

change.df <- read.table("change_species.csv", sep = ",", header = T, stringsAsFactors = F) %>% 
  select(treeid, species) %>% 
  inner_join(., 
             tree.db %>% select(id, treeid), 
             by = "treeid")

change.df %>% dbWriteTable(KELadmin, c('public','change'), value = ., row.names = FALSE, overwrite = TRUE)

# ! close database connection ---------------------------------------------

poolClose(KELadmin);poolClose(KELuser)
