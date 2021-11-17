# setup -------------------------------------------------------------------

library(pool);library(tidyverse)

source("pw.R")
source("0.functions.R")

# 0. _fk ------------------------------------------------------------------

fk <- dbGetQuery(KELuser, "SELECT tablename FROM pg_tables WHERE schemaname = 'public' AND tablename LIKE '%fk'")

fk.list <- list()

for (i in fk$tablename) {
  
  fk.list[i] <- tbl(KELuser, paste(i)) %>% collect()
  
}

# STRUCTURAL DATA ---------------------------------------------------------

setwd("C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2021/raw")

# 1. reading --------------------------------------------------------------

file <- list.files(pattern = '.xlsx')

data.raw <- list()

for (i in file) {
  
     data.new <- read_structural_data(i)
  
     data.raw$plot <- bind_rows(data.raw$plot, data.new$plot)
     data.raw$tree <- bind_rows(data.raw$tree, data.new$tree)
     data.raw$mortality <- bind_rows(data.raw$mortality, data.new$mortality)
     data.raw$microsites <- bind_rows(data.raw$microsites, data.new$microsites)
     data.raw$deadwood <- bind_rows(data.raw$deadwood, data.new$deadwood)
     data.raw$regeneration <- bind_rows(data.raw$regeneration, data.new$regeneration)
     data.raw$regeneration_subplot <- bind_rows(data.raw$regeneration_subplot, data.new$regeneration_subplot)
     data.raw$soil <- bind_rows(data.raw$soil, data.new$soil)

}

date.id <- unique(data.raw$plot$date)

plot.id <- unique(data.raw$plot$plotid)

plot.db <- tbl(KELuser, "plot") %>% 
  filter(plotid %in% plot.id & !date %in% date.id) %>%
  group_by(plotid) %>% 
  arrange(desc(date)) %>%
  filter(row_number() == 1) %>%
  select(plot_id = id, plotid, lng_old = lng, lat_old = lat, plotsize_old = plotsize, dbh_min_old = dbh_min) %>%
  inner_join(., tbl(KELuser, "tree") %>% filter(!onplot %in% 0), by = "plot_id") %>%
  mutate(n_pos = ifelse(is.na(x_m), 0, 1)) %>%
  collect() %>%
  group_by(plotid, lng_old, lat_old, plotsize_old, dbh_min_old) %>%
  summarise(n_pos = sum(n_pos),
            n_trees = n(),
            coef_old = (n_pos/n_trees) * 100) %>%
  ungroup()

tree.db <- tbl(KELuser, "tree") %>% 
  inner_join(., 
             tbl(KELuser, "plot") %>% 
               filter(plotid %in% plot.id & !date %in% date.id) %>%
               group_by(plotid) %>%
               arrange(desc(date)) %>%
               filter(row_number() == 1), 
             by = c("plot_id" = "id")) %>% 
  collect()

# shp <- list.files("C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2020/positions/trees", # path to the directory
#                   pattern = 'TreesRem2019_point.dbf', recursive = T, full.names = T)
# 
# tree.pos <- tibble()
# 
# for(i in shp){
#   tree.pos <- bind_rows(
#     tree.pos,
#     sf::st_read(i, quiet = T) %>%
#       do({ x <- .
#       bind_cols(
#         select(as.tibble(x), plot_id = IDPLOTS, treeid = ID), # IDPLOTS - SLO / IDPlots
#         sf::st_coordinates(x) %>% as.tibble() %>% set_names(c("x", "y"))
#       ) %>%
#         mutate(filename = i)
#       })
#   )
# }

fm.list <- list.files("C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2021/fieldmap", # path to the directory
                      pattern = 'FM*', recursive = F, full.names = T)

fm.plotids <- openxlsx::read.xlsx("C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2021/fieldmap/plotids2021.xlsx")

tree.pos <- tibble()

for (FM in fm.list) {
  
  fm <- substr(FM, 66, nchar(FM) - 5)
  
  fmid <- fm.plotids %>% filter(project %in% fm) %>% pull(fmid)
  
  tree.pos <- bind_rows(
    tree.pos,
    openxlsx::read.xlsx(FM, sheet = "Trees_1") %>%
      filter(IDPlots %in% fmid) %>%
      select(plot_id = IDPlots, x = `X,m`, y = `Y,m`, treeid = ID)
  )
}

data.raw$tree <- tbl(KELuser, "plot") %>% 
  select(plot_id = id, plotid) %>% 
  collect() %>%
  right_join(., tree.pos, by = "plot_id") %>% 
  mutate(treeid = as.character(treeid),
         treeid = case_when(
           nchar(treeid) == 1 ~ paste0("00", treeid),
           nchar(treeid) == 2 ~ paste0("0", treeid),
           nchar(treeid) == 3 ~ treeid),
         treeid = paste(plotid, treeid, sep = "_")) %>%
  select(treeid, x, y) %>%
  right_join(., data.raw$tree, by = "treeid") %>%
  mutate(x_m = x, y_m = y) %>%
  select(-x, -y)

### check the number of NAs in x_m, y_m

data.raw$tree %>% filter(is.na(x_m))

## VEGETATION

vegetation.file <- list.files(pattern = '_vegetation.csv')

vegetation.df <- tibble()

for (i in vegetation.file) {
  
  data.df <- read.table(i, sep = ";", header = T) 
  data.df$sampling_date <- as.POSIXct(strptime(data.df$sampling_date,"%d.%m.%Y", tz = "UTC"))
  
  vegetation.df <- bind_rows(vegetation.df, data.df)
  
}

data.raw$vegetation <- vegetation.df

## HABITAT

habitat.file <- list.files(pattern = '_habitat.csv')

habitat.df <- tibble()

for (i in habitat.file) {
  
  data.df <- read.table(i, sep = ";", header = T) 
  data.df$sampling_date <- as.POSIXct(strptime(data.df$sampling_date,"%d.%m.%Y", tz = "UTC"))
  
  habitat.df <- bind_rows(habitat.df, data.df)
  
}

data.raw$habitat <- habitat.df

# 2. cleaning -------------------------------------------------------------

## check data

### 'plottype' in PLOT needs to be checked/edited manually

error.list <- check_structural_data(data = data.raw, fk = fk.list)

## additional tree position check

p.check <- data.raw$tree %>%
  select(plotid, treeid, x_m, y_m) %>%
  left_join(., tree.db %>% select(plotid, treeid, x_m, y_m), by = c("plotid", "treeid")) %>%
  mutate(x_m_diff = ifelse(!x_m.x %in% NA & !x_m.y %in% NA, abs(x_m.x - x_m.y), 0),
         y_m_diff = ifelse(!y_m.x %in% NA & !y_m.y %in% NA, abs(y_m.x - y_m.y), 0),
         diff_m = sqrt(x_m_diff^2 + y_m_diff^2)) %>%
  group_by(plotid) %>%
  summarise(total = n(),
            shift = length(treeid[diff_m > 0.75]),
            sus = shift / total * 100) %>%
  filter(sus > 33) %>%
  pull(plotid)

data.map <- tree.db %>% 
  filter(plotid %in% p.check,
         !is.na(x_m),
         !treetype %in% c("m", "x", "t", "g")) %>%
  select(date, plotid, treen, x_m, y_m, species, status, dbh_mm) %>%
  bind_rows(., 
            data.raw$tree %>% 
              filter(plotid %in% p.check,
                     !is.na(x_m),
                     !treetype %in% c("m", "x", "t", "g")) %>%
              select(date, plotid, treen, x_m, y_m, species, status, dbh_mm)
            ) %>%
  mutate( status = ifelse(status %in% c(1:4), 'alive', status),
          status = ifelse(status %in% c(0, 10, 11:30), 'dead', status),
          species = ifelse(!species %in% c("Abies alba",
                                           "Picea abies",
                                           "Fagus sylvatica",
                                           "Acer pseudoplatanus",
                                           "Acer",
                                           "Betula pendula",
                                           "Fraxinus excelsior",
                                           "Salix caprea",
                                           "Ulmus glabra",
                                           "Corylus avellana"), 'Others', species),
          status = as.factor(status),
          species = as.factor(species)) %>%
  arrange(plotid, date) %>%
  mutate(plotid = paste(date, plotid, sep = "_"))

pdf("treePosCheck.pdf", width = 9.2, height = 8, pointsize = 12, onefile = T)

for( PL in unique(data.map$plotid)){
  
  print(plotTree(PL))
  
}

dev.off()
  
## correct data

### double check plot census in case of any a priori exceptions
### double check tree census! (case_when() malfunctioning)

data.clean <- clean_structural_data(data = data.raw)

# 3. exporting ------------------------------------------------------------

setwd("C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2021/clean")

for (i in names(data.clean)) {

  name <- paste(date.id, i, sep = "_")
  
  write.table(data.clean[i], paste0(name, ".csv"), sep = ",", row.names = F, na = "")
    
}

# DENDROCHRONOLOGICAL DATA ------------------------------------------------

setwd("C:/Users/Ondrej_Vostarek/Downloads")

# 1. reading --------------------------------------------------------------

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

# 2. cleaning -------------------------------------------------------------

## check data

error.list <- check_dendro_data(data = data.raw, fk = fk.list)

## correct data

data.clean <- list()

data.clean$core <- data.raw$core %>% distinct(., .keep_all = T) %>% mutate(coretype = 1) # 2 - mortality cores
data.clean$ring <- data.raw$ring %>% distinct(., .keep_all = T)

# 3. exporting ------------------------------------------------------------

name <- paste(unique(data.clean$core$date), substr(first(data.clean$core$treeid), 1, 3), st, sep = "_")

write.table(data.clean$core, paste(name, "core.csv", sep = "_"), sep = ",", row.names = F, na = "")
write.table(data.clean$ring, paste(name, "ring.csv", sep = "_"), sep = ",", row.names = F, na = "")

# 4. change species -------------------------------------------------------

change.df <- read.table("change_species.csv", sep = ",", header = T, stringsAsFactors = F) %>% 
  select(treeid, species) %>% 
  inner_join(., 
             tree.db %>% select(id, treeid), 
             by = "treeid")

change.df %>% dbWriteTable(KELadmin, c('public','change'), value = ., row.names = FALSE, overwrite = TRUE)

# disconnection -----------------------------------------------------------

poolClose(KELadmin)
poolClose(KELuser)
