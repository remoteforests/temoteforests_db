# setup -------------------------------------------------------------------

library(pool);library(tidyverse)

source("pw.R")

# 1. extract subplot positions --------------------------------------------

fm.list <- list.files("C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2022/fieldmap", # path to the directory
                      pattern = 'FM*', recursive = F, full.names = T)

fm.plotids <- openxlsx::read.xlsx("C:/Users/Ondrej_Vostarek/Desktop/MVP/DB/data/2022/fieldmap/plotids2022.xlsx")

sub.pos <- tibble()

for (FM in fm.list) {
  
  fm <- substr(FM, 66, nchar(FM) - 5)
  
  fmid <- fm.plotids %>% filter(project %in% fm) %>% pull(fmid)
  
  sub.pos <- bind_rows(
    sub.pos,
    openxlsx::read.xlsx(FM, sheet = "GPSReferencePoints") %>%
      filter(IDPLOTS %in% fmid) %>%
      select(fmid = IDPLOTS, subplot_n = ID, x_m = X_M, y_m = Y_M)
  )
}

# 2. check errors in subplot_n --------------------------------------------

## check strange subplot numbers

error <- sub.pos %>% filter(!subplot_n %in% c(1:6)) %>% distinct(fmid) %>% pull(fmid)

pdf("subplotPosCheck.pdf", width = 9.2, height = 8, pointsize = 12, onefile = T)

for (e in error) {
  
  x <- sub.pos %>% 
    filter(fmid %in% e) %>%
    inner_join(., fm.plotids, by = "fmid") %>%
    unite(., "plot", project, plotid, fmid, sep = "-")
  
  p <- ggplot(x) +
    geom_point(aes(x = x_m, y = y_m)) +
    geom_point(aes(0,0), shape = 3, color = "red",size = 3) +
    geom_text(aes(x_m+0.5, y_m+0.5, label = subplot_n), size = 3, color = "grey20") +
    ggtitle(unique(x$plot))
  
  print(p)
  
}

dev.off()


## check all plots if order of subplots is correct (remove previously corrected strange subplots - 99)

pdf("subplotPosCheck.pdf", width = 9.2, height = 8, pointsize = 12, onefile = T)

for (i in unique(sub.pos$fmid)) {
  
  x <- sub.pos %>% 
    filter(fmid %in% i & subplot_n %in% c(1:6)) %>%
    inner_join(., fm.plotids, by = "fmid") %>%
    unite(., "plot", project, plotid, fmid, sep = "-")
  
  p <- ggplot(x) +
    geom_point(aes(x = x_m, y = y_m)) +
    geom_point(aes(0,0), shape = 3, color = "red",size = 3) +
    geom_text(aes(x_m+0.5, y_m+0.5, label = subplot_n), size = 3, color = "grey20") +
    ggtitle(unique(x$plot))
  
  print(p)
  
}

dev.off()

# 3. clean data -----------------------------------------------------------

clean <- sub.pos %>%
  filter(subplot_n %in% c(1:6)) %>%
  group_by(fmid) %>%
  filter(n() >= 5) %>% # based on current data inspection during previous correction steps
  ungroup() %>%
  mutate(subplot_n = subplot_n - 1,
         date = 2022) %>% # year when data were collected
  inner_join(., fm.plotids, by = "fmid") %>%
  select(date, plotid, subplot_n, x_m, y_m) %>%
  arrange(date, plotid, subplot_n)

write.table(clean, "2022_regSubplot_position.csv", sep = ",", row.names = F, na = "") 

# 4. upload data ----------------------------------------------------------

pull_id <- function(name){
  #' @description pull the maximal id from a database table
  #' @param name name of the database table from which the id should be pulled
  
  id <- tbl(KELuser, name) %>% filter(id == max(id, na.rm = TRUE)) %>% pull(id)
  
}

colorder <- function(name){
  #'@description arrange columns of a data.frame in the same order as in the corresponding table in database 
  #'@param name name of the database table
  
  tbl(KELuser, name) %>% colnames()
  
}

id.max <- pull_id("reg_subplot_position")

plot_id <- tbl(KELuser, "plot") %>% select(date, plotid, plot_id = id) %>% collect()

data.df <- read.table("2022_regSubplot_position.csv", sep = ",", header = T, stringsAsFactors = F) %>% 
  inner_join(., plot_id, by = c("date", "plotid")) %>% 
  mutate(id = row_number() + id.max) %>% 
  select(colorder("reg_subplot_position"))

dbWriteTable(conn = KELadmin, 
             name = "reg_subplot_position",
             value = data.df,
             overwrite = F, append = T, row.names = F)
