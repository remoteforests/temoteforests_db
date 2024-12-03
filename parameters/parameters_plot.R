# 0. setup ----------------------------------------------------------------

# R 4.2.3 (2023-03-15) "Shortstop Beagle"

library(geosphere) # 1.5-18
library(ineq) # 0.2-13
library(pool) # 1.0.3
library(reshape2) # 1.4.4
library(RPostgreSQL) # 0.7-6 (DBI 1.2.3)
library(tidyverse) # 2.0.0 (dplyr 1.1.4, forcats 1.0.0, ggplot2 3.5.1, lubridate 1.9.3, purr 1.0.2, readr 2.1.5, stringr 1.5.1, tibble 3.2.1, tidyr 1.3.1)
library(vegan) # 2.6-4 (permute 0.9-7, lattice 0.20-45)
library(TapeR) # 0.5.3 (nlme 3.1-162, splines 4.2.3, pracma 2.4.4)
library(lme4) # 1.1-35.1 (Matrix 1.5-3)
library(zoo) # 1.8-12

source("pw.R")

source("parameters/parameters_fc.R")

plot.id <- tbl(KELuser, "plot") %>% 
  filter(ownership %in% 1,
         !plottype %in% c(10, 99)) %>%
  pull(id)

params <- c(
  "plot",
  "tree",
  "core",
  "disturbance",
  "deadwood",
  "deadwood_tree",
  "regeneration",
  "regeneration_subplot",
  "canopy",
  "mortality",
  "temperature"
  )

# 1. PLOT-LEVEL PARAMETERS ------------------------------------------------

# 1. 1. data --------------------------------------------------------------

data.list <- paramsGetData(plot.id, params)

# 1. 2. calculation -------------------------------------------------------

data.params <- paramsCalculate(data = data.list, params)

# 1. 3. collection --------------------------------------------------------

data.col <- paramsCollect(data.params)

# ! close database connection ---------------------------------------------

poolClose(KELadmin);poolClose(KELuser)
