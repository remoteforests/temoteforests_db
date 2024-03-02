# 0. setup ----------------------------------------------------------------

# R 3.6.3 (2020-02-29)

library(geosphere) # 1.5-10
library(ineq) # 0.2-13
library(pool) # 0.1.4.3
library(reshape2) # 1.4.4
library(RPostgreSQL) # 0.6-2 (DBI 1.1.0)
library(tidyverse) # 1.3.0 (dplyr 1.0.7, forcats 0.5.0, ggplot2 3.3.5, purr 0.3.4, readr 1.3.1, stringr 1.4.0, tibble 3.0.0, tidyr 1.0.2)
library(vegan) # 2.5-6 (permute 0.9-5, lattice 0.20-38)
library(TapeR) # 0.3.3 (nlme 3.1-144, splines 3.6.3, pracma 2.2.9)
library(lme4) # 1.1-27.1 (Matrix 1.2-18)
library(zoo) # 1.8-7

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
