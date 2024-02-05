# 0. setup ----------------------------------------------------------------

library(tidyverse);library(pool);library(ineq);library(geosphere);library(TapeR);library(zoo);library(vegan);library(lme4) 

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
