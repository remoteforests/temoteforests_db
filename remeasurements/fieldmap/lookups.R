# 0. setup ----------------------------------------------------------------

# R 4.2.3 (2023-03-15) "Shortstop Beagle"

library(openxlsx) # 4.2.5.2
library(pool) # 1.0.3
library(RPostgreSQL) # 0.7-6 (DBI 1.2.2)
library(tidyverse) # 2.0.0 (dplyr 1.1.4, forcats 1.0.0, ggplot2 3.5.0, lubridate 1.9.3, purr 1.0.2, readr 2.1.5, stringr 1.5.1, tibble 3.2.1, tidyr 1.3.1)

source("pw.R")

# 1. LOOKUPS --------------------------------------------------------------

# 1. 1. landform ----------------------------------------------------------

landform <- data.frame(id = c(1:5),
                       value = c("1 - top", "2 - concave", "3 - mid", "4 - convex", "5 - bottom"))

# 1. 2. hillform ----------------------------------------------------------

hillform <- data.frame(id = c(1:3),
                       value = c("1 - top", "2 - mid", "3 - bottom"))

# 1. 3. species -----------------------------------------------------------

species <- tbl(KELuser, "tree") %>%
  inner_join(., tbl(KELuser, "species_fk"), by = c("species" = "id")) %>%
  distinct(., treeid, species) %>%
  group_by(species) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(id = row_number()) %>%
  select(id, value = species) %>%
  collect()

# 1. 4. status ------------------------------------------------------------

status <- data.frame(id = c(0:4, 10:17, 21:23),
                     value = c("0 - stump < 1.3 m (harvested)",
                               "1 - alive - full",
                               "2 - alive - crown breakage",
                               "3 - alive - stem breakage (> 1.3 m)",
                               "4 - alive - uprooted",
                               "10 - stump < 1.3 m (natural)",
                               "11 - dead - full",
                               "12 - dead - crown breakage",
                               "13 - dead - stem breakage (> 1.3 m)",
                               "14 - dead - uprooted while alive",
                               "15 - dead - competition",
                               "16 - dead - uprooted while dead",
                               "17 - dead - top breakage (less than 12 - crown breakage)",
                               "21 - recently dead (large-scale disturbance) - full",
                               "22 - recently dead (large-scale disturbance) - crown breakage",
                               "23 - recently dead (large-scale distrubance) - stem breakage"))

# 1. 5. growth ------------------------------------------------------------

growth <- data.frame(id = c(0:1),
                     value = c("suppressed", "released"))

# 1. 6. layer -------------------------------------------------------------

layer <- data.frame(id = c(11:13),
                    value = c("upper", "mid", "lower"))

# 1. 7. decay -------------------------------------------------------------

decay <- data.frame(id = c(1:5),
                    value = c(1:5))

# 1. 8. decayht -----------------------------------------------------------

decayht <- data.frame(id = c(0:6),
                      value = c("0.0 - 9.9 m",
                                "10.0 - 19.9 m",
                                "20.0 - 29.9 m",
                                "30.0 - 39.9 m",
                                "40.0 - 49.9 m",
                                "50.0 - 59.9 m",
                                "60.0 - 69.9 m"))

# 1. 9. mortality ---------------------------------------------------------

mort <- data.frame(id = c(0, 111:113, 121:123, 131:133, 141:143, 15:17, 21, 31, 411:413, 42, 51, 61, 71),
                   value = c("0 - no clear cause (max 2 trees per plot)",
                             "111 - crown break - wind",
                             "112 - crown break - ice/snow",
                             "113 - crown break - another falling tree",
                             "121 - stem break while alive - wind",
                             "122 - stem break while alive - ice/snow",
                             "123 - stem break while alive - another falling tree",
                             "131 - stem break while dead - wind",
                             "132 - stem break while dead - ice/snow",
                             "133 - stem break while dead - another falling tree",
                             "141 - uprooted - wind",
                             "142 - uprooted - ice/snow",
                             "143 - uprooted - another falling tree",
                             "15 - lightning",
                             "16 - landslide",
                             "17 - avalanche",
                             "21 - competition",
                             "31 - fungi infection (NOT Fomitopsis in spruce stands)",
                             "411 - Ips typographus - bark signs",
                             "412 - Ips typographus - conks of Fomitopsis",
                             "413 - Ips typographus - qualified estimation",
                             "42 - insects (other than Ips typographus)",
                             "51 - no clear cause (3 or more trees per plot)",
                             "61 - significant damage by game",
                             "71 - logging"))

mort.status <- bind_rows(
  mort %>% filter(id %in% 71) %>% mutate(status = 0) %>% select(status, id, value),
  mort %>% filter(id %in% c(121:123, 131:133, 15:17, 31, 411:413, 42, 61)) %>% mutate(status = 10) %>% select(status, id, value),
  mort %>% filter(id %in% c(0, 15:17, 31, 411:413, 42, 51, 61)) %>% mutate(status = 11) %>% select(status, id, value),
  mort %>% filter(id %in% c(111:113, 15:17, 31, 411:413, 42, 61)) %>% mutate(status = 12) %>% select(status, id, value),
  mort %>% filter(id %in% c(121:123, 131:133, 15:17, 31, 411:413, 42, 61)) %>% mutate(status = 13) %>% select(status, id, value),
  mort %>% filter(id %in% c(141:143, 15:17, 31, 411:413, 42, 61)) %>% mutate(status = 14) %>% select(status, id, value),
  mort %>% filter(id %in% c(21, 31, 411:413, 42, 61)) %>% mutate(status = 15) %>% select(status, id, value),
  mort %>% filter(id %in% c(0, 15:17, 21, 31, 411:413, 42, 51, 61)) %>% mutate(status = 16) %>% select(status, id, value),
  mort %>% filter(id %in% c(15:17, 21, 31, 411:413, 42, 61)) %>% mutate(status = 17) %>% select(status, id, value),
  mort %>% filter(id %in% c(0, 15:17, 31, 411:413, 42, 51, 61)) %>% mutate(status = 21) %>% select(status, id, value),
  mort %>% filter(id %in% c(111:113, 15:17, 31, 411:413, 42, 61)) %>% mutate(status = 22) %>% select(status, id, value),
  mort %>% filter(id %in% c(121:123, 131:133, 15:17, 31, 411:413, 42, 61)) %>% mutate(status = 23) %>% select(status, id, value)
) 

# 1. 10. microsite --------------------------------------------------------

microsite <- data.frame(id = c(1:47),
                        value = c(1:47))

# 1. 11. yes / no ---------------------------------------------------------

yes_no <- data.frame(id = c(0:1),
                     value = c("no", "yes"))

# 1. 12. export -----------------------------------------------------------

lookups <- list("lookup_landform" = landform,
                "lookup_hillform" = hillform,
                "lookup_species" = species,
                "lookup_status" = status,
                "lookup_growth" = growth,
                "lookup_layer" = layer,
                "lookup_decay" = decay,
                "lookup_decayht" = decayht,
                "lookup_c_mort" = mort.status,
                "lookup_microsite" = microsite,
                "lookup_yes_no" = yes_no) 

write.xlsx(lookups, "lookups.xlsx")

# ! close database connection ---------------------------------------------

poolClose(KELadmin);poolClose(KELuser)
