#################
### The theory of legal cynicism
### Thiago R. Oliveira
#################

## Part 1. Ecological analysis

## Loading necessary packages 
library(tidyverse)      # loads all tidyverse packages
library(haven)          # reads foreign data sources (e.g., Stata)
library(spdep)          # spatial analysis
library(spatialreg)     # spatial analysis  
library(dpm)            # dynamic panel models
options(scipen = 999)   # no scientific notation

## Loading necessary data files

# SQF data 2012-2015 (source: Wood)
load("data/tidy/cc_tract_2000.RData")

# SQF data 2016-2021 (source: CPD)
load("data/tidy/isrs_tract_2000.RData")

# Binding both datasets
sqfs <-
  cc_tract_2000 %>%
  rename(stops = cc) %>%
  bind_rows(isrs_tract_2000 %>%
              rename(stops = isrs))

# Crime data from the 1990's (source: Sampson)
crime90s <-
  read_stata("data/crime/data/export/CPD-Time-Varying 1995-2006.dta") %>%
  mutate(tract_2000 = NA) %>%
  dplyr::select(tract_2000, tract, year, homicide, robbery, burglary) %>%
  mutate(tract = factor(tract))

# Crime data from the 2000s (source: CPD)
crime00s <-
  read.csv("data/phdcn_neighborhoods/crime/data/export/tract/cpd_crimes_tract_2000_year.csv") %>%
  filter(year > 2006 & year < 2022) %>%
  mutate(tract = if_else(
    substring(tract_2000, 6, 6) == 0,  substring(tract_2000, 7, 9), substring(tract_2000, 6, 9)
  )) %>%
  dplyr::select(tract_2000, tract, year, homicide, robbery, burglary) %>%
  mutate(tract = factor(tract))

# Binding both data sources
crime <-
  crime90s %>%
  bind_rows(crime00s)

# demographics
demographics <- read.csv("data/phdcn_neighborhoods/demographics/data/export/ltdb_factors_2000.csv") %>%
  filter(year == 2010) %>%
  mutate(pop_density = population / area) %>%
  dplyr::select(-year)

## Data wrangling

# merging all data sources by tracts (2000)
chicago <-
  sqfs %>%
  mutate(tract_2000 = factor(tract_2000)) %>%
  left_join(crime00s %>%
              mutate(tract_2000 = factor(tract_2000))) %>%
  mutate(violent = homicide + robbery) %>%
  left_join(demographics %>% 
              mutate(tract_2000 = factor(tract_2000)))
#dplyr::select(-tract)
#  left_join(read_excel('data/PHDCN/tract90_to_nc.xlsx') %>%
#            dplyr::select(tract, nc_num) %>%
#              mutate(tract = factor(tract))) %>%
#  left_join(demographics) %>%
#  dplyr::select(-c(tract, nc_num))

# function: get all neighbouring units (by C. Lanfear)
get_neighbors <- function(df, id, snap = 0.001){
  out <- df |>
    dplyr::select(all_of(id), "geometry") |>
    # Reclassing poly2nb lets it be used in a normal mutate call
    mutate(neighbors = magrittr::set_class(spdep::poly2nb(geometry, snap = snap), "list")) |>
    st_drop_geometry() |>
    unnest(neighbors) |>
    mutate(neighbors = df[[id]][neighbors])
  return(out)
}

# spatial data set (by tracts)
chicago.geo <-
  st_read(dsn = "data/boundaries/data/export/tract/2000", "chicago_tract_2000") %>%
  rename(tract_2000 = tract) %>%
  left_join(chicago)

# for each tract, get neighbouring tracts
neighbours <- get_neighbors(df = chicago.geo, id = "tract_2000")

# calculate the average scores of each variable among neighbouring tracts
chicago_splags <- 
  neighbours %>%
  inner_join(chicago.geo) %>%
  group_by(neighbors, year) %>%
  summarise(across(c(stops, homicide:pop_density),
                   ~ mean(., na.rm = TRUE), .names = "splag_{.col}"), .groups = "drop") %>%
  rename('tract_2000' = 'neighbors')

# merge final data set
chicago <-
  chicago %>%
  left_join(chicago_splags)

# create panel data set
chicago.panel <-
  chicago %>%
  mutate(wave = year - 2011,
         log.pop_density = log(pop_density + 1)) %>%
  panel_data(id = tract_2000,
             wave = wave)

## Analysis

# checking raw counts by year
stops_timeseries <-
  ggplot(chicago, aes(x = year, y = stops)) + geom_line()

# DPM: Violent crime and sqfs (pre-2016)
dpm_pre2016 <- dpm(violent ~ pre(lag(stops)) + lag(splag_stops) | FAC_disadv + FAC_hispimm + FAC_stability + log.pop_density, # + log.n_POP_under18, #| perc_black + perc_hisp + perc_owned + perc_fhh + perc_foreign + perc_unemployed + perc_poverty + perc_moved,
                              data = chicago.panel %>% filter(year < 2017), error.inv = T, information = "observed", missing = "ML")
dpm_pre2016 <- dpm(violent ~ pre(lag(stops)) + pre(stops) | FAC_disadv + FAC_hispimm + FAC_stability + log.pop_density, # + log.n_POP_under18, #| perc_black + perc_hisp + perc_owned + perc_fhh + perc_foreign + perc_unemployed + perc_poverty + perc_moved,
                   data = chicago.panel %>% filter(year < 2017), error.inv = T, information = "observed", missing = "ML")

# DPM: Violent crime and sqfs (post-2016)
dpm_post2016 <- dpm(violent ~ pre(lag(stops)) + lag(splag_stops) | FAC_disadv + FAC_hispimm + FAC_stability + log.pop_density, # + log.n_POP_under18, #| perc_black + perc_hisp + perc_owned + perc_fhh + perc_foreign + perc_unemployed + perc_poverty + perc_moved,
                   data = chicago.panel %>% filter(year > 2015), error.inv = T, information = "observed", missing = "ML")
dpm_post2016 <- dpm(violent ~ pre(lag(stops))| FAC_disadv + FAC_hispimm + FAC_stability + log.pop_density, # + log.n_POP_under18, #| perc_black + perc_hisp + perc_owned + perc_fhh + perc_foreign + perc_unemployed + perc_poverty + perc_moved,
                    data = chicago.panel %>% filter(year > 2015), error.inv = T, information = "observed", missing = "ML")
dpm_post2016 <- dpm(violent ~ pre(stops)| FAC_disadv + FAC_hispimm + FAC_stability + log.pop_density, # + log.n_POP_under18, #| perc_black + perc_hisp + perc_owned + perc_fhh + perc_foreign + perc_unemployed + perc_poverty + perc_moved,
                    data = chicago.panel %>% filter(year > 2015), error.inv = T, information = "observed", missing = "ML")

## this one
dpm_post2016 <- dpm(violent ~ pre(lag(stops)) + pre(stops) | FAC_disadv + FAC_hispimm + FAC_stability + log.pop_density + splag_FAC_disadv + splag_FAC_hispimm + splag_FAC_stability,
                    data = chicago.panel %>% filter(year > 2014), error.inv = T, information = "observed", missing = "ML")

dpm_post2016 <- dpm(violent ~ pre(stops) + splag_stops | FAC_disadv + FAC_hispimm + FAC_stability + log.pop_density, # + log.n_POP_under18, #| perc_black + perc_hisp + perc_owned + perc_fhh + perc_foreign + perc_unemployed + perc_poverty + perc_moved,
                   data = chicago.panel %>% filter(year > 2015), error.inv = T, information = "observed", missing = "ML")

# DPM: Violent crime and sqfs (all)
dpm <- dpm(violent ~ pre(lag(stops)) + lag(splag_stops) | FAC_disadv + FAC_hispimm + FAC_stability + log.pop_density, # + log.n_POP_under18, #| perc_black + perc_hisp + perc_owned + perc_fhh + perc_foreign + perc_unemployed + perc_poverty + perc_moved,
                    data = chicago.panel, error.inv = T, information = "observed", missing = "ML")
dpm <- dpm(violent ~ pre(stops) + splag_stops | FAC_disadv + FAC_hispimm + FAC_stability + log.pop_density, # + log.n_POP_under18, #| perc_black + perc_hisp + perc_owned + perc_fhh + perc_foreign + perc_unemployed + perc_poverty + perc_moved,
                    data = chicago.panel, error.inv = T, information = "observed", missing = "ML")

