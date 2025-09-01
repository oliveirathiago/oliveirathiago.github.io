###################################
##### ISRS DATA BY CENSUS TRACTS ##
###################################

### Calculating the number of ISRS by tract by year
### Thiago R. Oliveira
### Jan 2024

##############################################

library(tidyverse)   # wrangling data
library(sf)          # spatial analysis

##############################################

# read tidy ISRS data
load('data/tidy/isrs.RData')

# Load tract boundaries
load("data/boundaries/data/derived/chicago_tract_2000.RData")
load("data/boundaries/data/derived/chicago_tract_2010.RData")
load("data/boundaries/data/derived/chicago_tract_2020.RData")
#####

isrs_tract <- isrs %>%
  filter(!is.na(long) & !is.na(lat)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(st_crs(chicago_tract_2000)) %>%
  st_join(chicago_tract_2000 %>% select(tract_2000 = tract, geometry)) %>%
  st_join(chicago_tract_2010 %>% select(tract_2010 = tract, geometry)) %>%
  st_join(chicago_tract_2020 %>% select(tract_2020 = tract, geometry)) %>%
  st_drop_geometry()
save(isrs_tract, file = "data/tidy/isrs_tract.RData")

# calculate number of ISRS by tract each year

isrs_tract_2000 <- isrs_tract %>%
  filter(!is.na(tract_2000)) %>%
  count(year, tract_2000, name = "isrs") %>%
  complete(year, tract_2000, fill = list(isrs = 0))

isrs_tract_2010 <- isrs_tract %>%
  filter(!is.na(tract_2010)) %>%
  count(year, tract_2010, name = "trrs") %>%
  complete(year, tract_2010, fill = list(isrs = 0))

isrs_tract_2020 <- isrs_tract %>%
  filter(!is.na(tract_2020)) %>%
  count(year, tract_2020, name = "trrs") %>%
  complete(year, tract_2020, fill = list(isrs = 0))

# save data set
save(isrs_tract_2000, file = "data/tidy/isrs_tract_2000.RData")
save(isrs_tract_2010, file = "data/tidy/isrs_tract_2010.RData")
save(isrs_tract_2020, file = "data/tidy/isrs_tract_2020.RData")
write_csv(isrs_tract_2000, file = "data/tidy/isrs_tract_2000.csv")
write_csv(isrs_tract_2010, file = "data/tidy/isrs_tract_2010.csv")
write_csv(isrs_tract_2020, file = "data/tidy/isrs_tract_2020.csv")