#####################################
##### GETTING AND CLEANING DATA #####
#####################################

## Getting ISRS data from the CPD website
## Thiago R. Oliveira

#####################################

library(tidyverse)
library(stringr)
library(tidygeocoder)

# read and append all data sets from 2016 to 2021
isrs <- 
  read.csv('data/ISR/2016-ISR.csv') %>%
  dplyr::select(CONTACT_CARD_ID, STREET_NO, STREET_DIRECTION_CD, STREET_NME, CITY, ZIP_CD) %>%
  mutate(year = 2016) %>%
  bind_rows(read.csv('data/ISR/2017-ISR.csv') %>%
              dplyr::select(CONTACT_CARD_ID, STREET_NO, STREET_DIRECTION_CD, STREET_NME, CITY, ZIP_CD) %>%
              mutate(year = 2017)) %>%
  bind_rows(read.csv('data/ISR/2018-ISR.csv') %>%
              dplyr::select(CONTACT_CARD_ID, STREET_NO, STREET_DIRECTION_CD, STREET_NME, CITY, ZIP_CD) %>%
              mutate(ZIP_CD = as.character(ZIP_CD),
                     year = 2018)) %>%
  bind_rows(read.csv('data/ISR/2019-ISR-Jan-Jun.csv') %>%
              dplyr::select(CONTACT_CARD_ID, STREET_NO, STREET_DIRECTION_CD, STREET_NME, CITY, ZIP_CD) %>%
              mutate(ZIP_CD = as.character(ZIP_CD),
                     year = 2019)) %>%
  bind_rows(read.csv('data/ISR/2019-ISR-Jul-Dec.csv') %>%
              dplyr::select(CONTACT_CARD_ID, STREET_NO, STREET_DIRECTION_CD, STREET_NME, CITY, ZIP_CD) %>%
              mutate(ZIP_CD = as.character(ZIP_CD),
                     year = 2019)) %>%
  bind_rows(read.csv('data/ISR/2020-ISR.csv') %>%
              dplyr::select(CONTACT_CARD_ID, STREET_NO, STREET_DIRECTION_CD, STREET_NME, CITY, ZIP_CD) %>%
              mutate(ZIP_CD = as.character(ZIP_CD),
                     year = 2020)) %>%
  bind_rows(read.csv('data/ISR/2021-ISR.csv') %>%
              dplyr::select(CONTACT_CARD_ID, STREET_NO, STREET_DIRECTION_CD, STREET_NME, CITY, ZIP_CD) %>%
              mutate(ZIP_CD = as.character(ZIP_CD),
                     year = 2021)) %>%
  filter(str_detect(CITY, "^CH"))

# checking raw counts by year
isrs_timeseries <-
  isrs %>%
  group_by(year) %>%
  summarise(n = n())
plot(isrs_timeseries)

# cleaning address data
isrs <- 
  isrs %>%
  mutate(number = str_sub(STREET_NO, end = -3L) %>% paste("00", sep = ""),
         direction = case_when(
           STREET_DIRECTION_CD == 'E' ~ 'East',
           STREET_DIRECTION_CD == 'N' ~ 'North',
           STREET_DIRECTION_CD == 'W' ~ 'West',
           STREET_DIRECTION_CD == 'S' ~ 'South',
         )) %>%
  mutate(addr = paste(number, direction, STREET_NME)) %>%
  mutate(address = paste(addr, "Chicago", "Illinois", sep = ", ")) %>%
  dplyr::select(CONTACT_CARD_ID, year, address)


# Geocode 50 random addresses to test
set.seed(1234)
test <- isrs %>%
  sample_n(50) %>%
  geocode(address = address, method = "osm") %>%
  rename(lat_osm = lat,
         long_osm = long) %>%
  geocode(address = address, method = "census") %>%
  rename(lat_census = lat,
         long_census = long) %>%
  geocode(address = address, method = "arcgis") %>%
  rename(lat_arcgis = lat,
         long_arcgid = long)
  
# Check with Google Maps
## ArcGIS is flawless!

# Geocode all ~70k addresses
isrs <- isrs %>%
  geocode(address = address, method = "arcgis")

# Save csv file
write.csv2(isrs, "data/tidy/isrs.csv")
save(isrs, file = "data/tidy/isrs.Rdata")

