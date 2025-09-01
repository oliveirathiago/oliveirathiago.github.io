#####################################
##### GETTING AND CLEANING DATA #####
#####################################

## Getting contact cards data from GW
## Thiago R. Oliveira

#####################################

library(tidyverse)
library(stringr)
library(tidygeocoder)

# read and append all data sets from 2012 to 2015
cc <- 
  read.csv('data/ContactCardData/ContactCardData2012.csv') %>%
  dplyr::select(date = DATE, street_number = ST.NUM, direction = DIR, street_name = STREET.NAME) %>%
  mutate(year = 2012) %>%
  bind_rows(read.csv('data/ContactCardData/ContactCardData2013.csv') %>%
              dplyr::select(date = DATE, street_number = ST.NUM, direction = DIR, street_name = STREET.NAME) %>%
              mutate(year = 2013)) %>%
  bind_rows(read.csv('data/ContactCardData/ContactCardData2014.csv') %>%
              dplyr::select(date = Contact.Date, street_number = Street.Number, direction = Street.Direction, street_name = Street.Name) %>%
              mutate(year = 2014)) %>%
  bind_rows(read.csv('data/ContactCardData/ContactCardData2015.csv') %>%
              dplyr::select(date = Contact.Date, street_number = Street.Number, direction = Street.Direction, street_name = Street.Name) %>%
              mutate(year = 2015))

# checking raw counts by year
cc_timeseries <-
  cc %>%
  group_by(year) %>%
  summarise(n = n())
plot(cc_timeseries)

# cleaning address data
cc <- 
  cc %>%
  mutate(number = str_sub(street_number, end = -3L) %>% paste("00", sep = ""),
         direction_complete = case_when(
           direction == 'E' ~ 'East',
           direction == 'N' ~ 'North',
           direction == 'W' ~ 'West',
           direction == 'S' ~ 'South',
         )) %>%
  mutate(addr = paste(number, direction_complete, street_name)) %>%
  mutate(address = paste(addr, "Chicago", "Illinois", sep = ", "),
         ID = row_number()) %>%
  dplyr::select(ID, year, address)

# Geocode 50 random addresses to test
set.seed(1234)
test <- cc %>%
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
cc <- cc %>%
  geocode(address = address, method = "arcgis")

# Save csv file
write.csv2(cc, "data/tidy/cc.csv")
save(cc, file = "data/tidy/cc.Rdata")