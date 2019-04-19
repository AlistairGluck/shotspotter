library(tigris)
library(tidyverse)
library(maps)
library(readr)
library(fs)
library(sf)
library(lubridate)

fresno = read_csv("http://justicetechlab.org/wp-content/uploads/2018/09/fresno_sst.csv", col_types = cols(
  address = col_character(),
  city = col_character(),
  state = col_character(),
  datetime = col_character(),
  numrounds = col_double(),
  shotspotterflexid = col_double(),
  lat = col_double(),
  long = col_double()
))

shapes = urban_areas(class = "sf") %>%
  filter(NAME10 == "Fresno, CA")

fresno = fresno %>%
  mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M:%OS"))

fresno = fresno[!(duplicated(fresno$shotspotterflexid)), ]

fresno_final = fresno %>%
  select(long, lat, numrounds, datetime) %>%
  filter(!is.na(lat)) %>%
  filter(!is.na(long)) %>%
  filter(long > -120 & long < -119.45) %>%
  filter(lat > 36.5 & lat < 37) %>%
  mutate(day = date(datetime)) %>%
  mutate(day = format(day, format = "%B %e, %Y"))

locations = st_as_sf(fresno_final, coords = c("long", "lat"), crs = 4326)

ggplot(data = shapes) + 
  geom_sf() +
  geom_sf(data = locations, aes(alpha = 0.5, size = numrounds))



