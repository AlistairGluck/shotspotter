library(tigris)
library(tidyverse)
library(maps)
library(readr)
library(fs)
library(sf)
library(lubridate)
library(gganimate)
library(transformr)
library(ggthemes)

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
  mutate(date_shot = date(datetime)) %>%
  arrange(date_shot) 

locations = st_as_sf(fresno_final, coords = c("long", "lat"), crs = 4326)

ggplot(data = shapes) + 
  geom_sf() +
  geom_sf(data = locations, aes(colour = numrounds, alpha = 0.6)) + 
  labs(caption = "Source: Justice Tech Lab ShotSpotter Data") +
  guides(alpha = FALSE) +
  scale_colour_gradient(name = "Rounds Fired", 
                        low = "mediumblue", 
                        high = "orangered3") + 
  theme(legend.position = c(0.8, 0.1)) +
  theme(panel.grid.major = element_line(colour = "white")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_map() +
  
  #Making each data a different frame in the animation
  
  transition_time(date_shot) + 
  ease_aes() +
  
  #Leaving each previous frame as a permanent mark on the map
  
  shadow_mark(past = TRUE) +
  
  #Adding a title that includes the date shown in the present frame
  
  ggtitle("Gunshots Fired in Fresco, California on {frame_time}")


