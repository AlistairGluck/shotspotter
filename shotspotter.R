# Loading packages

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

# Reading in the CSV file with data from Fresno

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

# Used tigris package to obtain shape files for Fresno

shapes = urban_areas(class = "sf") %>%
  
# NAME10 is the variable that corresponds to the municipality's name 
  
  filter(NAME10 == "Fresno, CA")

# Converting the "datetime" variable to a POSIXct class rather than a character string 

fresno = fresno %>%
  mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M:%OS"))

# Subsetting the data to only include observations with a unique shotspotterflexid
# (the ID assigned to each shooting incident identified by the program)

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

# Creating a map out of the shapes data

ggplot(data = shapes) + 
  geom_sf() +

# Delineating the number of rounds fired in each shooting incident by colour and
# decreasing the transparency to show overlap of the points
  
  geom_sf(data = locations, aes(colour = numrounds, alpha = 0.6)) + 

# Adding a source note
  
  labs(caption = "Source: Justice Tech Lab ShotSpotter Data") +

# Removing the legend for alpha
  
  guides(alpha = FALSE) +

# Changing the colour scale of the plot
  
  scale_colour_gradient(name = "Rounds Fired", 
                        low = "mediumblue", 
                        high = "orangered3") + 
  
# Moving the legend position so it doesn't cover the map
  
  theme(legend.position = c(0.8, 0.1)) +

# Centering the title of the plot 
  
  theme(plot.title = element_text(hjust = 0.5)) +

# Applying the map theme 
  
  theme_map() +
  
# Making each data a different frame in the animation
  
  transition_time(date_shot) + 
  ease_aes() +
  
# Leaving each previous frame as a permanent mark on the map
  
  shadow_mark(past = TRUE) +
  
# Adding a title that includes the date shown in the present frame
  
  ggtitle("Gunshots Fired in Fresno, California on {frame_time}")

# Save the animation as a GIF to include in the app 

anim_save("shotspotter/fresno.gif", animation = last_animation())


