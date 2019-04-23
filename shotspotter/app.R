#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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


shapes <- urban_areas(class = "sf") %>%
  filter(NAME10 == "Fresno, CA")

fresno <- read_csv("http://justicetechlab.org/wp-content/uploads/2018/09/fresno_sst.csv", col_types = cols(
  address = col_character(),
  city = col_character(),
  state = col_character(),
  datetime = col_character(),
  numrounds = col_double(),
  shotspotterflexid = col_double(),
  lat = col_double(),
  long = col_double()
))

fresno <- fresno %>%
  mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M:%OS"))

fresno <- fresno[!(duplicated(fresno[["shotspotterflexid"]])), ]

# Define UI for application that draws a map of fresno

ui <- fluidPage(tabsetPanel(
  
  tabPanel("Choose the Range",
   
   # Application title
  
   titlePanel("Shooting Incidents in Fresno, California from 2015-2018"),
   
   
   
   
   #Sidebar with a two numeric input for number of shots per incident 
   
   sidebarLayout(
      sidebarPanel(
        tags$h4(helpText("Select the range of shots fired per shooting incident. 
                         The maximum number is 83, the minimum is 1.")),
            numericInput(inputId = "minshots",
                     label = "Minimum Number of Rounds Fired",
                     value = 1,
                     min = 1, 
                     max = 83),
            numericInput(inputId = "maxshots", 
                        label = "Maximum Number of Rounds Fired",
                        value = 83,
                        min = 1, 
                        max = 83),
        tags$h6(helpText("If you receive an error message, it likely means that either you have not selected the minimum or maximum, 
                        or that there are no shooting incidents in the selected range 
                         (there are very few incidents at the higher end of the range)."))
       ),
     
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput(outputId = "map"),
        tags$h6(helpText("Each point represents an incident of gunfire, and the color represents rounds fired (the number of shots). \nFind the code here:")),
        tags$link("https://github.com/AlistairGluck/shotspotter"),
        tags$h5(helpText("By Alistair Gluck and Dasha Metropolitansky"))
       ))
    ),
  tabPanel("Animated Graphic",
           basicPage(imageOutput("animated_map"))
)))

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$map <- renderPlot({
    
      fresno_final <- fresno %>%
       select(long, lat, numrounds, datetime) %>%
       filter(!is.na(lat)) %>%
       filter(!is.na(long)) %>%
       filter(long > -120 & long < -119.45) %>%
       filter(lat > 36.5 & lat < 37) %>%
       mutate(date_shot = date(datetime)) %>%
       arrange(date_shot) %>% 
        filter(input$minshots <= numrounds & numrounds <= input$maxshots)
     
      locations <- st_as_sf(fresno_final, coords = c("long", "lat"), crs = 4326)
      
      map_fresno <- ggplot(data = shapes) + 
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
       theme_map()
      
      map_fresno
   })
   output$animated_map <- renderImage({

     list(src = "fresno.gif",
          contentType = 'image/gif')
   }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

