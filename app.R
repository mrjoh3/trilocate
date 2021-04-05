#
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)

library(sf)
library(geosphere)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(glue)
library(dplyr)
library(purrr)

library(tmaptools)

towers <- readRDS('towers.rds')

APP_TITLE <- 'Smoke Locate'


ui <- shinyUI(fluidPage(
      title = APP_TITLE,
      #responsive = TRUE,
      theme = shinytheme("superhero"),
      header = NULL,
      useShinydashboard(),
      tags$div(style = 'text-align: center;',
               h1(APP_TITLE, icon('fire', class = 'orange')),
               #h5(textOutput('subtitle')),
               tags$hr()
      ),
      includeCSS('www/css/styles.css'),
      fluidRow(
         leafletOutput('map'),
         tags$hr()
      ),
      fluidRow(
         uiOutput('towers')
      ),
      fluidRow(actionBttn('button', 'Calculate'))
  )
)


server <- function(input, output, session) {
   
   selected <- reactiveValues(towers = c())
   
   output$map <- renderLeaflet({

      leaflet() %>%
         addTiles() %>%
         addMarkers(data = towers, layerId = ~ FEATURE_ID) %>%
         addMeasure(
            position = "bottomleft",
            primaryLengthUnit = "meters",
            primaryAreaUnit = "sqmeters",
            activeColor = "#3D535D",
            completedColor = "#7D4479") %>%
         addScaleBar(position = 'bottomright') %>%
         leaflet.extras::addFullscreenControl() %>%
         leafem::addMouseCoordinates(epsg = 4283) 
         
      
   })
   
   # observe({
   #    
   #    proxy <- leafletProxy("map")
   #    
   # })
   
   observeEvent(input$map_marker_click, {

      click <- input$map_marker_click
      
      isolate(selected$towers <- c(selected$towers, click$id))
      
      #assign('towers', selected$towers, envir = .GlobalEnv)
   })
   
   observe({
      
      # output$towers <- renderUI({
      #    h3(glue('seems to work {paste(selected$towers, collapse = ", ")}'))
      # })
      
      
      # construct UI
      output$towers <- renderUI({
         
         if (length(selected$towers) == 0) {
            
            column(12, h3('Please Select 1 or more Towers'))
            
         } else {
            
            tagList(lapply(1:length(selected$towers), function(n){
               
               id <- selected$towers[n]
               r <- filter(towers, FEATURE_ID == id)
               name <- r[['NAME_LABEL']]
               
               column(12 / length(selected$towers),
                      h3(name, id),
                      numericInput(glue('bearing_{id}'), label = 'Bearing', value = NULL),
                      sliderInput(glue('vis_{id}'), label = 'Visibility (km)', min = 0, max = 50, step = 5, value = 20), 
                      tags$hr()
                      )
               
            }))
            
         }

         
         
      })
      
      
      # render forms
      
   })
   
   observeEvent(input$button, {
      
      bearings <<- map_dbl(selected$towers, ~ glue('bearing_{.x}') %>% input[[.]])
      visibility <<- map_dbl(selected$towers, ~ glue('vis_{.x}') %>% input[[.]] * 1000)
      
      twrs <<- towers %>% 
         filter(FEATURE_ID %in% selected$towers) %>%
         left_join(tibble(FEATURE_ID = selected$towers,
                          bear = bearings,
                          vis = visibility))
      


      
      
      #create end points
      dest = destPoint(st_coordinates(twrs), twrs$bear, twrs$vis)
      
      # draw lines from towers to end points
      lines <- st_coordinates(twrs) %>%
         cbind(dest) %>% 
         as.data.frame() %>% 
         split(1:nrow(.)) %>%
         map(., ~ st_linestring(rbind(c(.x$X,.x$Y),c(.x$lon,.x$lat)))) %>%
         st_as_sfc(crs = 4283)

      # get points where lines intersect
      tri <- lines %>%
         st_transform(3111) %>%
         st_intersection() %>% 
         st_as_sf(crs = 3111) %>%
         mutate(type = st_geometry_type(x)) %>%
         filter(type == 'POINT') 
      
      # draw circle around points
      cent <- tri %>%
         st_union() %>%
         st_convex_hull() %>% 
         st_centroid() 
      radius <- max(st_distance(tri, cent))
      
      circ <- st_buffer(cent, radius * 1.1) %>%
         st_transform(4283)
      
      # convert location centre to gda and reverse geocode
      # address <<- cent %>%
      #    st_transform(4283) %>%
      #    st_as_sf(crs = 4283) %>%
      #    tmaptools::rev_geocode_OSM(projection = 4283)
         
      fire_icon <- makeAwesomeIcon(icon= 'fire', markerColor = 'red', iconColor = '#FFFFFF', library = "fa")
      bb <- round(st_bbox(circ), 4)
      
      # add lines and circle to map proxy
      leafletProxy('map', session) %>%
         addPolygons(data = circ, fillColor = 'red', weight = 0.5) %>%
         addAwesomeMarkers(data = st_transform(cent, 4283),
                           icon = fire_icon) %>%
         flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
      
   })
   
}


shinyApp(ui = ui, server = server)

