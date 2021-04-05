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

towers <- readRDS('towers.rds') %>%
   st_transform(4326)

APP_TITLE <- 'Smoke Locate'


ui <- shinyUI(fluidPage(
      title = APP_TITLE,
      #responsive = TRUE,
      theme = shinytheme("superhero"),
      header = NULL,
      useShinydashboard(),
      shinyalert::useShinyalert(),
      tags$div(style = 'text-align: center;',
               h1(APP_TITLE, icon('fire', class = 'orange')),
               #h5(textOutput('subtitle')),
               tags$hr()
      ),
      includeCSS('www/css/styles.css'),
      fluidRow(
         leafletOutput('map', height = 450),
         tags$hr()
      ),
      fluidRow(
         uiOutput('towers')
      ),
      fluidRow(
         style = 'padding: 20px;',
         actionBttn('button', 'Calculate',
                          style = 'material-flat',
                          block = TRUE,
                          color = 'warning',
                          size = 'lg'))
  )
)


server <- function(input, output, session) {
   
   selected <- reactiveValues(towers = c())
   
   output$map <- renderLeaflet({

      leaflet() %>%
         addProviderTiles(providers$CartoDB.Positron, group = "Default") %>%
         addTiles(group = 'Streets') %>%
         addProviderTiles(providers$Esri.WorldImagery, group = "Aerial") %>%
         addMarkers(data = towers, layerId = ~ FEATURE_ID) %>%
         addMeasure(
            position = "bottomleft",
            primaryLengthUnit = "meters",
            primaryAreaUnit = "sqmeters",
            activeColor = "#3D535D",
            completedColor = "#7D4479") %>%
         addScaleBar(position = 'bottomright') %>%
         leaflet.extras::addFullscreenControl() %>%
         leafem::addMouseCoordinates(epsg = 4326) %>%
         addLayersControl(
            baseGroups = c("Default", "Street", "Aerial"),
            overlayGroups = c("Estimate", "Triangulate"),
            options = layersControlOptions(collapsed = FALSE)
         )
         
      
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
            
            column(12, h3('Please Select 2 or more Towers'))
            
         } else {
            
            tagList(lapply(1:length(selected$towers), function(n){
               
               id <- selected$towers[n]
               r <- filter(towers, FEATURE_ID == id)
               name <- r[['NAME_LABEL']]
               
               column(12 / length(selected$towers),
                      h3(name),
                      numericInput(glue('bearing_{id}'), label = 'Bearing', value = NULL),
                      sliderInput(glue('vis_{id}'), label = 'Visibility (km)', min = 0, max = 50, step = 5, value = 50), 
                      tags$hr()
                      )
               
            }))
            
         }

         
         
      })
      
      
      # render forms
      
   })
   
   observeEvent(input$button, {
      
      if (length(selected$towers) < 2) {
         
         shinyalert("Error",
                    "You need to select more than one tower before a location can be estimated")
         
         ##TODO:  change this to estimate location based on one tower with bearing and distance
         
      } else {
         
         bearings <<- map_dbl(selected$towers, ~ glue('bearing_{.x}') %>% input[[.]])
         visibility <<- map_dbl(selected$towers, ~ glue('vis_{.x}') %>% input[[.]] * 1000)
         
         twrs <<- towers %>% 
            filter(FEATURE_ID %in% selected$towers) %>%
            left_join(tibble(FEATURE_ID = selected$towers,
                             bear = bearings,
                             vis = visibility))
         
         
         
         
         
         #create end points
         dest <- destPoint(st_coordinates(twrs), twrs$bear, twrs$vis)
         
         # draw lines from towers to end points
         lines <- st_coordinates(twrs) %>%
            cbind(dest) %>% 
            as.data.frame() %>% 
            split(1:nrow(.)) %>%
            map(., ~ st_linestring(rbind(c(.x$X,.x$Y),c(.x$lon,.x$lat)))) %>%
            st_as_sfc(crs = 4326)
         
         # get points where lines intersect
         tri <- lines %>%
            st_intersection() %>% 
            st_as_sf(crs = 4326) %>%
            mutate(type = st_geometry_type(x))
         
         tri_lines <- tri %>%
            filter(type == 'LINESTRING')
         
         tri_points <- tri %>%
            filter(type == 'POINT') 
         
         # draw circle around points
         cent <- tri_points %>%
            st_union() %>%
            st_convex_hull() %>% 
            st_centroid() 
         
         # determine if some lines do not intersect (TODO: needs work)
         # need to flash warning when no lines intersect
         if (nrow(tri_points) < nrow(tri_lines)) {
            
            radius <- max(st_distance(twrs, cent)) * 0.005
            msg <- 'Some lines did not intersect. Accuracy reflects distance from towers to intersecting point(s)'
            
         } else {
            
            radius <- max(st_distance(tri_points, cent))
            msg <- 'All lines intersect.'
         }
         
         
         circ <- cent %>%
            st_transform(3111) %>%
            st_buffer(radius * 1.1) %>%
            st_transform(4326)
         
         # convert location centre to gda and reverse geocode
         address <<- ''
         try(
            
            address <<- cent %>%
               st_as_sf(crs = 4326) %>%
               tmaptools::rev_geocode_OSM(projection = 4326)
            
         )
         
         if (address != '') {
            
            coord_lab <- st_coordinates(cent)
            
            label <- glue('{address[[1]]$name}<BR>',
                          "Longitude: {round(coord_lab[1], 8)}<BR>",
                          "Latitude: {round(coord_lab[2], 8)}<BR>",
                          "Accuracy: {floor(radius)}m<BR>",
                          "Message: {msg}")
         }
         
         
         fire_icon <- makeAwesomeIcon(icon= 'fire', markerColor = 'red', iconColor = '#FFFFFF', library = "fa")
         bb <- round(st_bbox(circ), 4)
         
         # add lines and circle to map proxy
         leafletProxy('map', session) %>%
            addPolygons(data = circ, group = 'Estimate',
                        fillColor = 'red', 
                        weight = 0.5) %>%
            addAwesomeMarkers(data = cent, group = 'Estimate',
                              icon = fire_icon,
                              popup = label) %>%
            addPolylines(data = tri_lines, group = 'Triangulate',
                         weight = 1,
                         color = 'darkred') %>%
            addCircleMarkers(data = tri_points, group = 'Triangulate',
                             radius = 10,
                             fillColor = 'green',
                             weight = 0.8,
                             color = 'darkgrey') %>%
            flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']]) %>%
            hideGroup('Triangulate')
         
         
      }
      
      
   })
   
}


shinyApp(ui = ui, server = server)

