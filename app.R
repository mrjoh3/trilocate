#
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(shinyjs)

library(sf)
library(geosphere)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(glue)
library(dplyr)
library(purrr)

library(tmaptools)


APP_TITLE <- 'Smoke Locate'

# import all towers (TODO: find a dynamic import that updates)
towers <- readRDS('towers.rds') %>%
   st_transform(4326)

# get current situation 
statewide <- st_read('https://www.emergency.vic.gov.au/public/osom-geojson.json', stringsAsFactors = FALSE) %>% 
   st_transform(4326) %>%
   mutate(sizeFmt = as.character(sizeFmt),
          sizeFmt = ifelse(sizeFmt == 'character(0)', '', sizeFmt)) %>%
   filter(!is.na(st_is_valid(.))) %>%
   st_make_valid() %>%
   select(sourceId,
          feedType,
          sourceTitle,
          cap,
          category1, 
          category2,
          status,
          location,
          incidentFeatures,
          webHeadline,
          url,
          resources, 
          sizeFmt)

# burn area
ba <- statewide %>% 
   filter(feedType == 'burn-area') %>%
   mutate(area = st_area(.),
          area = round(units::set_units(area, ha), 2),
          type = st_geometry_type(.)) %>%
   filter(!grepl('LINE', type)) %>%
   select(status, location, area) %>%
   st_cast('MULTIPOLYGON')

# planned burn
pb <- statewide %>% 
   filter(category1 == 'Planned Burn') %>%
   select(status, location, category = category2, category1, resources, size = sizeFmt)

# fire incident
fi <- statewide %>% 
   filter(category1 == 'Fire') %>%
   select(status, location, category = category2, category1, resources, size = sizeFmt)

# map icons
all_icons <- awesomeIconList(
   Fire = makeAwesomeIcon(icon= 'fire', markerColor = 'darkred', iconColor = '#FFFFFF', library = "fa"),
   `Planned Burn` = makeAwesomeIcon(icon= 'fire', markerColor = 'purple', iconColor = '#FFFFFF', library = "fa"),
   towers = makeAwesomeIcon(icon= 'binoculars', markerColor = 'green', iconColor = '#FFFFFF', library = "fa")
)

ui <- shinyUI(fluidPage(
      title = APP_TITLE,
      #responsive = TRUE,
      theme = shinytheme("superhero"),
      header = NULL,
      useShinydashboard(),
      shinyalert::useShinyalert(),
      useShinyjs(),

            # Application title
      titlePanel(withTags(
         div(icon('fire', class = 'orange'), APP_TITLE, 
             div(class = 'pull-right',
                 a(href = 'https://github.com/mrjoh3/trilocate',
                   icon('github'))), hr() )
      ), 
      windowTitle = APP_TITLE
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
         addProviderTiles(providers$OpenStreetMap, group = 'Streets') %>%
         addProviderTiles(providers$Esri.WorldImagery, group = "Aerial") %>%
         addPolygons(data = ba,
                     weight = 1,
                     color = 'red',
                     fillColor = 'black',
                     popup = ~paste(sep = '<br>',
                                    glue('<strong>Status: {status}</strong>'),
                                    glue('Location: {location}'),
                                    glue('Burnt Area (Ha): {area}')),
                     group = 'Burnt Area') %>%
         addAwesomeMarkers(data = pb %>% st_cast("POINT"), 
                           icon = ~all_icons[category1],
                           popup = ~paste(sep = '<br>',
                                          glue('<strong>{status}</strong>'),
                                          ifelse(category1 == category,
                                                 glue('Category: {category1}'),
                                                 glue('Category: {category1} - {category}')),
                                          glue('Location: {location}'),
                                          glue('Resources: {resources}'),
                                          glue('Size: {size}')),
                           group = 'Planned Burns') %>%
         addAwesomeMarkers(data = fi %>% st_cast("POINT"), 
                           icon = ~all_icons[category1],
                           popup = ~paste(sep = '<br>',
                                          glue('<strong>{status}</strong>'),
                                          ifelse(category1 == category,
                                                 glue('Category: {category1}'),
                                                 glue('Category: {category1} - {category}')),
                                          glue('Location: {location}'),
                                          glue('Resources: {resources}'),
                                          glue('Size: {size}')),
                           group = 'Current Fires') %>%
         addAwesomeMarkers(data = towers, layerId = ~ FEATURE_ID, 
                           label = ~ glue('{NAME_LABEL} ({FEATURE_ID})'),
                           popup = ~ glue('{NAME_LABEL} ({FEATURE_ID})'),
                           icon = all_icons['towers'],
                           group = 'Towers') %>%
         leaflet.extras::addSearchFeatures('Towers', options = searchFeaturesOptions(openPopup = TRUE)) %>%
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
            baseGroups = c("Default", "Vicmap", "Streets", "Aerial"),
            overlayGroups = c("Estimate", "Triangulate", "Planned Burns", "Current Fires", "Burnt Area"),
            options = layersControlOptions(collapsed = FALSE)
         ) %>%
         hideGroup(c("Estimate", "Triangulate", "Planned Burns", "Current Fires", "Burnt Area"))
         
      
   })
   
   
   observeEvent(input$map_marker_click, {

      click <- input$map_marker_click
      
      isolate(selected$towers <- c(selected$towers, click$id))

   })
   
   observe({
      
      # construct UI
      output$towers <- renderUI({
         
         if (length(selected$towers) == 0) {
            
            column(12, h3('To begin click on 2 or more Towers'))
            
         } else {
            
            tagList(lapply(1:length(selected$towers), function(n){
               
               id <- selected$towers[n]
               r <- filter(towers, FEATURE_ID == id)
               name <- r[['NAME_LABEL']]
               
               column(12 / length(selected$towers),
                      h3(name),
                      numericInput(glue('bearing_{id}'), label = 'Bearing', value = NULL),
                      sliderInput(glue('vis_{id}'), label = 'Visibility (km)', min = 0, max = 100, step = 10, value = 50), 
                      tags$hr()
                      )
               
            }))
            
         }

         
         
      })
      
      
      # render forms
      
   })
   
   observeEvent(input$button, {
      
      if (length(selected$towers) < 2) {
         
         shinyalert("ERROR",
                    "You need to select more than one tower before a location can be estimated",
                    type = 'error')
         
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
         # need to account for 2 lines and only on intersect (currently assumes perfect accuracy)
         if (nrow(tri_points) == 0) {
            
            ## NO INTERSECTION FOUND 
            
            bb_lines <- st_bbox(lines)
            
            # add lines and circle to map proxy
            leafletProxy('map', session) %>%
               addPolylines(data = st_as_sf(lines, crs = 4326), group = 'Triangulate',
                            weight = 1.2,
                            color = 'darkred') %>%
               flyToBounds(bb_lines[['xmin']], bb_lines[['ymin']], bb_lines[['xmax']], bb_lines[['ymax']]) %>%
               showGroup('Triangulate')
            
            # pan page back to map (effect only noticeable on mobile)
            runjs('document.getElementById("map").scrollIntoView();')
            
            shinyalert("WARNING",
                       "The supplied towers and bearings do not intersect. Towers may be observing multiple incidents, or the visibility of a tower may need to be extended. Use the red lines on the map as a guide.",
                       type = 'warning')
            
            
         } else {
            
            ## INTERSECTION SUCCESSFULL
            
            if (nrow(tri_points) < nrow(tri_lines)) { # TODO: examine different intersection patterns. Pattern below may not always hold
               
               radius <- max(st_distance(twrs, cent)) * 0.005
               # TODO: make msg dark red
               
               if (nrow(tri_lines) == 2 & nrow(tri_points) == 1) {
                  msg <- 'Only 2 towers were selected. Accuracy reflects distance from towers to intersecting point(s)'
               } else {
                  msg <- 'Some lines did not intersect. Accuracy reflects distance from towers to intersecting point(s)'
               }
               
               
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
                            weight = 1.2,
                            color = 'darkred') %>%
               addCircleMarkers(data = tri_points, group = 'Triangulate',
                                radius = 10,
                                fillColor = 'green',
                                weight = 0.8,
                                color = 'darkgrey') %>%
               flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']]) %>%
               showGroup('Estimate')
            
            # pan page back to map (effect only noticeable on mobile)
            runjs('document.getElementById("map").scrollIntoView();')
            
            
            
            
         }
         
      }
      
      
   })
   
}


shinyApp(ui = ui, server = server)

