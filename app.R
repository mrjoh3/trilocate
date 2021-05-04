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

source('icon_utils.R')

APP_TITLE <- 'Smoke Locate'
APP_CRS <- 4326 # WGS84
REPORT_CRS <- 4283 # GDA94

# import all towers 
towers <- readRDS('towers.rds') %>%
   st_transform(APP_CRS) 

tfb <- readRDS('tfb.rds') %>%
   st_transform(APP_CRS)

# get current fire situation 
statewide <- st_read('https://www.emergency.vic.gov.au/public/osom-geojson.json', stringsAsFactors = FALSE) %>% 
   st_transform(APP_CRS) %>%
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

# current incidents and planned burns
current_incidents <- statewide %>% 
   filter(category1 %in% c('Fire', 'Planned Burn')) %>%
   select(sourceId, sourceTitle, status, location, category = category2, category1, resources, size = sizeFmt)



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
         div(icon('fire', class = 'orange', lib = 'glyphicon'), APP_TITLE, 
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
         div(actionBttn('button', 'Calculate',
                          style = 'material-flat',
                          block = TRUE,
                          color = 'warning',
                          size = 'lg'))
      ),
      fluidRow(column(6,
                      div(style = 'padding-top: 15px;',
                          shinyWidgets::materialSwitch(inputId = 'add_obs', 
                                                       label = 'Add Observation Post', 
                                                       inline = TRUE, 
                                                       status = 'success', value = FALSE)),
                      ),
               column(6,
                      # display download button after calculation success
                      div(style = 'float: right;', hidden = TRUE, id = 'download_div',
                             downloadBttn(outputId = 'download',
                                          style = 'material-flat',
                                          size = 'md',
                                          color = 'success'))
                      )),
      fluidRow(column(3,
                      div(style = 'display: flex; justify-content: center;',
                          HTML(markerLegendHTML(all_icons)))),
               column(4,
                      includeMarkdown('instructions.md')
                      ),
               column(4,
                      includeMarkdown('about.md')
                      ),
               column(1)),
      fluidRow(tags$hr(),
               div(class = 'pull-right', style = 'padding-right: 20px; padding-bottom: 20px;',
                   icon('fire', class = 'orange', lib = 'glyphicon'), APP_TITLE)
      )
  )
)


server <- function(input, output, session) {
   
   selected <- reactiveValues(towers = c(),
                              inc_bearings = tibble(),
                              success = FALSE)
   tow <- reactiveValues(ers = towers)
   
   export <-reactiveValues(map = NULL,
                           address = '',
                           coords = NULL,
                           crs = NULL)
   
   # only show download button after successful calculation
   observe({
      if (selected$success){
         shinyjs::showElement(id = 'download_div')
      } else {
         shinyjs::hideElement(id = 'download_div')
      }
   })
   
   
   # add new observation points by clicking anywhere on the map
   observeEvent(input$map_click, {
      
      if (input$add_obs) {
         
         click <- input$map_click
         sc <- input$map_shape_click
         
         assign('mclk', click, .GlobalEnv)
         
         if (!is.null(click) & is.null(sc$group)){
            
            id <- as.integer(Sys.time())
            isolate(selected$towers <- c(selected$towers, id))
            
            isolate(tow$ers <- st_sf(type = 'mobile location', 
                                     id = id, 
                                     name = 'New Location', 
                                     geometry = list(st_point(c(click$lng, click$lat))), crs = APP_CRS) %>%
                       rbind(tow$ers, .))
            
         } else {
            towers
         }
      }
   })
   
   
   
   output$map <- renderLeaflet({

      map <- leaflet() %>%
         addProviderTiles(providers$CartoDB.Positron, group = "Default") %>%
         addWMSTiles('https://base.maps.vic.gov.au/service?', layers = 'CARTO_WM', group = 'Vicmap') %>%
         addProviderTiles(providers$OpenStreetMap, group = 'Streets') %>%
         addProviderTiles(providers$Esri.WorldImagery, group = "Aerial") %>%
         addPolygons(data = tfb, group = 'TFB Districts', layerId = ~ TFB_DISTRICT, 
                     weight = 0.8,
                     color = '#980c1d',
                     fillColor = 'lightgrey',
                     fillOpacity = 0.3,
                     label = ~ TFB_DISTRICT
                     ) %>%
         addPolygons(data = ba,
                     weight = 1,
                     color = 'red',
                     fillColor = 'black',
                     popup = ~paste(sep = '<br>',
                                    glue('<strong>Status: {status}</strong>'),
                                    glue('Location: {location}'),
                                    glue('Burnt Area (Ha): {area}')),
                     group = 'Burnt Area') %>%
         addAwesomeMarkers(data = current_incidents %>% st_cast("POINT"), 
                           icon = ~all_icons[category1],
                           popup = ~paste(sep = '<br>',
                                          glue('<strong>{status}</strong>'),
                                          ifelse(category1 == category,
                                                 glue('Category: <strong>{category1}</strong>'),
                                                 glue('Category: <strong>{category1} - {category}</strong>')),
                                          glue('Location: {location}'),
                                          glue('Resources: {resources}'),
                                          glue('Size: {size}')),
                           group = 'Current Incidents') %>%
         addAwesomeMarkers(data = tow$ers, layerId = ~ id, # TODO: responsive value very slow may be better to use observe, remove markers and add again with update
                           label = ~ glue('{name} ({id})'),
                           #popup = ~ glue('{name} ({id})'),
                           icon = ~ all_icons['Fire Lookout'],
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
         leafem::addMouseCoordinates(epsg = APP_CRS) %>%
         addLayersControl(
            baseGroups = c("Default", "Vicmap", "Streets", "Aerial"),
            overlayGroups = c("Smoke Location", "Triangulate", "TFB Districts", 'Current Incidents', "Burnt Area"),
            options = layersControlOptions(collapsed = FALSE)
         ) %>%
         hideGroup(c("Smoke Location", "Triangulate", "TFB Districts", 'Current Incidents', "Burnt Area"))
         
      isolate(export$map <- map)
      
      map
      
   })
   
   proxy_map <- leafletProxy('map', session)
   
   
   # when clicking on a tower record the id and calculate bearings to incidents within 50km
   observeEvent(input$map_marker_click, {

      mclk <- input$map_marker_click
      
      if (!is.null(mclk$id)) {
         
         isolate(selected$towers <- c(selected$towers, mclk$id))
         
         # calculate incident bearings
         bears <- current_incidents %>%
            filter(st_is_within_distance(., st_sfc(st_point(c(mclk$lng, mclk$lat)), crs = APP_CRS), dist = 50000, sparse = FALSE)) 
         
         if (nrow(bears) > 0) {
            bearing_df <<- tibble(tower_id = mclk$id,
                                 tower_name = filter(towers, id == mclk$id) %>% pull(name),
                                 tower_X = mclk$lng,
                                 tower_Y = mclk$lat,
                                 sourceId = bears$sourceId,
                                 sourceTitle = bears$sourceTitle,
                                 location = bears$location,
                                 bearing = bearing(c(mclk$lng, mclk$lat), st_coordinates(bears)[,1:2])) %>%
               mutate(bearing = ifelse(bearing < 0, 360 + bearing, bearing),
                      bearing = round(bearing, 4)) %>%
               cbind(st_coordinates(bears))
            
            isolate(selected$inc_bearings <- rbind(selected$inc_bearings, bearing_df))
         }
      }
   })
   
   
   # zoom into selected map area
   observeEvent(input$map_shape_click, {
      
      shp_click <- input$map_shape_click
      if (!is.null(shp_click$group) & shp_click$group == "TFB Districts") {
         
         bb_shp <- tfb %>% filter(TFB_DISTRICT == shp_click$id) %>% st_bbox()
         
         proxy_map %>%
            flyToBounds(bb_shp[['xmin']], bb_shp[['ymin']], bb_shp[['xmax']], bb_shp[['ymax']]) 
      }
   })
   
   
   # render forms for each tower selected
   observe({
      
      output$towers <- renderUI({
         
         if (length(selected$towers) == 0) {
            
            column(12, h3('To begin click on 2 or more Towers'))
            
         } else {
            
            tagList(lapply(1:length(selected$towers), function(n){
               
               tower_id <- selected$towers[n]
               r <- filter(tow$ers, id == tower_id)
               name <- r[['name']]
               
               column(12 / length(selected$towers),
                      h3(name),
                      numericInput(glue('bearing_{tower_id}'), label = 'Bearing', value = NULL),
                      sliderInput(glue('vis_{tower_id}'), label = 'Visibility (km)', min = 0, max = 100, step = 10, value = 50), 
                      tags$hr()
                      )
            }))
         }
      })
   })
   
   
   # calculate location of smoke sighting
   observeEvent(input$button, {
      
     withProgress({
        
        setProgress(0.1, 'Calculating', 'preparing for calculation')
        
        if (length(selected$towers) < 2) {
           
           isolate(selected$success <- FALSE)
           
           shinyalert("ERROR",
                      "You need to select more than one tower before a location can be estimated",
                      type = 'error')
           
           ##TODO:  change this to estimate location based on one tower with bearing and distance
           
        } else {
           
           bearings <- map_dbl(selected$towers, ~ glue('bearing_{.x}') %>% input[[.]])
           visibility <- map_dbl(selected$towers, ~ glue('vis_{.x}') %>% input[[.]] * 1000)
           
           twrs <- tow$ers %>% 
              filter(id %in% selected$towers) %>%
              left_join(tibble(id = selected$towers,
                               bear = bearings,
                               vis = visibility))
           
           isolate(export$towers <- twrs)
           
           #create end points
           dest <- destPoint(st_coordinates(twrs), twrs$bear, twrs$vis)
           
           # draw lines from towers to end points
           lines <- st_coordinates(twrs) %>%
              cbind(dest) %>% 
              as.data.frame() %>% 
              split(1:nrow(.)) %>%
              map(., ~ st_linestring(rbind(c(.x$X,.x$Y),c(.x$lon,.x$lat)))) %>%
              st_as_sfc(crs = APP_CRS)
           
           setProgress(0.3, 'Calculating', 'collate tower details')
           
           # get points where lines intersect
           tri <- lines %>%
              st_intersection() %>% 
              st_as_sf(crs = APP_CRS) %>%
              mutate(type = st_geometry_type(x))
           
           tri_lines <- tri %>%
              filter(type == 'LINESTRING')
           
           tri_points <- tri %>%
              filter(type == 'POINT') 
           
           # draw circle around points and get the centre
           cent <- tri_points %>%
              st_union() %>%
              st_convex_hull() %>% 
              st_centroid() 
           
           isolate({
              expt <<- cent %>% st_transform(REPORT_CRS)
              export$coords <- expt %>% st_coordinates() %>% as.data.frame()
              export$crs <- expt %>% st_crs()
           })
           
           setProgress(0.5, 'Calculating', 'get bearing to local incidents')
           
           # create lines for incident bearings
           if (nrow(selected$inc_bearings) > 0) {
              inc_bearings_lines <- selected$inc_bearings %>% 
                 split(1:nrow(.)) %>%
                 map(., ~ st_linestring(rbind(c(.x$tower_X,.x$tower_Y),c(.x$X,.x$Y)))) %>%
                 st_as_sfc(crs = APP_CRS) %>%
                 st_sf() %>%
                 cbind(selected$inc_bearings)
              
              add_bearings <- function(map){
                 map %>% 
                    addPolylines(data = inc_bearings_lines, group = 'Current Incidents',
                                 weight = 1,
                                 color = 'orange',
                                 popup = ~ glue('From: {tower_name}<br>',
                                                'To: {location}<br>',
                                                'Bearing: {bearing}'))
              }
              
              isolate(export$map <- add_bearings(export$map))
              add_bearings(proxy_map)
              
           }
           
           
           # determine if some lines do not intersect (TODO: needs work)
           # need to flash warning when no lines intersect
           # need to account for 2 lines and only on intersect (currently assumes perfect accuracy)
           if (nrow(tri_points) == 0) {
              
              ## NO INTERSECTION FOUND 
              
              bb_lines <- st_bbox(lines)
              
              # add lines and circle to map proxy
              proxy_map %>%
                 addPolylines(data = st_as_sf(lines, crs = APP_CRS), group = 'Triangulate',
                              weight = 1.2,
                              color = 'darkred') %>%
                 flyToBounds(bb_lines[['xmin']], bb_lines[['ymin']], bb_lines[['xmax']], bb_lines[['ymax']]) %>%
                 showGroup('Triangulate')
              
              # pan page back to map (effect only noticeable on mobile)
              runjs('document.getElementById("map").scrollIntoView();')
              
              isolate(selected$success <- FALSE)
              
              shinyalert("WARNING",
                         "The supplied towers and bearings do not intersect. Towers may be observing multiple incidents, or the visibility of a tower may need to be extended. Use the red lines on the map as a guide.",
                         type = 'warning')
              
              
           } else {
              
              ## INTERSECTION SUCCESSFULL
              
              setProgress(0.7, 'Calculating', 'identify location')
              
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
                 st_transform(APP_CRS)
              
              # convert location centre to gda and reverse geocode
              try({
                 
                 export$address <- cent %>%
                    st_as_sf(crs = APP_CRS) %>%
                    st_transform(REPORT_CRS) %>%
                    tmaptools::rev_geocode_OSM(projection = REPORT_CRS)
                 
              })
              
              if (export$address != '') {
                 
                 coord_lab <- st_coordinates(cent)
                 
                 label <- glue('{export$address[[1]]$name}<BR>',
                               "Longitude: {round(coord_lab[1], 8)}<BR>",
                               "Latitude: {round(coord_lab[2], 8)}<BR>",
                               "Projection: GDA94<BR>",
                               "Accuracy: {floor(radius)}m<BR>",
                               "Message: {msg}")
              }
              
              bb <- round(st_bbox(circ), 2)
              
              # add lines and circle to map
              
              setProgress(0.9, 'Calculating', 'add to map')
              
              add_2_map <- function(map){
                 map %>%
                    addPolygons(data = circ, group = 'Smoke Location',
                                fillColor = 'red', 
                                weight = 0.5) %>%
                    addAwesomeMarkers(data = cent, group = 'Smoke Location',
                                      icon = all_icons['Smoke Location'],
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
                    showGroup('Smoke Location')
              }
              
              isolate(export$map <- add_2_map(export$map))
              
              add_2_map(proxy_map)
              
              
              # pan page back to map (effect only noticeable on mobile)
              runjs('document.getElementById("map").scrollIntoView();')
              
              
              
              
           }
           
        }
        
        updateMaterialSwitch(session, 'add_obs', value = FALSE)
        isolate(selected$success <- TRUE)
        
        setProgress(0.1, 'Calculating', 'completed')
        
     })
      
   })
   
   output$download <- downloadHandler(
      
      filename = paste0("smoke_report_", format(Sys.time(), '%Y%m%d_%H%M'), ".html"),
      content = function(file) {
         
         withProgress({
            
            setProgress(0.2, 'Download Report', 'prepareing data')
            
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(map = export$map,
                           towers = export$towers,
                           address = export$address,
                           inc_bearings = selected$inc_bearings,
                           coords = export$coords,
                           crs = export$crs
            )
            
            setProgress(0.6, 'Download Report', 'rendering report')
            
            rmarkdown::render(tempReport,
                              rmdformats::html_docco(thumbnails = FALSE, 
                                                     mathjax = NULL, 
                                                     gallery = FALSE
                                                     ),
                              #rmarkdown::pdf_document(),
                              output_file = file,
                              clean = TRUE,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
            
            setProgress(1, 'Download Report', 'completed')
            
         })

      }
   )
   
   
   
}


shinyApp(ui = ui, server = server)

