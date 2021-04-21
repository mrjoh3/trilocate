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
   st_transform(4326) %>%
   select(FEATSUBTYP, 
          FEATURE_ID, 
          NAME_LABEL)

tfb <- readRDS('tfb.rds') %>%
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

# current incidents and planned burns
current_incidents <- statewide %>% 
   filter(category1 %in% c('Fire', 'Planned Burn')) %>%
   select(sourceId, sourceTitle, status, location, category = category2, category1, resources, size = sizeFmt)


# map icons
all_icons <- awesomeIconList(
   'Smoke Location' = makeAwesomeIcon(icon= 'fire', markerColor = 'orange', iconColor = '#FFFFFF', library = "fa"),
   'Fire' = makeAwesomeIcon(icon = 'fire', markerColor = 'darkred', iconColor = '#FFFFFF', library = "fa"),
   'Planned Burn' = makeAwesomeIcon(icon = 'fire', markerColor = 'purple', iconColor = '#FFFFFF', library = "fa"),
   'Fire Lookout' = makeAwesomeIcon(icon = 'binoculars', markerColor = 'green', iconColor = '#FFFFFF', library = "fa"),
   'Mobile Location' = makeAwesomeIcon(icon = 'truck', markerColor = 'blue', iconColor = '#FFFFFF', library = "fa")
)

# legend html generator
# Thanks to Andrew Reid on StackOverflow https://stackoverflow.com/a/47107058/1498485
markerLegendHTML <- function(IconSet) {
   
   # container div:
   legendHtml <- "<div style='padding: 10px; padding-bottom: 10px;'><h2 style='padding-top:0; padding-bottom:10px; margin: 0;'> Map Legend </h4>"
   
   n <- 1
   # add each icon for font-awesome icons icons:
   for (Icon in IconSet) {
      if (Icon[["library"]] == "fa") {
         legendHtml<- paste0(legendHtml, "<div style='width: auto; height: 45px'>",
                             "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
                             "<i style='color: #FFFFFF; margin-left: 0px; margin-top: 11px;' class= 'fa fa-",Icon[["icon"]]," fa-inverse'></i>",
                             "</div>",
                             "<p style='position: relative; top: 5px; display: inline-block;' >", names(IconSet)[n] ,"</p>",
                             "</div>")    
      }
      n<- n + 1
   }
   paste0(legendHtml, "</div>")
}



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
         div(actionBttn('button', 'Calculate',
                          style = 'material-flat',
                          block = TRUE,
                          color = 'warning',
                          size = 'lg')),
         div(style = 'padding-top: 15px;',
                shinyWidgets::materialSwitch(inputId = 'add_obs', 
                                             label = 'Add Observation Post', 
                                             inline = TRUE, 
                                             status = 'success', value = FALSE))
      ),
      fluidRow(column(2),
               column(4,
                      h2('Instructions'),
                      h3('Step 1:'),
                        p('Locate the towers that have observed a smoke column. ',
                          'You can zoom into the area or use the search ',
                          icon('search'), ' option in the top left of the map. ',
                          'You can also select the "TFB District" option in the map layers and then zoom to any "Total Fire Ban District" by clicking on it. '
                          ),
                        p('If an observation is not from a tower, you can click on the ', 
                          tags$strong('Add Observation Post'), ' switch. ',
                          'Then zoom into the area of the observation and click on the map to create a temporary location. '),
                      h3('Step 2:'),
                        p('Click on ',
                          tags$strong('ALL'),
                          'of the towers for which you have a known bearing to the smoke column.'),
                      h3('Step 3:'),
                        p('Enter the bearing of the smoke sighting for each tower.'),
                      h3('Step 4:'),
                        p('Click on the ', tags$strong('Calculate'), ' button')
                      ),
               column(4,
                      h2('About'),
                      p('In Victoria on a hot, dry summer day fire can spread at up to 10 km/h in forest and up to 20 km/h in grassland. ',
                        'The timeliness of the initial response to a fire is critical to containment and reducing the severity and impact of the fire. '),
                      p('Victoria has a network of ', nrow(towers), 'fire watch towers. When smoke is sighted these towers use a triangulation method to narrow in on the exact location. ',
                        'When you select 2 or more towers, the lines drawn from each tower at the given bearing will intersect indicating the location ',
                        'of the smoke.',
                        'This application is intended to replicate and simplify the manual triangulation process, while at the same time incorporate modern reverse geocoding ',
                        'capabilities that can convert a location into an address.'),
                      div(HTML(markerLegendHTML(all_icons)))
                      ),
               column(2))
  )
)


server <- function(input, output, session) {
   
   selected <- reactiveValues(towers = c(),
                              inc_bearings = tibble())
   tow <- reactiveValues(ers = towers)
   
   observeEvent(input$map_click, {
      
      if (input$add_obs) {
         
         click <- input$map_click
         sc <<- input$map_shape_click
         
         assign('mclk', click, .GlobalEnv)
         
         if (!is.null(click) & is.null(sc$group)){
            
            id <- as.integer(Sys.time())
            isolate(selected$towers <- c(selected$towers, id))
            
            isolate(tow$ers <- st_sf(FEATSUBTYP = 'mobile location', 
                                     FEATURE_ID = id, 
                                     NAME_LABEL = 'New Location', 
                                     geometry = list(st_point(c(click$lng, click$lat))), crs = 4326) %>%
                       rbind(tow$ers, .))
            
         } else {
            towers
         }
         
      }
      
   })
   
   
   
   output$map <- renderLeaflet({

      leaflet() %>%
         addProviderTiles(providers$CartoDB.Positron, group = "Default") %>%
         addTiles('https://base.maps.vic.gov.au/wmts/CARTO_WM/EPSG:3857/${z}/${x}/${y}.png', group = 'Vicmap') %>%
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
         addAwesomeMarkers(data = tow$ers, layerId = ~ FEATURE_ID, # TODO: responsive value vey slow may be better to use observe, remove markers and add again with update
                           label = ~ glue('{NAME_LABEL} ({FEATURE_ID})'),
                           #popup = ~ glue('{NAME_LABEL} ({FEATURE_ID})'),
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
         leafem::addMouseCoordinates(epsg = 4326) %>%
         addLayersControl(
            baseGroups = c("Default", "Vicmap", "Streets", "Aerial"),
            overlayGroups = c("Smoke Location", "Triangulate", "TFB Districts", 'Current Incidents', "Burnt Area"),
            options = layersControlOptions(collapsed = FALSE)
         ) %>%
         hideGroup(c("Smoke Location", "Triangulate", "TFB Districts", 'Current Incidents', "Burnt Area"))
         
      
   })
   
   proxy_map <- leafletProxy('map', session)
   
   
   observeEvent(input$map_marker_click, {

      mclk <<- input$map_marker_click
      
      if (!is.null(mclk$id)) {
         
         isolate(selected$towers <- c(selected$towers, mclk$id))
         
         # calculate incident bearings
         bears <- current_incidents %>%
            filter(st_is_within_distance(., st_sfc(st_point(c(mclk$lng, mclk$lat)), crs = 4326), dist = 50000, sparse = FALSE)) 
         
         if (nrow(bears) > 0) {
            bearing_df <- tibble(tower_id = mclk$id,
                                 tower_name = filter(towers, FEATURE_ID == mclk$id) %>% pull(NAME_LABEL),
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
   
   observeEvent(input$map_shape_click, {
      
      shp_click <- input$map_shape_click
      if (!is.null(shp_click$group) & shp_click$group == "TFB Districts") {
         
         bb_shp <- tfb %>% filter(TFB_DISTRICT == shp_click$id) %>% st_bbox()
         
         proxy_map %>%
            flyToBounds(bb_shp[['xmin']], bb_shp[['ymin']], bb_shp[['xmax']], bb_shp[['ymax']]) 
         
      }

         
   })
   
   observe({
      
      # construct UI
      output$towers <- renderUI({
         
         if (length(selected$towers) == 0) {
            
            column(12, h3('To begin click on 2 or more Towers'))
            
         } else {
            
            tagList(lapply(1:length(selected$towers), function(n){
               
               id <- selected$towers[n]
               r <- filter(tow$ers, FEATURE_ID == id)
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
         
         twrs <<- tow$ers %>% 
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
         
         
         # create lines for incident bearings
         if (nrow(selected$inc_bearings) > 0) {
            inc_bearings_lines <<- selected$inc_bearings %>% 
               split(1:nrow(.)) %>%
               map(., ~ st_linestring(rbind(c(.x$tower_X,.x$tower_Y),c(.x$X,.x$Y)))) %>%
               st_as_sfc(crs = 4326) %>%
               st_sf() %>%
               cbind(selected$inc_bearings)
            
            proxy_map %>%
               addPolylines(data = inc_bearings_lines, group = 'Current Incidents',
                            weight = 1,
                            color = 'orange',
                            popup = ~ glue('From: {tower_name}<br>',
                                           'To: {location}<br>',
                                           'Bearing: {bearing}'))
         }

         
         # determine if some lines do not intersect (TODO: needs work)
         # need to flash warning when no lines intersect
         # need to account for 2 lines and only on intersect (currently assumes perfect accuracy)
         if (nrow(tri_points) == 0) {
            
            ## NO INTERSECTION FOUND 
            
            bb_lines <- st_bbox(lines)
            
            # add lines and circle to map proxy
            proxy_map %>%
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
            
            bb <- round(st_bbox(circ), 4)
            
            # add lines and circle to map proxy
            proxy_map %>%
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
            
            # pan page back to map (effect only noticeable on mobile)
            runjs('document.getElementById("map").scrollIntoView();')
            
            
            
            
         }
         
      }
      
      updateMaterialSwitch(session, 'add_obs', value = FALSE)
      
   })
   
}


shinyApp(ui = ui, server = server)

