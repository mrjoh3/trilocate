#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)

library(sf)
library(leaflet)
library(glue)
library(dplyr)

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
    )
  )
)


server <- function(input, output) {
   
   selected <- reactiveValues(towers = c())
   
   output$map <- renderLeaflet({

      leaflet() %>%
         addTiles() %>%
         addMarkers(data = towers, layerId = ~ FEATURE_ID)
      
   })
   
   observe({
      
      proxy <- leafletProxy("map")
      
   })
   
   observeEvent(input$map_marker_click, {

      click <- input$map_marker_click
      
      isolate(selected$towers <- c(selected$towers, click$id))
      
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
                      h3(name),
                      numericInput(glue('bearing_{id}'), label = 'Bearing', value = NULL),
                      sliderInput(glue('vis_{id}'), label = 'Visibility (km)', min = 0, max = 50, step = 5, value = 20), 
                      tags$hr()
                      )
               
            }))
            
         }

         
         
      })
      
      
      # render forms
      
   })
   
}


shinyApp(ui = ui, server = server)

