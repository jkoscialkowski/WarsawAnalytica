#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
    coords <- as.data.frame(rbind(c(52.211944, 20.981944),
                                  c(52.246485, 21.003989)))
    colnames(coords) <- c("lat","lng")
    rownames(coords) <- c("MIM", "WNE")

    output$map <- renderLeaflet({
        coords[input$place,] %>% leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>% addMarkers()
    })

    output$text <- reactive({
        paste(input$place)
    })
    
})
