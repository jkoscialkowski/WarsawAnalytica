
library(shiny)
library(leaflet)

shinyServer(function(input, output) {
   
    coords <- as.data.frame(rbind(c(52.211944, 20.981944),
                                  c(52.246485, 21.003989)))
    colnames(coords) <- c("lat","lng")
    rownames(coords) <- c("MIM", "WNE")
    
    centre <- c(52.2183, 21.02557)
    
    
    ## Przeliczanie metrow na stopnie za polska Wikipedia. Przyjety do tych obliczen promien Ziemi to 6371.1km.
    ## Srednia szerokosc geograficzna w Warszawie uzyta do obliczen to 52.2183
    
    met_deg <- reactive({
        c(input$lattice_dim/111196.672, input$lattice_dim*360/2/pi/6371100/cos(52.2183*pi/180))
    })
    
    output$map <- renderLeaflet({
        coords[input$place,] %>% leaflet() %>% setView(lat = centre[1], lng = centre[2], zoom = 15) %>% 
            addProviderTiles(providers$OpenStreetMap) %>% addMarkers() %>% 
            addRectangles(lat1 = centre[1], lng1 = centre[2], lat2 = centre[1] + met_deg()[1], lng2 = centre[2] + met_deg()[2])
    })
    
    output$text <- reactive({
        paste(input$place)
    })
    
    output$address_clean <- reactive({
        
    })
})
