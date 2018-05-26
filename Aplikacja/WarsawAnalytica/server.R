
library(shiny)
library(leaflet)

shinyServer(function(input, output) {
   
    centre <- c(52.41696, 16.90722)
    
    ## Przeliczanie metrow na stopnie za polska Wikipedia. Przyjety do tych obliczen promien Ziemi to 6371.1km.
    ## Srednia szerokosc geograficzna w Warszawie uzyta do obliczen to 52.2183
    side = 500
    met_deg <- c(side/111196.672, side*360/2/pi/6371100/cos(52.41696*pi/180))
    
    
    output$map <- renderLeaflet({
        leaflet() %>% setView(lat = centre[1], lng = centre[2], zoom = 15) %>% 
            addProviderTiles(providers$OpenStreetMap)
    })
    
    newdata <- reactive({
        clean <- POST(url = "http://api.locit.pl/webservice/address-autocomplete/v2.0.0/", 
                      query = list(key = "maraton0n895gbsgc72bbksa042mad02", 
                                   schema = "basic", query = input$address, format = "json", charset = "UTF-8"))
        clean <- content(clean, as = "parse")
        
        data.frame(x = ceiling((as.numeric(clean$data[[1]]$x) - centre[2])/met_deg[2]),
                   y = ceiling((as.numeric(clean$data[[1]]$y) - centre[1])/met_deg[1]),
                   doch_min = input$income - 1000,
                   doch_med = input$income,
                   doch_max = input$income + 1000,
                   zabudowa = input$buildings,
                   plec = ifelse(input$sex == "Female", 1, ifelse(input$sex == "Male", 0, 0.5)),
                   wiek_stud = ifelse(input$age < 25, 1, 0),
                   wiek_25_34 = ifelse(input$age >= 25 & input$age < 35, 1, 0),
                   wiek_35_44 = ifelse(input$age >= 35 & input$age < 45, 1, 0),
                   wiek_45_64 = ifelse(input$age >= 45 & input$age < 65, 1, 0),
                   dzieci = input$children/10,
                   nastolatkowie = input$teenagers/10,
                   studenci = input$students/10,
                   jedzenie = ifelse("Food" %in% input$amenities, 1, 0),
                   kawiarnie = ifelse("Cafes" %in% input$amenities, 1, 0),
                   trans_pub = ifelse("Public transport" %in% input$amenities, 1, 0),
                   punkty_usl = ifelse("Service points" %in% input$amenities, 1, 0),
                   sklepy_siec = ifelse("Chain shops" %in% input$amenities, 1, 0),
                   edukacja = ifelse("Education" %in% input$amenities, 1, 0),
                   kultura_rozr = ifelse("Culture & entertainment" %in% input$amenities, 1, 0),
                   galerie = ifelse("Shopping malls" %in% input$amenities, 1, 0),
                   zdrowie = ifelse("Health" %in% input$amenities, 1, 0),
                   miejsca_kultu = ifelse("Places of worship" %in% input$amenities, 1, 0))
    })
      
    
    
    output$text <- reactive({
        paste(newdata())
    })
    
    output$address_clean <- reactive({
        clean <- POST(url = "http://api.locit.pl/webservice/address-autocomplete/v2.0.0/", 
                      query = list(key = "maraton0n895gbsgc72bbksa042mad02", 
                                   schema = "basic", query = input$address, format = "json", charset = "UTF-8"))
        clean <- content(clean, as = "parse")
        paste(clean$data[[1]]$street, clean$data[[1]]$building, clean$data[[1]]$city, clean$data[[1]]$zip, sep = " ")
        
    })
})
