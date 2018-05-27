
library(shiny)
library(leaflet)
load("deploy.RData")


shinyServer(function(input, output) {
   
    centre <- c(52.41696, 16.90722)
    
    ## Przeliczanie metrow na stopnie za polska Wikipedia. Przyjety do tych obliczen promien Ziemi to 6371.1km.
    ## Srednia szerokosc geograficzna w Warszawie uzyta do obliczen to 52.2183
    side = 500
    met_deg <- c(side/111196.672, side*360/2/pi/6371100/cos(52.41696*pi/180))
    
    clean <- reactive({
        clean <- POST(url = "http://api.locit.pl/webservice/address-autocomplete/v2.0.0/", 
                      query = list(key = "maraton0n895gbsgc72bbksa042mad02", 
                                   schema = "basic", query = paste(input$address, "Poznan", sep = " "), 
                                   format = "json", charset = "UTF-8"))
        clean <- content(clean, as = "parse")
        clean
    })
    
    newdata <- reactive({
        data.frame(x = ceiling((as.numeric(clean()$data[[1]]$x) - centre[2])/met_deg[2]),
                   y = ceiling((as.numeric(clean()$data[[1]]$y) - centre[1])/met_deg[1]),
                   doch_min = (input$income - 1000 - mean(X_nonsc[,3]))/sd(X_nonsc[,3]),
                   doch_med = (input$income - mean(X_nonsc[,4]))/sd(X_nonsc[,4]),
                   doch_max = (input$income + 1000 - mean(X_nonsc[,5]))/sd(X_nonsc[,5]),
                   zabudowa = input$buildings,
                   plec = ifelse(input$sex == "Female", 1, ifelse(input$sex == "Male", 0, 0.5)),
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
      
    prediction <- reactive({
        class <- which.min(apply(sweep(model$centers, 2, as.numeric(newdata())), 1, function(x) sum(x^2)))
        class_logi <- model$cluster == class
        score <- apply(sweep(X_sc[class_logi,], 2, model$centers[class,]), 1, function(x) sum(x^2))
        score <- cbind(grid_to_rect(X_grid = X_nonsc$x[class_logi], Y_grid = X_nonsc$y[class_logi], 
                                    centre_x = centre[2], centre_y = centre[1],
                                    deg_per_tick_x = met_deg[2], deg_per_tick_y = met_deg[1]), score)
        score[,5] <- order(score[,5])/nrow(score)
        colnames(score) <- c("lng1", "lat1", "lng2", "lat2", "score")
        score
    })
    
    output$text <- reactive({
        clean()$data[[1]]$x
    })
    
    output$map <- renderLeaflet({
        #req(input$amenities)
        req(input$address)
        # req(input$sex)
        # req(input$age)
        # req(input$income)
        # req(input$buildings)
        # req(input$children)
        # req(input$teenagers)
        # req(input$students)
        # 
        leaflet() %>% setView(lat = clean()$data[[1]]$y, lng = clean()$data[[1]]$x, zoom = 15) %>% 
            addProviderTiles(providers$OpenStreetMap) %>%
            addMarkers(lat = as.numeric(clean()$data[[1]]$y), lng = as.numeric(clean()$data[[1]]$x), 
                       popup = "You currently live here") %>%
            addRectangles(lng1 = prediction()[,1], lat1 = prediction()[,2], 
                          lng2 = prediction()[,3], lat2 = prediction()[,4], 
                          fillColor = colorQuantile("Blues", prediction()[,5], n = 9)(prediction()[,5]),
                          fillOpacity = 0.7, weight = 1)
    })
    
    
    # output$address_clean <- reactive({
    #     clean <- POST(url = "http://api.locit.pl/webservice/address-autocomplete/v2.0.0/", 
    #                   query = list(key = "maraton0n895gbsgc72bbksa042mad02", 
    #                                schema = "basic", query = input$address, format = "json", charset = "UTF-8"))
    #     clean <- content(clean, as = "parse")
    #     paste(clean$data[[1]]$street, clean$data[[1]]$building, clean$data[[1]]$city, clean$data[[1]]$zip, sep = " ")
    #     
    # })
})
