
library(shiny)
library(leaflet)
load("deploy.RData")
load("modelRF.Rdata")

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
        df <- data.frame(PLEC=integer(),
                         B19=integer(),
                         is_wies=logical(),
                         is_miasto_do_19_tys=integer(),
                         is_miasto_20_99_tys=integer(),
                         is_miasto_100_499_tys=integer(),
                         is_Vocational=integer(),
                         is_Secondary=integer(),
                         is_Higher=integer(),
                         is_26_30=integer(),
                         is_31_35=integer(),
                         is_36_40=integer(),
                         is_41_45=integer(),
                         is_45_65=integer(),
                         is_more66=integer()
        )
        df[1, ] <- c(1, 5500, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0)
        names(df)
        city_input <- input$city
        cities <- c("village", "city under 19k", "city 19-99k", "city 100-499k", "city over 500k")
        cities_model <- c('is_wies', 'is_miasto_do_19_tys', 'is_miasto_20_99_tys', 'is_miasto_100_499_tys')
        if(is.na(cities_model[which(city_input == cities)])){
            df[, which(names(df) %in% cities_model)] <- 0
        }else{
            df[, which(names(df) %in% cities_model)] <- 0
            df[, cities_model[which(cities == city_input)]] <- 1
        }
        
        sexies <- c("Male", "Female", "Prefer not to say")
        sex_input <- input$sex
        
        ind <- which(sexies == sex_input)
        if(ind == 0){
            # the more in df_all_cases database
            sex <- 1
        }else{
            sex <- ind - 1
        }
        
        ages <- c("is_26_30", "is_31_35", "is_36_40", "is_41_45", "is_45_65", "is_more66")         
        age_input <- input$age
        
        if(age_input < 25){
            age <- 0
        }else if(age_input < 30){
            age <- "is_26_30"
        }else if(age_input < 35){
            age <- "is_31_35"
        }else if(age_input < 40){
            age <- 'is_36_40'
        }else if(age_input < 45){
            age <- 'is_41_45'
        }else if(age_input < 65){
            age <- 'is_45_65'
        }else{
            age <- 'is_more66'
        }
        df[, ages] <- 0
        if(age != 0){
            df[, age] <- 1
        }
        education_input <- input$education
        educations <- c("Prefer not to say", 'is_Vocational', 'is_Secondary', 'is_Higher')
        ind <- which(education_input == gsub('is_', '', educations))
        ind
        if(length(ind) == 0){
            df[, educations] <- 0
        }else if(ind == 0){
            df[, educations] <- 0
        }else{
            df[, educations] <- 0
            df[, which(names(df) == educations[ind])] <- 1
        }
        earnings_input <- input$income
        if(earnings_input == 0){
            earnings_input <- mean(X$doch_med)
        }else{
            earnings <- earnings_input
        }
        
        
        rf_new <- predict(fit, newdata = df)
        
        data.frame(x = ceiling((as.numeric(clean()$data[[1]]$x) - centre[2])/met_deg[2]),
                   y = ceiling((as.numeric(clean()$data[[1]]$y) - centre[1])/met_deg[1]),
                   doch_min = (input$income - 2*565*(1 - rf_new) - mean(X_nonsc[,3]))/sd(X_nonsc[,3]),
                   doch_med = (input$income - mean(X_nonsc[,4]))/sd(X_nonsc[,4]),
                   doch_max = (input$income + 2*565*(1 - rf_new) - mean(X_nonsc[,5]))/sd(X_nonsc[,5]),
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
