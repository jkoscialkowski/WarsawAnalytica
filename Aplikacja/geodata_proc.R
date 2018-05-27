## Geospatial data processing script

# Warsaw boundary coordinates downloaded with Overpass, as OpenStreetMap is used.

# The following code was used:
# {{geocodeArea: Warszawa }};
# rel(pivot);
# // print results
# out body geom;

# resulting in parsable JSON files.

    poznan <- readLines("D:/WarsawAnalytica/Aplikacja/export_PZN.osm")
    poznan <- poznan[grep("(.*)lat(.*)lon(.*)", poznan)]
    extreme <- poznan[1]
    poznan <- poznan[-c(1,length(poznan))]
    
    strip <- function(x) {
        x <- gsub(pattern = "( *)<nd lat=\\\"", replacement = "", x)
        x <- gsub(pattern = "\\\" lon=\\\"", replacement = ";", x)
        x <- gsub(pattern = "\\\"/>", replacement = "", x)
        x <- strsplit(x, split = ";")
    }
    poznan <- sapply(poznan, strip)
    poznan <- matrix(as.numeric(unlist(poznan)), ncol = 2, byrow = TRUE)
    
    
    # Trial leaflet codes
    centre_y_x <- c(mean(poznan[,1]), mean(poznan[,2]))
    met_deg_y_x <- c(500/111196.672, 500*360/2/pi/6371100/cos(52.2183*pi/180))
    
    poznan2 <- sweep(poznan, 2, centre_y_x)
    poznan2[,1] <- poznan2[,1]/met_deg_y_x[1]
    poznan2[,2] <- poznan2[,2]/met_deg_y_x[2]
    poznan2 <- round(poznan2)
    poznan2 <- unique(poznan2)
    
    # Boundary interpolation
    poznan_aux <- poznan2[1,]
    for (i in 2:nrow(poznan2)) {
        add <- cbind(round(seq(poznan2[i-1,1], poznan2[i,1], length.out = 20)), 
                     round(seq(poznan2[i-1,2], poznan2[i,2], length.out = 20)))
        poznan_aux <- rbind(poznan_aux, add)
    }
    poznan2 <- unique(poznan_aux)
    
    poznan3 <- cbind(poznan2, poznan2) + 1
    poznan3[,3] <- poznan3[,3] - 1
    poznan3[,4] <- poznan3[,4] - 1
    
    poznan3[,1] <- poznan3[,1] * met_deg_y_x[1] + centre_y_x[1]
    poznan3[,2] <- poznan3[,2] * met_deg_y_x[2] + centre_y_x[2]
    poznan3[,3] <- poznan3[,3] * met_deg_y_x[1] + centre_y_x[1]
    poznan3[,4] <- poznan3[,4] * met_deg_y_x[2] + centre_y_x[2]
    
    
    poznan4 <- NULL
    for (i in min(poznan2[,2]):max(poznan2[,2])) {
        add <- poznan2[poznan2[,2] == i,1]
        poznan4 <- rbind(poznan4, cbind(min(add):max(add), rep(i, max(add)-min(add)+1)))
    }
    poznan4 <- cbind(poznan4, poznan4) + 1
    poznan4[,3] <- poznan4[,3] - 1
    poznan4[,4] <- poznan4[,4] - 1
    
    poznan4[,1] <- poznan4[,1] * met_deg_y_x[1] + centre_y_x[1]
    poznan4[,2] <- poznan4[,2] * met_deg_y_x[2] + centre_y_x[2]
    poznan4[,3] <- poznan4[,3] * met_deg_y_x[1] + centre_y_x[1]
    poznan4[,4] <- poznan4[,4] * met_deg_y_x[2] + centre_y_x[2]
    
    leaflet() %>% setView(lat = centre_y_x[1], lng = centre_y_x[2], zoom = 14) %>% 
        addProviderTiles(providers$OpenStreetMap) %>%
        addRectangles(lat1 = poznan3[,1], lng1 = poznan3[,2], lat2 = poznan3[,3], lng2 = poznan3[,4],
                      weight = 2)
    
    leaflet() %>% setView(lat = centre_y_x[1], lng = centre_y_x[2], zoom = 14) %>% 
        addProviderTiles(providers$OpenStreetMap) %>%
        addRectangles(lat1 = poznan4[,1], lng1 = poznan4[,2], lat2 = poznan4[,3], lng2 = poznan4[,4],
                      weight = 2, fill = FALSE)

## Initial analysis
library(dplyr)

setwd("D:/hackaton_maraton_analizy_danych")
adresy <- read.csv("adresy_miejsc.csv")
adresy <- adresy %>% filter(nazwa_miejsc == "POZNAÑ")

# Grid for demo
    demo <- read.csv("adresy_demo_ext_x_y.csv")
    demo <- demo[demo$adr_pk %in% adresy$adr_pk, ]
    demo <- unique(demo)
    grid_coords <- cbind(demo$st_y, demo$st_x)
    grid_coords <- sweep(grid_coords, 2, centre_y_x)
    grid_coords[,1] <- grid_coords[,1]/met_deg_y_x[1]
    grid_coords[,2] <- grid_coords[,2]/met_deg_y_x[2]
    grid_coords <- ceiling(grid_coords)
    demo <- cbind(demo, grid_coords)
    colnames(demo)[34:35] <- c("Y_grid", "X_grid")
    write.csv(x = demo, file = "demo_grid.csv")
    
# Grid for dochod
    dochod <- read.csv("adresy_miejsc_dochod_x_y.csv")
    dochod <- dochod[dochod$adr_pk %in% adresy$adr_pk, ]
    grid_coords <- cbind(dochod$st_y, dochod$st_x)
    grid_coords <- sweep(grid_coords, 2, centre_y_x)
    grid_coords[,1] <- grid_coords[,1]/met_deg_y_x[1]
    grid_coords[,2] <- grid_coords[,2]/met_deg_y_x[2]
    grid_coords <- ceiling(grid_coords)
    dochod <- cbind(dochod, grid_coords)
    colnames(dochod)[26:27] <- c("Y_grid", "X_grid")
    write.csv(x = dochod, file = "dochod_grid.csv")
    
# Grid for POI
    poi <- read.csv("poi_x_y.csv")
    poi <- poi[poi$poi_locality == "POZNAÑ", ]
    grid_coords <- cbind(poi$st_y, poi$st_x)
    grid_coords <- sweep(grid_coords, 2, centre_y_x)
    grid_coords[,1] <- grid_coords[,1]/met_deg_y_x[1]
    grid_coords[,2] <- grid_coords[,2]/met_deg_y_x[2]
    grid_coords <- ceiling(grid_coords)
    poi <- cbind(poi, grid_coords)
    colnames(poi)[30:31] <- c("Y_grid", "X_grid")
    write.csv(x = poi, file = "poi_grid.csv")
    
    
# Grid -> rect
    grid_to_rect <- function(X_grid, Y_grid, centre_x, centre_y, deg_per_tick_x, deg_per_tick_y) {
        return(cbind((X_grid-1)*deg_per_tick_x + centre_x, (Y_grid-1)*deg_per_tick_y + centre_y,
                     X_grid*deg_per_tick_x + centre_x, Y_grid*deg_per_tick_y + centre_y))
    }
    
    
# Semifinal data
    doch_clean <- read.csv("doch_pop_clen.csv")
    poi_clean <- read.csv("poi_grid_clen.csv")
    
    dane <- cbind(doch_clean, poi_clean[match(doch_clean$grid_x, poi_clean$grid),])
    apply(dane, 2, function(x) sum(is.na(x)))     
    dane <- dane %>% select(-c(grid_x.1, grid)) %>% filter(dochod_median != 0)
    colnames(dane)[c(47,52)] <- c("Punkt.uslugowy", "Urzad.i.sluzba.publiczna")
    for (i in 36:53) {
        dane[is.na(dane[,i]),i] <- 0     
    }
    dane$zabudowa[is.na(dane$zabudowa)] <- 0
    
    grid_coords <- matrix(as.numeric(unlist(strsplit(as.character(dane$grid_x), ","))), byrow = TRUE, ncol = 2)
    dane <- cbind(grid_coords, dane)
    colnames(dane)[1:2] <- c("X_grid", "Y_grid")
    write.csv(dane, "dane.csv")

# Final data
    X <- dane %>% transmute(x = X_grid, 
                           y = Y_grid,
                           doch_min = dochod_min,
                           doch_med = dochod_median,
                           doch_max = dochod_max,
                           zabudowa = zabudowa,
                           plec = populacja_k_razem/populacja_razem,
                           wiek_25_34 = (populacja_25_29 + populacja_30_34)/populacja_razem,
                           wiek_35_44 = (populacja_35_39 + populacja_40_44)/populacja_razem,
                           wiek_45_64 = (populacja_45_49 + populacja_50_54 + populacja_55_59 + populacja_60_64)/
                               populacja_razem,
                           dzieci = (populacja_00_04 + populacja_05_09 + populacja_10_14)/populacja_razem,
                           nastolatkowie = populacja_15_19/populacja_razem,
                           studenci = populacja_20_24/populacja_razem,
                           # Dla transportu, sklepów i zdrowia normowanie przez œredni¹ dla niezerowych
                           jedzenie = Fast.Food,
                           kawiarnie = Kawiarnia,
                           trans_pub = Transport/mean(Transport[Transport > 0]),
                           punkty_usl = Punkt.uslugowy,
                           sklep_siec = Sklep.sieciowy/mean(Sklep.sieciowy[Sklep.sieciowy > 0]),
                           edukacja = Edukacja,
                           kultura_rozr = Rozrywka.i.kultura,
                           galerie = Centrum.Handlowe,
                           zdrowie = Zdrowie/mean(Zdrowie[Zdrowie > 0]),
                           miejsca_kultu = Miejsce.kultu)
    X <- X %>% filter(!is.na(X$wiek_25_34))
    write.csv(X, file = "X.csv")
    
# Map grid for dane.csv file
    a <- grid_to_rect(dane$X_grid, dane$Y_grid, centre_x = centre_y_x[2], centre_y = centre_y_x[1], 
                      deg_per_tick_x = met_deg_y_x[2], deg_per_tick_y = met_deg_y_x[1])
    
    leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>% addRectangles(lng1 = a[,1], lat1 = a[,2], 
                                                                              lng2 = a[,3], lat2 = a[,4])
    
    b <- dane %>% filter(dane$populacja_razem < 10)
    b <- grid_to_rect(b$X_grid, b$Y_grid, centre_x = centre_y_x[2], centre_y = centre_y_x[1], 
                      deg_per_tick_x = met_deg_y_x[2], deg_per_tick_y = met_deg_y_x[1])
    
    leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>% addRectangles(lng1 = b[,1], lat1 = b[,2], 
                                                                              lng2 = b[,3], lat2 = b[,4])

# Clustering
    library(ggplot2)
    X_sc <- X
    X_sc[,3:5] <- scale(X_sc[,3:5])
    pca <- prcomp(X_sc)
    
    d <- dist(X_sc)
    hclust <- hclust(d, method = "ward")  
    plot(hclust)
    cut <- cutree(hclust, k = 5)
    
    ggplot(data = data.frame(PC1 = as.matrix(X) %*% pca$rotation[,1], PC2 = as.matrix(X) %*% pca$rotation[,2], 
                             cut =  cut), mapping = aes(x = PC1, y = PC2, col = as.factor(cut))) + geom_point()
    
    kmeans <- kmeans(X_sc, centers = 5, nstart = 25)
    ggplot(data = data.frame(PC1 = as.matrix(X) %*% pca$rotation[,1], PC2 = as.matrix(X) %*% pca$rotation[,2], 
                             cut =  kmeans$cluster), mapping = aes(x = PC1, y = PC2, col = as.factor(cut))) + 
        geom_point()
    
    kmeans_list <- vector(mode = "list", length = 5)
    plots <- vector(mode = "list", length = 5)
    results <- vector(mode = "list", length = 5)
library(factoextra)
for (k in 4:8) {
    kmeans_list[[k-3]] <- kmeans(X_sc, centers = k, nstart = 25)
    plots[[k-3]] <- fviz_cluster(kmeans_list[[k-3]], data = X_sc, palette = "jco")
    
    results[[k-3]] <- X_sc %>% mutate(class = as.factor(kmeans_list[[k-3]]$cluster)) %>% group_by(class) %>%
        summarise(mean_doch = mean(doch_med), mean_zab = mean(zabudowa), ods_kobiet = mean(plec),
                  ods_dzieci = mean(dzieci), ods_mlodzi = mean(wiek_25_34), 
                  ods_emeryci = 1 - mean(dzieci) - mean(nastolatkowie) - mean(studenci) 
                  - mean(wiek_25_34) - mean(wiek_35_44) - mean(wiek_45_64), n = n())
}
    
    
# Exploration
    plots[[1]]
    plots[[2]]
    plots[[3]]
    plots[[4]]
    plots[[5]]
    
    results[[1]]
    results[[2]]
    results[[3]]
    results[[4]]
    results[[5]]
    
# Saving .RData file to deploy
    library(randomForest)
    load("D:/WarsawAnalytica/Aplikacja/modelRF.Rdata")
    
    
    model <- kmeans_list[[2]]
    clusters <- results[[2]]
    clusters$mean_doch <- clusters$mean_doch*sd(X$doch_med) + mean(X$doch_med)
    X_nonsc <- X[,1:5]
    save(list = c("model", "clusters", "X_sc", "grid_to_rect", "X_nonsc"), 
         file = "D:/WarsawAnalytica/Aplikacja/WarsawAnalytica/deploy.RData")
    
    
    save(list = c("plots"), file = "plots.RData")

    
# Otodom
    otodom <- read.csv("otodom.csv")
    