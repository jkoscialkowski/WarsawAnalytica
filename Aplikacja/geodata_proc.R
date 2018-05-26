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
centre_y_x_y_x <- c(mean(poznan[,1]), mean(poznan[,2]))
met_deg_y_x <- c(500/111196.672, 500*360/2/pi/6371100/cos(52.2183*pi/180))

poznan2 <- sweep(poznan, 2, centre_y_x_y_x)
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

poznan3[,1] <- poznan3[,1] * met_deg_y_x[1] + centre_y_x_y_x[1]
poznan3[,2] <- poznan3[,2] * met_deg_y_x[2] + centre_y_x_y_x[2]
poznan3[,3] <- poznan3[,3] * met_deg_y_x[1] + centre_y_x_y_x[1]
poznan3[,4] <- poznan3[,4] * met_deg_y_x[2] + centre_y_x_y_x[2]


poznan4 <- NULL
for (i in min(poznan2[,2]):max(poznan2[,2])) {
    add <- poznan2[poznan2[,2] == i,1]
    poznan4 <- rbind(poznan4, cbind(min(add):max(add), rep(i, max(add)-min(add)+1)))
}
poznan4 <- cbind(poznan4, poznan4) + 1
poznan4[,3] <- poznan4[,3] - 1
poznan4[,4] <- poznan4[,4] - 1

poznan4[,1] <- poznan4[,1] * met_deg_y_x[1] + centre_y_x_y_x[1]
poznan4[,2] <- poznan4[,2] * met_deg_y_x[2] + centre_y_x_y_x[2]
poznan4[,3] <- poznan4[,3] * met_deg_y_x[1] + centre_y_x_y_x[1]
poznan4[,4] <- poznan4[,4] * met_deg_y_x[2] + centre_y_x_y_x[2]



leaflet() %>% setView(lat = centre_y_x_y_x[1], lng = centre_y_x[2], zoom = 14) %>% 
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
    
    
# Grid -> wsp.
    grid_to_rect <- function(X_grid, Y_grid, centre_x, centre_y, deg_per_tick_x, deg_per_tick_y) {
        return(cbind((X_grid-1)*deg_per_tick_x + centre_x, (Y_grid-1)*deg_per_tick_y + centre_y,
                     X_grid*deg_per_tick_x + centre_x, Y_grid*deg_per_tick_y + centre_y))
    }
    
    
# Semifinal data
doch_clean <- read.csv("doch_pop_clen.csv")
poi_clean <- read.csv("poi_grid_clen.csv")

dane <- cbind(doch_clean, poi_clean[match(doch_clean$grid_x, poi_clean$grid),])
apply(dane, 2, function(x) sum(is.na(x)))     
write.csv(dane, "dane.csv")

# Clustering
library(cluster)
daisy