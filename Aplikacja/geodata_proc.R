## Geospatial data processing script

# Warsaw boundary coordinates downloaded with Overpass, as OpenStreetMap is used.

# The following code was used:
# {{geocodeArea: Warszawa }};
# rel(pivot);
# // print results
# out body geom;

# resulting in parsable JSON files.

warsaw <- readLines("D:/WarsawAnalytica pomocnicze/export.osm")
warsaw <- warsaw[grep("(.*)lat(.*)lon(.*)", warsaw)]
extreme <- warsaw[1]
warsaw <- warsaw[-c(1,length(warsaw))]

strip <- function(x) {
    x <- gsub(pattern = "( *)<nd lat=\\\"", replacement = "", x)
    x <- gsub(pattern = "\\\" lon=\\\"", replacement = ";", x)
    x <- gsub(pattern = "\\\"/>", replacement = "", x)
    x <- strsplit(x, split = ";")
}
warsaw <- sapply(warsaw, strip)
warsaw <- matrix(as.numeric(unlist(warsaw)), ncol = 2, byrow = TRUE)


# Trial leaflet codes
centre <- c(52.2183, 21.02557)
met_deg <- c(500/111196.672, 500*360/2/pi/6371100/cos(52.2183*pi/180))

warsaw2 <- sweep(warsaw, 2, centre)
warsaw2[,1] <- warsaw2[,1]/met_deg[1]
warsaw2[,2] <- warsaw2[,2]/met_deg[2]
warsaw2 <- round(warsaw2)
warsaw2 <- unique(warsaw2)

# Boundary interpolation
warsaw_aux <- warsaw2[1,]
for (i in 2:nrow(warsaw2)) {
    add <- cbind(round(seq(warsaw2[i-1,1], warsaw2[i,1], length.out = 20)), 
                 round(seq(warsaw2[i-1,2], warsaw2[i,2], length.out = 20)))
    warsaw_aux <- rbind(warsaw_aux, add)
}
warsaw2 <- unique(warsaw_aux)

warsaw3 <- cbind(warsaw2, warsaw2) + 1
warsaw3[,3] <- warsaw3[,3] - 1
warsaw3[,4] <- warsaw3[,4] - 1

warsaw3[,1] <- warsaw3[,1] * met_deg[1] + centre[1]
warsaw3[,2] <- warsaw3[,2] * met_deg[2] + centre[2]
warsaw3[,3] <- warsaw3[,3] * met_deg[1] + centre[1]
warsaw3[,4] <- warsaw3[,4] * met_deg[2] + centre[2]


warsaw4 <- NULL
for (i in min(warsaw2[,2]):max(warsaw2[,2])) {
    add <- warsaw2[warsaw2[,2] == i,1]
    warsaw4 <- rbind(warsaw4, cbind(min(add):max(add), rep(i, max(add)-min(add)+1)))
}
warsaw4 <- cbind(warsaw4, warsaw4) + 1
warsaw4[,3] <- warsaw4[,3] - 1
warsaw4[,4] <- warsaw4[,4] - 1

warsaw4[,1] <- warsaw4[,1] * met_deg[1] + centre[1]
warsaw4[,2] <- warsaw4[,2] * met_deg[2] + centre[2]
warsaw4[,3] <- warsaw4[,3] * met_deg[1] + centre[1]
warsaw4[,4] <- warsaw4[,4] * met_deg[2] + centre[2]



leaflet() %>% setView(lat = centre[1], lng = centre[2], zoom = 14) %>% 
    addProviderTiles(providers$OpenStreetMap) %>%
    addRectangles(lat1 = warsaw3[,1], lng1 = warsaw3[,2], lat2 = warsaw3[,3], lng2 = warsaw3[,4],
                  weight = 2)


leaflet() %>% setView(lat = centre[1], lng = centre[2], zoom = 14) %>% 
    addProviderTiles(providers$OpenStreetMap) %>%
    addRectangles(lat1 = warsaw4[,1], lng1 = warsaw4[,2], lat2 = warsaw4[,3], lng2 = warsaw4[,4],
                  weight = 2, fill = FALSE)



## API

library(httr)
params <- list(key = "maraton0n895gbsgc72bbksa042mad02", city = "wars", street = "Mysia", building = "3", geocoding = "1", country = "POL", format = "json", charset = "UTF-8")
a <- POST(url = "http://api.locit.pl/webservice/address-hygiene/v2.0.0/?key=maraton0n895gbsgc72bbksa042mad02&city=Warszawa&prefix=ul.&street=Mysia&building=3&geocoding=1&country=POL&format=json&charset=UTF-8")

a <- POST(url = "http://api.locit.pl/webservice/address-hygiene/v2.0.0/", query = params)

library(jsonlite)
b <- content(a, as = "parse")
b
