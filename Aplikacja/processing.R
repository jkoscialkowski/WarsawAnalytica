    
## Czyste
poi_clean <- read.csv("poi_grid_clen.csv")
a <- matrix(as.numeric(unlist(strsplit(as.character(poi_clean$grid), split = ","))), byrow = TRUE, ncol = 2)
a <- cbind(a, poi_clean)

    
## Miara zadowolenia
adresy_demo <- read.csv(file = "D:/hackaton_maraton_analizy_danych/adresy_demo_ext.csv")

adresy_mieszk <- adresy_demo[adresy_demo$msw_2012_lokale > 0 & adresy_demo$msw_2015_lokale > 0,]

nrow(adresy_demo)
sum(adresy_mieszk$msw_2016_lokale/adresy_mieszk$msw_2012_lokale < adresy_mieszk$msw_2016_popul_razem/adresy_mieszk$msw_2012_popul_razem)




## IAHU
iahu <- read.csv("IAHU_x_y.csv")
iahu2 <- iahu[sample(1:20000, 500),]
leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>% addRectangles(lng1 = iahu2$st_xmin, lng2 = iahu2$st_xmax, lat1 = iahu2$st_ymin, lat2 = iahu2$st_ymax, opacity = iahu2$iahu/max(iahu2$iahu))


## API

library(httr)
params <- list(key = "maraton0n895gbsgc72bbksa042mad02", city = "warszawa", prefix = "ul.", street = "Mysia", building = "3", geocoding = "1", country = "POL", format = "json", charset = "UTF-8")
a <- POST(url = "http://api.locit.pl/webservice/address-hygiene/v2.0.0/?key=maraton0n895gbsgc72bbksa042mad02&city=Warszawa&prefix=ul.&street=Mysia&building=3&geocoding=1&country=POL&format=json&charset=UTF-8")

a <- POST(url = "http://api.locit.pl/webservice/address-hygiene/v2.0.0/", query = params)

library(jsonlite)
b <- content(a, as = "parse")
b


clean <- POST(url = "http://api.locit.pl/webservice/address-autocomplete/v2.0.0/", query = list(key = "maraton0n895gbsgc72bbksa042mad02", schema = "basic", query = "wars, olszewska 7", format = "json", charset = "UTF-8"))
