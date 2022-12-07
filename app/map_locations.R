### Read in libraries ###
library(sf)
library(leaflet)

### Read in data ###
# TODO: get location data
metrics <- read.csv('app/Full_results_112122.csv', as.is=T)

ecoregions_url <- "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l1.zip"
ecoregions_path <- "app/ecoregions1.zip"

download.file(ecoregions_url, destfile = ecoregions_path)
unzip(ecoregions_path, exdir = "app/ecoregions1")
ecoregions <- read_sf("app/ecoregions1/NA_CEC_Eco_Level1.shp")

### Plot map with ecoregions ###
ecoregions_crs <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

ecoregions_trans <- ecoregions %>% 
  rmapshaper::ms_simplify() %>% 
  st_set_crs(ecoregions_crs) %>%
  st_transform("+proj=longlat +datum=WGS84")

factpal <- colorFactor(rainbow(length(unique(ecoregions_trans$NA_L1CODE))), 
                       ecoregions_trans$NA_L1CODE)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = ecoregions_trans, color = ~factpal(NA_L1CODE), 
              fillOpacity = 0.5, popup = ~htmltools::htmlEscape(NA_L1NAME), 
              stroke = FALSE)
