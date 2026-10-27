# Download state/province boundaries (Natural Earth, public domain) and save a
# lean subset for looking up a state/province from a lat/long coordinate.
# More info: https://www.naturalearthdata.com/downloads/50m-cultural-vectors/

library(sf)
library(dplyr)

states_url  <- "https://naciscdn.org/naturalearth/50m/cultural/ne_50m_admin_1_states_provinces.zip"
states_path <- "app/states1.zip"

if (!file.exists("app/states1/ne_50m_admin_1_states_provinces.shp")) {
  download.file(states_url, destfile = states_path, mode = "wb")
  unzip(states_path, exdir = "app/states1")
  file.remove(states_path)
}

states_raw <- st_read("app/states1/ne_50m_admin_1_states_provinces.shp", quiet = TRUE)

states_lookup <- states_raw %>%
  filter(admin %in% c("United States of America", "Canada", "Mexico")) %>%
  select(name) %>%
  rmapshaper::ms_simplify() %>%
  st_transform("+proj=longlat +datum=WGS84")

saveRDS(states_lookup, "app/states1/na_states_provinces.rds")
