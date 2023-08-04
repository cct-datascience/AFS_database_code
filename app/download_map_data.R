# Download and unzip EPA Ecoregions data
# More info: https://www.epa.gov/eco-research/ecoregions 

ecoregions_url <- "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l1.zip"
ecoregions_path <- "app/ecoregions1.zip"

if(!file.exists("app/ecoregions1/NA_CEC_Eco_Level1.shp")){
  download.file(ecoregions_url, destfile = ecoregions_path)
  unzip(ecoregions_path, exdir = "app/ecoregions1")
  file.remove(ecoregions_path)
}
