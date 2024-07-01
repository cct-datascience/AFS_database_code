# Instructions to run the app locally

1. Fork this repo and clone it to your computer.
1. Open `AFS_database_code.Rproj` in Rstudio.
1. If prompted, run `renv::restore()` to synchronize this project's renv. 
1. Run `app/download_map_data.R` to download the shapefile needed to render the maps.
1. If you do not have access to the standardized fish records location file `app/sites.csv`, toggle the comments at lines 76-88 of `app/app.R` to read in `app/toy_locs.csv` to the object named `locs` instead. This will remove the dots plotted in the map on the "Explore" tab, but will not otherwise affect the app's functionality. 
1. Open `app.R` and select "Run App".
