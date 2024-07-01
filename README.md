[![DOI](https://zenodo.org/badge/581326973.svg)](https://zenodo.org/badge/latestdoi/581326973)

# AFS Standard Fish Data App

The primary purpose of this application is to display data collected across North America on fish species, using a standardized collection approach and resulting comparable metrics by AFS. Additionally, users can upload their own data to compare to the provided standardized data.

Link to the deployed app: [https://viz.datascience.arizona.edu/afs-standard-fish-data/](https://viz.datascience.arizona.edu/afs-standard-fish-data/)

### Repository file organization

**Dashboard**

-   `app/app.R`: file that generates dashboard showing standardized fish data and allows user to upload data to compare
-   `app/standardized_fish_data.csv`: standardized fish data file
-   `app/R/functions.R`: functions to calculate three metrics of interest
-   `app/www/AFS_sponsor_3.png`: image file with sponsor logo displayed on dashboard "About" page
- `app/example_user_upload_data.csv`: simulated example data to show how user uploaded datasets should be formatted

**Data prep**

-   `app/process_user_data.Rmd`: generates example user upload data (`user_example.csv`) shown in app; shows development of metric calculations
-   `app/user_example.csv`: example user upload data
-   `app/download_map_data.R`: code to download the [EPA Ecoregions](https://www.epa.gov/eco-research/ecoregions) data used in app map
-   `analysis_scripts`: folder with scripts to prepare standardized data; newer version of this is in `app/R/functions.R`
-   `input_examples`: folder containing additional user upload example datasets

**Package versions & dependences**

-   `renv` folder
-   `renv.lock`
-   `.Rprofile`

**Repository metadata**

-   `.gitignore`
-   `AFS_database_code.Rproj`
-   `LICENSE`
-   `README.md`
-   `CITATION.cff`

### How to cite

Please use the citation below if you use or modify this tool for research purposes.

> Tracy, E., Guo, J., Riemer, K., & Bonar, S. (2023). Code for "AFS Standard Fish Data App" (Version 1.0.0) [Computer software]. https://doi.org/10.5281/zenodo.8169922

### How to contribute

If you would like to suggest or make changes to this app, there are a few ways to do so. You can reach out via email to [Scott Bonar](mailto:SBonar@ag.arizona.edu) or the [CCT Data Science team](mailto:cct-datascience@arizona.edu) with suggestions.You can also create an issue describing a problem with the code or improvements under the "Issues" tab.

If you can make changes to the code yourself, feel free to fork this repo and make a pull request. To run the code locally, follow the instructions in `run_locally.md`. It will be necessary to download the ecoregions map data by running `app/download_map_data.R`. Additionally, running the app locally requires the standardized fish records location data file `app/sites.csv`, which is not available in the repo due to sensitive information. Package versions and dependencies are tracked with `renv`. 
