# Code review notes

Not systematic/collated yet - RMD just working through.

## Session info

```
> sessionInfo()
R version 4.2.1 (2022-06-23)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS Monterey 12.5.1

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices datasets  utils     methods   base     

loaded via a namespace (and not attached):
[1] compiler_4.2.1    tools_4.2.1       rstudioapi_0.15.0 renv_1.0.0       

```

## `renv`

-   On download, `renv` says:

``` 
- Project '~/Documents/CCT/forks/AFS_database_code' loaded. [renv 1.0.0]
- One or more packages recorded in the lockfile are not installed.
- Use `renv::status()` for more details.
```

- Following `renv::restore()`:

```
> renv::status()
The following package(s) are in an inconsistent state:

 package   installed recorded used
 packrat   y         y        n   
 rsconnect y         y        n   

See ?renv::status() for advice on resolving these issues.
```

## Getting the app to run

### Getting needed files

First I had to run `app/download_map_data.R` to get the ecoregions shapefile.

The `app/Lat_long_AFSshiny_012023.csv` data file is not publicly available (endangered species/data distribution concerns). 

I copied the `AZ_test.csv` file from `input_examples` to `app` and changed line 71 of `app.R` to read in `AZ_test`. 
With those changes, the app runs.

## Once the app is running

What are the questions I should ask? What should I be looking for?

### App functionality

_For code review in general - I would define this as a section on "app expected functionality"....and perhaps augmenting with a summary of what is expected to change with new or toy data._

- "Explore" page:
  - "Map" tab
    - Displays a map of points of sampling locations colored by ecoregion. The map zooms and if you click will show you a) the ecoregion or b) the sampling site (if you click on a specific dot). The dots shown on the map will correspond to datasets included in the filters you select. The dots get their lat/long data from the `locs` file, which for review only has one site in AZ.
    - Masks locations if anonymity is not possible [more detail here]
    - Has data filtering options 
  - "Preview" tab
    - Shows data available for download accounting for filters.

### User-facing documentation

### Code documentation

(Probably least important, tbh)