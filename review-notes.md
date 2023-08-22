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
    - [x] Displays a map of points of sampling locations colored by ecoregion. The map zooms and if you click will show you a) the ecoregion or b) the sampling site (if you click on a specific dot). The dots shown on the map will correspond to datasets included in the filters you select. The dots get their lat/long data from the `locs` file, which for review only has one site in AZ.
    - _Can't test with current toy data; could make a dataset designed to test it_. Masks locations if anonymity is not possible [more detail here]
    - [x] Has data filtering options that control which dots/locations show up on the map.
  - "Preview" tab
    - [x] Shows the filtered data that would be available for download given current filters. _Could try to break this filtering but it works appropriately given some 
  - "Download filtered" and "Download all" buttons
    - [x] Download filtered data/all data.
- "View" page:
  - Shows plots of fish metrics
  - [x] Plots update based on data filtration options
  - [x] May show 'No [] data for selected options' 
- "Compare your data" tab
  - "Instructions"
    - [x] Instructions for formatting a dataset so that it can be uploaded to compare to the in-app data
      - _I tested this using the supplied "user_example.csv"; I could make a new dataset following instructions to make sure that that works_
  - "Comparisons"
    - [x] Shows comparison plots - user data show up in different colors than corresponding in-app data
    - [x] If user data don't meet needs in Instructions, plots fail to render with various errors.

### User-facing documentation

- [ ] X axes for the plots - I don't know what these mean ("S-Q", "Q-P", "P-M", M-T", "T"). That might be standard fish stuff, though, and not problematic for the intended user base.
- [ ] Instructions for uploading data are sufficient to get comparisons plots to render
- [x] It's clear where to get help
- [ ] License/attribution instructions for downloaded data in the app itself?

### Code documentation

What are we looking for in code documentation for an app?

- [ ] File structure is explained
- [ ] It's possible to run the code locally as-is
      - _Not currently, RMD can contribute some short files/instructions for running locally. My current impression is that the only thing that breaks if you have fake data for the lat/longs is the map on the Explore page._
- [ ] Files that are present have a clear purpose
      - _Are the `analysis_scripts` and `input_examples` directories needed in the current iteration, or are they notetaking/archival? If so they're not hurting anything, just a clarity/tidying up thing._
- [ ] Scripts that are used are documented and readable
      - _To my understanding, the ones to focus on are:_
      - [ ] `app/r/functions.R`
      - [ ] `app/app.R`


## Suggestions/comments (so far)

- License/citation on the "About" page of the app
- Explanation of X axes on plots - see:

```r  
gcat = case_when(gcat == "stock" ~ "S-Q",
                              gcat == "quality" ~ "Q-P",
                              gcat == "preferred" ~ "P-M",
                              gcat == "memorable" ~ "M-T",
                              gcat == "trophy" ~ "T")
```
- Doc of instructions + necessary files to get the app to run locally
    - _I am happy to take a go at this._