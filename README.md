# AFS code for cleaning and analyzing fish data and Shiny webtool
Cleaning data
Packages needed (readxl, janitor, dplyr, plyr, tidyverse, data.table, FSA, qdapTools, stringr)
Examples of cleaning raw AFS fish data went through before analysis. Common cleaning functions include joining individual state and province data together, cleaning column names, removing columns, cleaning data names, joining old and new data, creating new sample ID and summing effort by new sample ID, assigning season to method and filtering out non-standard data (ex. fall boat electrofishing), grouping trout species by lentic and lotic, removing outliers from length, weight, and effort data, adding count data per data set, and adding Gabelhouse categories for PSD calculations using the FSA package. 


Analyzing data
Packages needed (dpylr, plyr, tidyverse, purrr, FSA, magrittr, data.table)
Analysis was split into 9 different scripts. A separate script for length frequency, (length), relative weight (weight) and count per unit effort (CPUE) was created then separate scripts for grouping and filtering by North America, Ecoregion, and State were created. 


Shiny webtool
Packages needed (shiny, tidyverse, data.table, dplyr, rsconnect, rmarkdown)
Code for displaying, filtering, and downloading AFS standard methods fish data
