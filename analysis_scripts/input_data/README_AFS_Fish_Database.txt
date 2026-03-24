# AFS Fish Database README

## Data Files

### Current Clean Data Files
**AFS_effort_cleaned_NV_Ontario_update_03232026.csv**
Description: Most recent cleaned effort dataset. Fixes Nevada tow_barge_electrofishing records (relabeled to boat_electrofishing) and replaces Ontario data with corrected 2009 Ontario dataset (data_2009_ontario). Use this file for CPUE calculations.

**AFS_lengthweight_cleaned_NV_Ontario_update_03232026.csv**
Description: Most recent cleaned length/weight dataset. Same Nevada and Ontario fixes as above. Use this file for length frequency and relative weight calculations.

### Raw Data Files that Produced Exact 2020 Book Averages
**AFS_lengthweight_cleaned_03182026.csv**
Description: Length/weight cleaned dataset that matched book averages exactly for length and relative weight. Contains Nevada and Ontario mistakes. Do not use for new analyses.

**AFS_outlier_data_cleaned_052824.csv**
Description: Effort cleaned dataset that matched book averages exactly for CPUE. Contains Nevada and Ontario mistakes. Do not use for new analyses.

**2009_AFSdata.csv**
Description: Original raw 2009 Ontario data used to replace incorrect Ontario records in the current clean files.

**outlier_AFS.csv**
Description: Outlier limits used for cleaning. Contains maximum recorded length and weight for each species (x 1.5) and lowest/highest CPUE values for each method. Individual fish length and weight data were excluded if a fish fell outside a lower limit of 5 mm and 5 g and an upper limit of 1.5 times their world record length and weight. Additionally, relative weights for individual fish below 30 and above 230 were excluded based on recommendations from practitioners. Effort outliers were set at 120 s (2 min) and 28,800 s (8 h) for electrofishing and 1 and 245 net-nights to accommodate both short and multiday surveys.

### Previous/Intermediate Files
**AFS_data_03072026.csv**
Description: Previous cleaned dataset used to calculate book tables. Still contains Ontario and Nevada mistakes. Superseded by NV_Ontario_update files above.

**AFS_oldandnew_data.csv**
Description: Original raw data file combining 2009 and 2020 data. Has not been cleaned for outliers, nonstandard gear, etc. Contains fish species without Gabelhouse length and weight equations.

---

## Known Data Issues and Cleaning Notes

### Nevada
- Nevada tow_barge_electrofishing records were incorrectly labeled and have been relabeled to boat_electrofishing in the current clean files.

### Ontario
- Original Ontario data contained errors and has been replaced with the corrected 2009 Ontario dataset (data_2009_ontario).
- Ontario data required creation of watername_method_yearID column (state + waterbody_name + method + year), gcat column (Gabelhouse length categories using psdAdd from FSA package), and count column (unique sampling events per species/method/waterbody_type).
- Substock fish were removed from Ontario data prior to joining.

### Species Name Changes
- Several species names were updated to match current FSA R package naming conventions:
  - Rainbow Trout split into Rainbow Trout (lentic) and Rainbow Trout (lotic) based on waterbody type
  - Cutthroat Trout split into Cutthroat Trout (lentic) and Cutthroat Trout (lotic) based on waterbody type
  - Walleye renamed to Walleye (overall)
  - Muskellunge renamed to Muskellunge (overall)
  - Paddlefish renamed to Paddlefish (overall)
  - Spotted Bass renamed to Spotted Bass (overall)

### Method Exclusions
- Boat electrofishing in wadeable streams was excluded as a nonstandard method.
- Effort (time-based) was set to NA for backpack electrofishing and tow barge electrofishing in rivers and wadeable streams since only CPUE by distance (fish/100m2) is appropriate for these methods.

### Rainbow Trout Count Fix
- The count column was missing for Rainbow Trout in the 2024 dataset and was recalculated based on number of fish per sampling event.

### Massachusetts Rainbow Trout
- Approximately 29 Massachusetts Rainbow Trout sampling events are missing waterbody_type and were excluded from the cleaned dataset. These records exist in the raw data but cannot be processed without waterbody_type.

### Why a Single Dataset Matching Length, Weight and Effort Could Not Be Produced
- Length and weight calculations match the book using AFS_lengthweight_cleaned_03182026.csv
- Effort calculations match the book using AFS_outlier_data_cleaned_052824.csv
- These two files were produced from different pipelines at different times and could not be reconciled into a single file that matched book averages for all three metrics simultaneously.
- Root causes: multiple versions saved across folders with inconsistent naming, different scripts run on different dataset versions, FSA package updates changed species names, and post-processing decisions applied to averages tables rather than raw data.

---

## R CodeCode is separated by length, weight, CPUE and by scale (state, ecoregion, North America) but functions essentially the same across scales.

**Analysis scripts:**
loop_eco_CPUE_sampleID.csv, loop_eco_length_sampleID.csv, loop_eco_weight_sampleID.csv, loop_northamerica_CPUE_sampleID.csv, loop_northamerica_length_sampleID.csv, loop_northamerica_weight_sampleID.csv, loop_state_CPUE_sampleID.csv, loop_state_length_sampleID.csv, loop_state_weight_sampleID.csv

**example_cleaning_script_AFS.csv:** Example of data cleaning steps and outlier analysis applied to the original data.

**Note on trout names for weight script:** Trout names must be manually updated in the weight script to match current FSA package naming conventions (e.g. Rainbow Trout (lentic)/(lotic), Cutthroat Trout (lentic)/(lotic)). The length and weight equations are the same for lentic and lotic variants but the FSA package now requires the split names.

**Note on plyr/dplyr conflict:** The plyr package must be loaded after dplyr or it will mask key dplyr functions (rename, summarise, mutate, count, n()). If experiencing issues use dplyr:: prefix explicitly or use base R alternatives.