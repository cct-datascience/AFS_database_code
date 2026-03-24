**AFS fish database** 



**Data**



**AFS\_data\_03072026.csv**

Description: most recent cleaned dataset used to calculate book tables. still contains Ontario and Nevada mistakes. 



**AFS\_oldandnew\_data.csv**

Description: original raw data file for all 2009 data and 2020 data. This has not been cleaned for outliers, nonstandard gear, etc. This also contains a lot more data with fish species that do not have Gabelhouse length and weights. 



**outlier\_AFS.csv** 

Description: this has the maximum recorded length and weight for each fish species (x 1.5) and lowest weight and highest and lowest values for CPUE for each method. "Individual fish length and weight data were excluded if a fish fell outside a lower limit of 5 mm and 5 g and an upper limit of 1.5 times their world record length and weight. Additionally, relative weights for individual fish below 30 and above 230 were excluded based on recommendations from practitioners. Finally, effort outliers were set at 120 s (2 min) and 28,800 s (8 h) for electrofishing and 1 and 245 for net-nights to accommodate both short and multiday surveys, respectively." 



There are a number of intermediate datasets labeled with dates in the description. There are also all the raw state data in the state data folder. 



**R code**



These are Erin's original copies of R code, they are potentially out of date with updates Kristina may have made to improve the code



Code is separated by length, weight, cpue and state, ecoregion, and north America but they should essentially function the same. Trout names for weight must be changed (in the code) to be consistent with the FSA package differences in length and weight notation. 



code to get the averages: 

loop\_eco\_CPUE\_sampleI.csv, loop\_eco\_length\_sampleID.csv, loop\_eco\_weight\_sampleID.csv, loop\_northamerica\_CPUE\_sampleID.csv, loop\_northamerica\_length\_sampleID.csv, loop\_northamerica\_weight\_sampleID.csv, loop\_state\_CPUE\_sampleID.csv, loop\_state\_length\_sampleID.csv, loop\_state\_weight\_sampleID.csv



example\_cleaning\_script\_AFS.csv : has example of some of the cleaning of the original data I did and the outlier analysis



