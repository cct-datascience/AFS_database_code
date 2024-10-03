<br>
  
<center><b>How to Format Input Data</b></center>

You can upload your own data to compare to the standardized data. This needs to be provided as a csv file that will have length and/or weight measurements with one row per observation (a single fish or multiple fish summed). This app will calculate the three metrics of interest for each unique combination of area, species, collection method, type of water body, and year. The three metrics are: 

1. Catch per unit effort (CPUE)
2. Length frequency
3. Relative weight

<center><b>Column Details</b></center>

Below are the details of the required columns in the data. The order of the columns does not matter <b>BUT</b> the column names are <b>case sensitive and must be lower case</b>. Additionally, only upload data from <b>one</b> waterbody sampling effort at a time (under the column `waterbody_name`). There is an example dataset at the bottom of the page with simulated data that may be a helpful guide.

1. CPUE requires `effort` column
2. Length frequency requires `total_length` column 
3. Relative weight requires `weight` and `total_length` columns

Required columns in input dataframe: 

- **Location**: 
  - **Requires** `state` column
    - `state` is name of state where fish were collected, spelled out with first letter capitalized
    - Every row should be the same state (as it's for the same water body)
  - An `ecoregion` column can optionally be included, to compare your data to standardized data in that ecoregion
    - `ecoregion` is name of ecoregion where fish were collected, spelled out and capitalized correctly
    - You can also find your ecoregion by clicking on your location the map on the Explore tab. Ecoregions can also be determined from [EPA ecoregions](https://www.epa.gov/eco-research/ecoregions-north-america) level 1.  Correctly formatted ecoregion names are listed below. 
  - `waterbody_name` is the name of the water body

<style>
.basic-styling td,
.basic-styling th {
  border: 1px solid #999;
  padding: 0.5rem;
}
</style>

<div class='ox-hugo-table basic-styling'>
<div></div>
<div class='table-caption'>
  <span class='table-number'></span>
</div>

| **Ecoregions**                    |
|-----------------------------------|
| 0 Water                           |
| 1 Arctic Cordillera               |
| 2 Tundra                          |
| 3 Taiga                           |
| 4 Hudson Plain                    |
| 5 Northern Forests                |
| 6 Northwestern Forested Mountains |
| 7 Marine West Coast Forest        |
| 8 Eastern Temperate Forests       |
| 9 Great Plains                    |
| 10 North American Deserts         |
| 11 Mediterranean California       |
| 12 Southern Semiarid Highlands    |
| 13 Temperate Sierras              |
| 14 Tropical Dry Forests           |
| 15 Tropical Wet Forests           |

<br>

- **Date**: 
  - `year` is a four-digit numeric
- **Measurements**: 
  - `total_length` is fish record length (mm)
  - `weight` is fish record weight (g)
  - `effort` is specified in **Collection method**
- **Collection method**: see the table below for details
  - `method` must exactly match one of the options in 'Method name'
  - `effort` is the **total** effort of the survey, report number of effort units
      - Examples of effort: 4,556 seconds for electrofishing, 36 net nights for gill net surveys, 5 100-m drifts for drifting trammel net
  - The 'gill_net_spring' method is for gill netting done between January and June (months 1 - 6), and 'gill_net_fall' is for between July and December (months 7 - 12)
  - Data for additional collection methods will be added in the future

<center>

<style>
.basic-styling td,
.basic-styling th {
  border: 1px solid #999;
  padding: 0.5rem;
}
</style>

<div class='ox-hugo-table basic-styling'>
<div></div>
<div class='table-caption'>
  <span class='table-number'></span>
</div>

| **Method**          | **Effort Units** |
|--------------------------|-----------------|
| boat_electrofishing      | Seconds         |
| raft_electrofishing      | Seconds         |
| gill_net_fall            | Net nights      |
| gill_net_spring          | Net nights      |
| drifting_trammel_net     | 100-m drift     |
| large_catfish_hoopnet    | 24 hour set     |
| bag_seine                | 0.25 arc (small_standing_waters); 0.5 arc (rivers) |
| stream_seine             | 10-15 m haul    |
| backpack_electrofishing  | 100 m<sup>2</sup>            |
| tow_barge_electrofishing | 100 m<sup>2</sup>            |

</div>

</center>

- **Type of water body**:
  - `waterbody_type` must exactly match one of the following: *large_standing_waters*, *small_standing_waters*, *two_story_standing_waters*, *wadeable_streams*, *rivers*
- **Species**:
  - `common_name` must **exactly match** one of following species, as from [`FSA::PSDlit`](https://fishr-core-team.github.io/FSA/):

<center>

<style>
.basic-styling td,
.basic-styling th {
  border: 1px solid #999;
  padding: 0.5rem;
}
</style>

<div class='ox-hugo-table basic-styling'>
<div></div>
<div class='table-caption'>
  <span class='table-number'></span>
</div>

| **Species**        | **Species**      | **Species**               |
|--------------------|------------------|---------------------------|
| Arctic Grayling    | Gizzard Shad     | Shorthead Redhorse        |
| Bigmouth Buffalo   | Grass Carp       | Silver Carp               |
| Black Bullhead     | Green Sunfish    | Smallmouth Bass           |
| Black Crappie      | Lake Trout       | Spotted Bass              |
| Blue Catfish       | Largemouth Bass  | Spotted Gar               |
| Bluegill           | Longnose Gar     | Striped Bass (landlocked) |
| Brook Trout        | Muskellunge      | Utah Chub                 |
| Brown Bullhead     | Northern Pike    | Walleye                   |
| Brown Trout        | Paddlefish       | Warmouth                  |
| Bull Trout         | Palmetto Bass    | White Bass                |
| Burbot             | Pumpkinseed      | White Crappie             |
| Chain Pickerel     | Rainbow Trout    | White Perch               |
| Channel Catfish    | Redear Sunfish   | White Sucker              |
| Common Carp        | River Carpsucker | Yellow Bass               |
| Cutthroat Trout    | Rock Bass        | Yellow Bullhead           |
| Flathead Catfish   | Sauger           | Yellow Perch              |
| Freshwater Drum    | Saugeye          |                           |

</div>

</center>  

<br>

<center><b>Example Dataset</b></center>

<br>  