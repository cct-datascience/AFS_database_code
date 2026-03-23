library(tidyverse)
library(dplyr)
library(plyr)
library(purrr)
library(FSA)
library(magrittr)
library(data.table)

#data <- read.csv("AFS_fishdata_FINAL_112121.csv")
#data <- read.csv("AZ_test.csv")

#cleaning AFS data, updating Nevada and Ontario
data <- read.csv("C:/Users/eetracy/Desktop/AFS_lengthweight_cleaned_03182026.csv")
data_effort <- read.csv("C:/Users/eetracy/Desktop/AFS_outlier_data_cleaned_052824.csv")
data_2009 <- read.csv("C:/Users/eetracy/Desktop/2009_AFSdata.csv")
outlier <- read.csv("C:/Users/eetracy/Desktop/AFS_database_chapter/outlier_AFS.csv")

#fixing Nevada
data$method[data$state == "Nevada" & data$method == "tow_barge_electrofishing"] <- "boat_electrofishing"
data_effort$method[data_effort$state == "Nevada" & data_effort$method == "tow_barge_electrofishing"] <- "boat_electrofishing"

#Fixing Ontario
#get subset of Ontario data from 2009 
data_2009_ontario <- data_2009 %>%
  filter(state == "Ontario")
# run outlier analysis 
# Join outlier limits to data by common_name for length/weight
# and by method for effort
# Then set values outside limits to NA

data_2009_ontario <- data_2009_ontario %>%
  left_join(outlier %>% select(common_name, upper_length, lower_length, 
                               upper_weight, lower_weight) %>% distinct(), 
            by = "common_name") %>%
  left_join(outlier %>% select(method, upper_effort, lower_effort) %>% distinct(), 
            by = "method") %>%
  mutate(
    # Set length to NA if outside limits
    total_length_mm = ifelse(!is.na(upper_length) & 
                               (total_length_mm > upper_length | 
                                  total_length_mm < lower_length), 
                             NA, total_length_mm),
    # Set weight to NA if outside limits
    weight_g = ifelse(!is.na(upper_weight) & 
                        (weight_g > upper_weight | 
                           weight_g < lower_weight), 
                      NA, weight_g),
    # Set effort to NA if outside limits
    effort = ifelse(!is.na(upper_effort) & 
                      (effort > upper_effort | 
                         effort < lower_effort), 
                    NA, effort)
  ) %>%
  # Remove the outlier limit columns after use
  select(-upper_length, -lower_length, -upper_weight, -lower_weight, 
         -upper_effort, -lower_effort)

# Check how many values were set to NA
sum(is.na(data_2009_ontario$total_length_mm))
sum(is.na(data_2009_ontario$weight_g))
sum(is.na(data_2009_ontario$effort))

# Check how many values were set to NA
#> sum(is.na(data_2009_ontario$total_length_mm))
#[1] 0
#> sum(is.na(data_2009_ontario$weight_g))
#[1] 6775
#> sum(is.na(data_2009_ontario$effort))
#[1] 204

# Step 1: Remove all Ontario data from data
data <- data[data$state != "Ontario", ]

# Step 2: Select and rename relevant columns from data_2009_ontario
ontario_clean <- data_2009_ontario %>%
  select(
    state,
    id,
    common_name,
    method,
    waterbody_type,
    waterbody_name,
    ecoregion_name,
    total_length_mm,
    weight_g,
    effort,
    lat,
    long,
    day,
    month,
    year,
    huc,
    normanID,
    additional_info,
    fish_type,
    ecoregion,
    scientific_name
  )

# Create watername_method_yearID for ontario_clean
ontario_clean <- ontario_clean %>%
  mutate(watername_method_yearID = paste(state, waterbody_name, method, year))

# Check it looks right
ontario_clean %>%
  distinct(watername_method_yearID) %>%
  head(10)
# Calculate count as number of unique sampling events per species/method/waterbody_type
count_lookup <- ontario_clean %>%
  group_by(common_name, method, waterbody_type) %>%
  summarise(count = n_distinct(watername_method_yearID), .groups = "drop")

#adding gcat gabel house lengths
ontario_clean <- ontario_clean %>%
  mutate(gcat = psdAdd(total_length_mm, common_name, verbose = FALSE))

# Check
table(ontario_clean$gcat, useNA = "always")

#substock     stock   quality preferred memorable    trophy      <NA> 
#  3988     20532     21358      4604      1058        48         0 

#deleting substock rows
ontario_clean <- ontario_clean[ontario_clean$gcat != "substock" & !is.na(ontario_clean$gcat), ]

# Drop id column from ontario_clean
ontario_clean$id <- NULL
ontario_clean$ecoregion <- as.integer(stringr::str_extract(ontario_clean$ecoregion_name, "^\\d+"))

data$day <- as.integer(data$day)
data$month <- as.integer(data$month)
data$year <- as.integer(data$year)
data$lat <- as.numeric(data$lat)
data$long <- as.numeric(data$long)
ontario_clean$lat <- as.numeric(ontario_clean$lat)
ontario_clean$long <- as.numeric(ontario_clean$long)

# Now join
data <- bind_rows(data, ontario_clean)

# Check
nrow(data)
unique(data$state) %>% sort()


write.csv(data, "C:/Users/eetracy/Desktop/AFS_lengthweight_cleaned_NV_Ontario_update_03232026.csv")

###############################################################################################
# now joining ontario with effort data
data_effort 

# Remove existing Ontario data from data_effort
data_effort <- data_effort[data_effort$state != "Ontario", ]

# Check what columns need to match
names(data_effort)[!names(data_effort) %in% names(ontario_clean)]
names(ontario_clean)[!names(ontario_clean) %in% names(data_effort)]

data_effort$day <- as.integer(data_effort$day)
data_effort$month <- as.integer(data_effort$month)
data_effort$year <- as.integer(data_effort$year)
data_effort$lat <- as.numeric(data_effort$lat)
data_effort$long <- as.numeric(data_effort$long)
ontario_clean$lat <- as.numeric(ontario_clean$lat)
ontario_clean$long <- as.numeric(ontario_clean$long)

# Join ontario_clean back to data_effort
data_effort <- bind_rows(data_effort, ontario_clean)

# Check
nrow(data_effort)
unique(data_effort$state) %>% sort()

write.csv(data_effort, "C:/Users/eetracy/Desktop/AFS_effort_cleaned_NV_Ontario_update_03232026.csv")
