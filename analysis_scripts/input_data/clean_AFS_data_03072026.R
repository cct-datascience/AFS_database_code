# AFS_data_03072026.csv is the raw data that is what went into the second edition book
# Still needs a few cleaning steps before running through analysis_scripts to get metrics
# See email from Scott Bonar 3/7/2026 for details

library(readr)
library(dplyr)

raw_data <- read_csv("analysis_scripts/input_data/AFS_data_03072026.csv")

ontario_raw_data <- raw_data %>% 
  filter(state == "Ontario", 
         year < 2009, 
         count >= 30)

retained_data <- raw_data %>% 
  filter(#watername_method_yearID != "Massachusetts E. BRIMFIELD RESERVOIR (LONG POND) bag_seine 1999", 
         #watername_method_yearID != "Massachusetts West End Pond bag_seine 2000", 
         #watername_method_yearID != "New York Hempstead Lake bag_seine 2005", 
         !is.na(waterbody_type), 
         state != "Ontario") %>% 
  mutate(waterbody_type = case_when(state == "Connecticut" & waterbody_name == "Highland Lake" & waterbody_type == "two_story_standing_waters" ~ "small_standing_waters",
                                    state == "Connecticut" & waterbody_name == "East Twin Lake (Washinee Lake)" & waterbody_type == "two_story_standing_waters" ~ "large_standing_waters",
                                    state == "Connecticut" & waterbody_name == "Long Pond" & waterbody_type == "two_story_standing_waters" ~ "small_standing_waters",
                                    state == "Connecticut" & waterbody_name == "West Hill Pond" & waterbody_type == "two_story_standing_waters" ~ "small_standing_waters", 
                                    TRUE ~ waterbody_type), 
         method = case_when(state == "Nevada" & waterbody_name == "South Fork Reservoir" & method == "tow_barge_electrofishing" ~ "boat_electrofishing",
                            state == "Nevada" & waterbody_name == "South Fork Reservoir Spillway Salvage" & method == "tow_barge_electrofishing" ~ "boat_electrofishing", 
                            state == "Nevada" & waterbody_name == "Wildhorse Reservoir" & method == "tow_barge_electrofishing" ~ "boat_electrofishing", 
                            TRUE ~ method), 
         ecoregion = case_when(ecoregion == 0 ~ NA, 
                               TRUE ~ ecoregion), 
         ecoregion_name = case_when(ecoregion_name == "0 - None" ~ NA,
                                    ecoregion_name == "NULL" ~ NA, 
                               TRUE ~ ecoregion_name), 
         common_name = case_when(common_name == "Cutthroat Trout" & waterbody_type %in% c("wadeable_streams", "rivers") ~ "Cutthroat Trout (lotic)", 
                                 common_name == "Cutthroat Trout" & waterbody_type %in% c("large_standing_waters", "small_standing_waters") ~ "Cutthroat Trout (lentic)", 
                                 common_name == "Rainbow Trout" & waterbody_type %in% c("wadeable_streams", "rivers") ~ "Rainbow Trout (lotic)", 
                                 common_name == "Rainbow Trout" & waterbody_type %in% c("large_standing_waters", "small_standing_waters") ~ "Rainbow Trout (lentic)", 
                                 TRUE ~ common_name)) %>% 
  bind_rows(ontario_raw_data)

write_csv(retained_data, "analysis_scripts/input_data/cleaned_AFS_data_03112026.csv")

ontario_removed_data <- raw_data %>% 
  filter(state == "Ontario", 
         year > 2009 | year < 2009 & count < 30)

removed_data <- raw_data %>% 
  filter(#watername_method_yearID == "Massachusetts E. BRIMFIELD RESERVOIR (LONG POND) bag_seine 1999" |
         #watername_method_yearID == "Massachusetts West End Pond bag_seine 2000" |
         #watername_method_yearID == "New York Hempstead Lake bag_seine 2005" | 
         is.na(waterbody_type)) %>% 
  bind_rows(ontario_removed_data)

#write_csv(removed_data, "analysis_scripts/input_data/removed_AFS_data_03092026.csv")
