library(readr)
library(dplyr)

current_raw_data <- read_csv("analysis_scripts/input_data/AFS_final_01132026.csv")

# Add pre-2009 Ontario data back into raw data
pre_2009_data <- read_csv("analysis_scripts/input_data/2009_AFSdata.csv")
pre_2009_ontario <- pre_2009_data %>% 
  filter(state == "Ontario") %>% 
  rename(state = state, 
         ecoregion = ecoregion, 
         ecoregion_name = ecoregion_name, 
         waterbody_name = waterbody_name, 
         fish_type = fish_type, 
         waterbody_type = waterbody_type, 
         month = month, 
         day = day, 
         year = year, 
         method = method, 
         #effort_day 
         common_name = common_name, 
         total_length_mm = total_length_mm, 
         weight_g = weight_g, 
         lat = lat, 
         long = long, 
         #sample_length_m 
         #sample_width_m
         additional_info = additional_info, 
         date = date, 
         # transect_net, 
         # effort_transect, 
         # effort_metric, 
         # method_info, 
         # effort_hours, 
         # gear_description, 
         # total_m2, 
         # season, 
         # age, 
         huc = huc, 
         normanID = normanID, 
         #watername_method_yearID - created in example_cleaning_script_AFS.R
         effort_old = effort, 
         #gcat, 
         #count - created in example_cleaning_script_AFS.R
         #total_m2_sum, 
         #effort_transect_new, 
         # effort_day_new, 
         # effort_old1, 
         # pass, 
         scientific_name = scientific_name, 
         effort = effort
         ) %>% 
  mutate(method_month = paste0(method, month), 
         watername_method_yearID = paste(state, waterbody_name, method, year)) %>% 
  group_by(common_name, waterbody_type, method, watername_method_yearID) %>% 
  mutate(count = n()) %>% 
  ungroup()


# Nevada



# save out new raw data
