library(dplyr)
library(readr)

# To generate each summary data file output
# This takes about a half hour to run
# Only rerun if these all need to be regenerated
# summarized_data_scripts <- list.files("analysis_scripts", pattern = "loop_*", full.names = TRUE)
# for(file in summarized_data_scripts){
#   print(file)
#   source(file)
# }

# To put together all summary data files into one file
summarized_data_files <- list.files("analysis_scripts/output_data", pattern = "*.csv")

summarized_data_combined <- c()
for(file in summarized_data_files){
  summarized_data_file <- read_csv(paste0("analysis_scripts/output_data/", file), 
                                          col_types = cols(area = col_character()))
  summarized_data_combined <- bind_rows(summarized_data_combined, summarized_data_file)
}

summarized_data_combined <- summarized_data_combined %>%
  mutate(area = case_when(area == "10" ~ "10 North American Deserts",
                          area == "12" ~ "12 Southern Semi-Arid Highlands",
                          area == "13" ~ "13 Temperate Sierras",
                          area == "15" ~ "15 Tropical Wet Forests Northern Forests",
                          area == "4" ~ "4 Hudson Plain",
                          area == "5" ~ "5 Northern Forests",
                          area == "6" ~ "6 Northwestern Forested Mountains",
                          area == "7" ~ "7 Marine West Coast Forest",
                          area == "8" ~ "8 Eastern Temperate Forests",
                          area == "9" ~ "9 Great Plains",
                          TRUE ~ area)) %>% 
  rename(`5%` = X5., 
         `25%` = X25., 
         `50%` = X50., 
         `75%` = X75., 
         `95%` = X95.)

write_csv(summarized_data_combined, "app/standardized_fish_data_03272026.csv")

prev_sum_data <- read.csv("app/standardized_fish_data.csv")
