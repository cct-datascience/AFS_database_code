library(dplyr)
library(readr)

summarized_data_files <- list.files("analysis_scripts/output_data", pattern = "*.csv")

summarized_data_combined <- c()
for(file in summarized_data_files){
  summarized_data_file <- read_csv(paste0("analysis_scripts/output_data/", file), 
                                          col_types = cols(area = col_character()))
  summarized_data_combined <- bind_rows(summarized_data_combined, summarized_data_file)
}

summarized_data_combined <- summarized_data_combined %>% 
  mutate(common_name = case_when(common_name == "Muskellunge (overall)" ~ "Muskellunge", 
                                 TRUE ~ common_name))

write_csv(summarized_data_combined, "app/standardized_fish_data_03232026.csv")

#todo: compare to previous version of summarized data
prev_sum_data <- read.csv("app/standardized_fish_data.csv")
