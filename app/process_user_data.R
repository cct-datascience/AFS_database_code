library(readxl)
library(dplyr)
library(FSA)

# Read in user upload data examples
raw_example <- readxl::read_xlsx("input_examples/AFS_test_data.xlsx")
process_example <- read.csv("input_examples/AFS_processed_test_data.csv")

# Process
# multiple rows for "individual" fish summarized into one row by area, 
# species, method, waterbody type

# Variables
# state/ecoregion: user provides one column with North America, full state name, 
# or ecoregion name; state and/or ecoregion columns?
# we put into "area" column

# species: need column of common names as spelled in FSA table function PSDlit

# Start with length frequency, needs length and species name

test <- raw_example %>% 
  select(state, common_name, method, waterbody_type, total_length) %>%  #also by year? 
  mutate(common_name = stringr::str_to_title(common_name)) %>% 
  mutate(gcat = psdAdd(total_length, common_name, what = "incremental")) %>% 
  group_by(state, common_name, method, waterbody_type) %>% 
  count(gcat) %>% 
  mutate(freq = n / sum(n))
  # xtabs(~common_name+gcat, data = .) %>% 
  # prop.table(., margin = 1) %>% 
  # as.data.frame() %>% 
  # mutate(Freq = Freq * 100)
# change categories into factor with all 5

single_ex <- raw_example %>% 
  select(state, common_name, method, waterbody_type, total_length) %>%
  filter(common_name == "largemouth bass")

species.i.method.j.type.w.id.u <- single_ex %>% 
  mutate(common_name = stringr::str_to_title(common_name)) %>% 
  mutate(gcat=psdAdd(total_length,common_name, what="incremental"))
freq <- xtabs(~common_name+gcat, data = species.i.method.j.type.w.id.u)
psd <- prop.table(freq,margin=1)*100
psd <-as.data.frame(psd)
psd$gcat <- as.character(psd$gcat)
