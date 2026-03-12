#Data cleaning script AFS
library(readxl)
library(janitor)
library(dplyr)
library(tidyverse)
library(qdapTools)
library(data.table)
library(FSA)
library(plyr)
library(stringr)

setwd("C:/Users/etracy1/Desktop/Backup/R_directory/AFS/StandardMethods/AFS_states")
data <- read.csv("AFS_fishdata_FINAL_121222.csv")

#Data was received in individual csvs from each state. and province Janitor is useful in cleaning column names to work in R
AB <- read.csv("Alberta_full.csv")
AB <- janitor::clean_names(AB)
#through
WV <- read.csv ("West_Virginia_full.csv")
WV <- janitor::clean_names(WV)

#combining all files with cleaned names in R
data.all.r <- rbind.fill(AL, AB, AZ, AK, AR, BC, CA, CO, CT, DE, FL, GA, HI, ID, IN, IL, IA, KA, KY, LA, ME, MN, MB, MD, MA, MI, MS, MT, NE, NV, NH, NJ, NY, NM, NC, ND, ON, OH, OK, QC, RI, SA, SO, SC, SD, TN, UT, VA, WA, WV)

#filter out unneeded columns
data <- data.all.r %>%
  dplyr::select(state, id, ecoregion, waterbody_name, fish_type, waterbody_type, month, day, year, method, effort, common_name, total_length_mm, weight_g, lat, long, pass, sample_length_m, sample_width_m, additional_info)
write.csv(data, "AFS_entire_subset.csv", row.names = FALSE)

#how to re-name mistakes that janitor missed such as additional spaces or capitalization
data$state[data$state=="North Carolina "] <- "North Carolina"
data$method[data$method=="Seine"] <- "seine"
data$method[data$method=="large_mesh_trawl"] <- "trawl"
str_to_title(data$common_name)
data$common_name[data$common_name=="Bering cisco"] <- "Bering Cisco"
data$common_name[data$common_name=="Sturgeons "] <- "Sturgeon"

#join new AFS data and 2009 data
setwd()
AFS_2009 <- read.csv("2009_AFSdata.csv")
AFS_total <- rbind.fill (data, AFS_2009)
data <- read.csv("AFS_oldandnew_data.csv")

#data <- read.csv("AFS_oldandnew_gabelhouse_length_subset_data.csv")

#New sampleID creation 
data <- data [-1]
data <- data %>%
  mutate(data, watername_method_yearID = paste(state, waterbody_name, method, year))

#assigning season to method
data3 <- data3 [,-56]

data3 <- data3 %>%
  mutate(data, method_month = paste(method, month, sep=""))
data <- data %>% 
  mutate(season = case_when(
    endsWith(method_month, "boat_electrofishing1" )~ "spring",
    endsWith(method_month, "boat_electrofishing2" )~ "spring",
    endsWith(method_month, "boat_electrofishing3" )~ "spring",
    endsWith(method_month, "boat_electrofishing4" )~ "spring",
    endsWith(method_month, "boat_electrofishing5" )~ "spring",
    endsWith(method_month, "boat_electrofishing6" )~ "spring",
    endsWith(method_month, "boat_electrofishing7" )~ "fall",
    endsWith(method_month, "boat_electrofishing8" )~ "fall",
    endsWith(method_month, "boat_electrofishing9" )~ "fall",
    endsWith(method_month, "boat_electrofishing10" )~ "fall",
    endsWith(method_month, "boat_electrofishing11" )~ "fall",
    endsWith(method_month, "boat_electrofishing12" )~ "fall",
#gillnet follows same code
    endsWith(method_month, "backpack_electrofishing4" )~ "summer",
    endsWith(method_month, "backpack_electrofishing5" )~ "summer",
    endsWith(method_month, "backpack_electrofishing6" )~ "summer",
    endsWith(method_month, "backpack_electrofishing7" )~ "summer",
    endsWith(method_month, "backpack_electrofishing8" )~ "summer",
    endsWith(method_month, "backpack_electrofishing9" )~ "summer",
    endsWith(method_month, "backpack_electrofishing10" )~ "summer",
    
  ))
newid$season <- as.character(newid$season)

#filtering out fall boat electrofishing
data3 <- data3 %>%
  filter(!method_month %in% c("boat_electrofishing7", "boat_electrofishing8", "boat_electrofishing9", "boat_electrofishing10", "boat_electrofishing11", "boat_electrofishing12", "boat_electrofishingNA", "boat_electrofishing98"))

#summing effort by sampleid
newid_test_3 <- data[!is.na(data$effort), ]
newid_test_3 <- newid_test_3[!is.na(newid_test_3$watername_method_yearID), ]
newid_test_3$effort <- as.numeric(as.character(newid_test_3$effort))
newid_test_3 <- newid_test_3[!is.na(newid_test_3$effort), ]

newid_test_4 <- newid_test_3 %>% 
  group_by(date, watername_method_yearID) %>%
  summarize_at(vars(effort), funs(mean))
newid_test_5 <- newid_test_4 %>% 
  group_by(watername_method_yearID) %>%
  summarize_at(vars(effort), funs(sum))
colnames(newid_test_5)[2] <- "effort_new"
colnames(newid_test_5)[1] <- "watername_method_yearID"
new_full <- join(data, newid_test_5, by="watername_method_yearID")
write.csv(data, "AFS_length_step2.csv")

#rename effort summed by year (which is sampleID) as effort and effort per day as effort_day
colnames(data)[12] <- "effort_day"
colnames(data)[56] <- "effort"

data <- read.csv("AFS_length_step2.csv")

#grouping trout into lentic and lotic 
#Brown Trout name change
data_lakes <- data[data$waterbody_type =="small_standing_waters" | data$waterbody_type =="large_standing_waters"| data$waterbody_type =="two_story_standing_waters",]
data_lakes$common_name[data_lakes$common_name=="Brown Trout"] <- "Brown Trout (lentic)"
data_rivers <- data[data$waterbody_type =="rivers" | data$waterbody_type =="wadeable_streams",]
data_rivers$common_name[data_rivers$common_name=="Brown Trout"] <- "Brown Trout (lotic)"
data <- rbind(data_lakes, data_rivers) 

#Rainbow Trout name change
data_lakes <- data[data$waterbody_type =="small_standing_waters" | data$waterbody_type =="large_standing_waters"| data$waterbody_type =="two_story_standing_waters",]
data_lakes$common_name[data_lakes$common_name=="Rainbow Trout"] <- "Rainbow Trout (lentic)"
data_rivers <- data[data$waterbody_type =="rivers" | data$waterbody_type =="wadeable_streams",]
data_rivers$common_name[data_rivers$common_name=="Rainbow Trout"] <- "Rainbow Trout (lotic)"
data <- rbind(data_lakes, data_rivers)

#Brook Trout name change (length separates them into lentic/lotic weight does not)
data_lakes <- data[data$waterbody_type =="small_standing_waters" | data$waterbody_type =="large_standing_waters"| data$waterbody_type =="two_story_standing_waters",]
data_lakes$common_name[data_lakes$common_name=="Brook Trout"] <- "Brook Trout (lentic)"
data_rivers <- data[data$waterbody_type =="rivers" | data$waterbody_type =="wadeable_streams",]
data_rivers$common_name[data_rivers$common_name=="Brook Trout"] <- "Brook Trout (lotic)"
data <- rbind(data_lakes, data_rivers)

write.csv(data, "AFS_length_step3", row.names = FALSE)
data <- step3


#Gabelhouse length subset
data_trial.df <- data.frame(data)
data_trial.df <- data_trial.df[!is.na(data_trial.df$common_name),]
data_trial.df <- data_trial.df[data_trial.df$common_name!=" ",]
data_trial.df <- data_trial.df[data_trial.df$common_name!="",]
data_trial.df <- data_trial.df[!is.na(data_trial.df$state),]
data_trial.df$weight_g <- as.numeric(as.character(data_trial.df$weight_g))
data_trial.df$total_length_mm <- as.numeric(as.character(data_trial.df$total_length_mm))
data_trial.df$effort <- as.numeric(as.character(data_trial.df$effort))
data_trial.df <- data_trial.df %>% mutate(gcat=psdAdd(total_length_mm,common_name, what="incremental"))
data_trial.df <- dplyr::filter(data_trial.df,gcat!="substock") 

write.csv(data, "AFS_length_step4.csv", row.names = FALSE)
data <- data_trial.df

# How to find references for standard length and weight equations 
weight <- WSlit
length <- PSDlit

# Remove outliers, length and weight above the recorded maximum 
#outside a lower limit of 5 mm and 5 g and an upper limit of 1.5 times their world record length and weight. 
#Additionally, relative weights for individual fish below 30 and above 230 were excluded based on recommendations from practitioners. 
#Effort outliers were set at 120 s (2 min) and 28,800 s (8 h) for electrofishing and 1 and 245 for net-nights 

# Load the outlier thresholds
outlier_AFS <- read.csv("path/to/outlier_AFS.csv")

# Join with your data and apply the rules
data_outlier_cleaned <- data %>%
  left_join(outlier_AFS, by = c("common_name", "method")) %>%
  mutate(
    # Length outliers
    total_length_mm = ifelse(total_length_mm < lower_length | 
                               total_length_mm > upper_length, 
                             NA, total_length_mm),
    
    # Weight outliers
    weight_g = ifelse(weight_g < lower_weight | 
                        weight_g > upper_weight, 
                      NA, weight_g),
    
    # Effort outliers
    effort = ifelse(effort < lower_effort | 
                      effort > upper_effort, 
                    NA, effort)
  ) %>%
  # Remove the threshold columns after applying
  select(-upper_length, -upper_weight, -lower_length, -lower_weight, 
         -upper_effort, -lower_effort)


#then do relative weight outlier analysis on finished averages


# add count of how many fish are in each sample ID
data <- add_count(data, common_name, method, waterbody_type, watername_method_yearID)
colnames(data)[59] <- "count"
data$count <- as.numeric(as.character(data$count))
write.csv(data,  "AFS_length_step5.csv", row.names = FALSE)

write.csv(data,  "AFS_fishdata_FINAL_112122.csv", row.names = FALSE)

write.csv(new_full, "AFS_fishdata_FINAL_012423.csv")
data <- read.csv("AFS_fishdata_FINAL_012423.csv")

#Paired down data with only essential columns
data <- data[ ,-c(4, 18, 23, 24, 29, 30, 32, 35, 36, 37, 38, 39, 40, 41, 43, 44, 45, 47, 48, 49, 50, 51, 52)]

write.csv(data, "AFS_fishdata_FINAL_121222.csv", row.names = FALSE)
data <- read.csv("AFS_fishdata_FINAL_121222.csv")

# exclude the variable if it contains the phrase "unnamed"
data <- data[data$waterbody_name!="Unnamed Water",]
data <- data[data$waterbody_name!="UNNAMED",]

#compare final dataset against newly re-run step5
old <- read.csv("AFS_fishdata_FINAL_110721.csv")

#produce lat and longs for map on Shiny app
data2 <- data [ , c(1, 2, 4, 6, 7, 8, 9, 10, 12, 15, 16, 20, 32)]

data2$watername_method_year_commonname_ID <- paste(data2$watername_method_yearID, data2$common_name)
data2$watername_method_year_commonname_typeID <- paste(data2$watername_method_year_commonname_ID, data2$waterbody_type)

test2 <- data2[!duplicated(data2$watername_method_year_commonname_typeID), ]

test1 <- newid_test_1 %>% 
  filter(common_name == "Brown Trout (lotic)") %>% 
  filter(waterbody_type == "wadeable_streams") %>% 
  filter(method == "backpack_electrofishing") %>%
  filter (state == "Colorado")

#fixing trawl name to small/large trawl and re-running effort through outlier analysis to turn 0s to NAs. 
test4 <- filter(newid_test_1, method %in% c("backpack_electrofishing", "boat_electrofishing", "raft_electrofishing", "tow_barge_electrofishing", "trawl", "snorkel"))
test6 <- filter(newid_test_1, method %in% c("seine", "small_mesh_trawl", "stream_seine", "bag_seine", "drifting_trammel_net", "small_catfish_hoopnet", "large_catfish_hoop_net", "hoop_net", "gill_net_spring", "gill_net_fall", "fyke_net" ))
final <- rbind(test5, test7)
test8 <- newid_test_1 %>% 
  filter(method == "trawl") %>% 
  filter(state == "New York")
test8$method[test8$method=="trawl"] <- "small_mesh_trawl"
test9 <- newid_test_1 %>% 
  filter(method == "trawl") %>% 
  filter(state == "Florida")
test9$method[test9$method=="trawl"] <- "large_mesh_trawl"
test10 <- newid_test_1 %>% 
  filter(method == "trawl") %>% 
  filter(state == "Montana")
test10$method[test10$method=="trawl"] <- "large_mesh_trawl"
test11 <- rbind(test8, test9, test10)
test12 <- filter(final, method %in% c("seine", "small_mesh_trawl", "stream_seine", "bag_seine", "drifting_trammel_net", "small_catfish_hoopnet", "large_catfish_hoop_net", "hoop_net", "gill_net_spring", "gill_net_fall", "fyke_net", "backpack_electrofishing", "boat_electrofishing", "raft_electrofishing", "tow_barge_electrofishing", "snorkel" ))
test13 <- rbind(test12, test11)


#subsetteddata2 <- subset(data2, !duplicated(watername_method_yearID))

newid_test_1 <- newid_test_1[-1]
write.csv(test13, "AFS_fishdata_FINAL_012423.csv", row.names = FALSE)
