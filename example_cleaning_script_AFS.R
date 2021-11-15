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

setwd()

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

#New sampleID creation 
tempid <- c("state", "waterbody_name", "method", "year")
newid$watername_method_yearID <-do.call(paste,c(newid[tempid], sep=""))
unique(newid$watername_method_yearID)

#summing effort by sampleid
newid_test_3 <- newid_test_2[!is.na(newid_test_2$effort), ]
newid_test_3 <- newid_test_3[!is.na(newid_test_3$watername_method_yearID), ]

newid_test_4 <- newid_test_3 %>% 
  group_by(date, watername_method_yearID) %>%
  summarize_at(vars(effort), funs(mean))
newid_test_5 <- newid_test_4 %>% 
  group_by(watername_method_yearID) %>%
  summarize_at(vars(effort), funs(sum))
colnames(newid_test_5)[2] <- "effort_new"
colnames(newid_test_5)[1] <- "watername_method_yearID"
new_full <- join(newid_test_2, newid_test_5, by="watername_method_yearID")

#assigning season to method
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
data <- data %>%
  filter(!method_month %in% c("boat_electrofishing7", "boat_electrofishing8", "boat_electrofishing9", "boat_electrofishing10", "boat_electrofishing11", "boat_electrofishing12"))

#grouping trout into lentic and lotic 
#Brown Trout name change
data_lakes <- data_trial.df[data_trial.df$waterbody_type =="small_standing_waters" | data_trial.df$waterbody_type =="large_standing_waters",]
data_lakes$common_name[data_lakes$common_name=="Brown Trout"] <- "Brown Trout (lentic)"
data_rivers <- data_trial.df[data_trial.df$waterbody_type =="rivers" | data_trial.df$waterbody_type =="wadeable_streams",]
data_rivers$common_name[data_rivers$common_name=="Brown Trout"] <- "Brown Trout (lotic)"
data_trial.df <- rbind(data_lakes, data_rivers) 
#Also includes Cutthroat and Rainbow name change

# Remove outliers in r using 3 as the outer fence limit eliminating extreme outliers

# length and weight outliers eliminated
species.i.results <- list()

for(i in unique(newid_test$common_name)){
  species.i <- newid_test[newid_test$common_name==i,]
  
  Q <- quantile(as.numeric(species.i$total_length_mm), probs=c(.25, .75), na.rm = TRUE)
  iqr <- IQR(species.i$total_length_mm, na.rm = TRUE)
  up <- Q[2] + 3*iqr
  species.i$total_length_mm[species.i$total_length_mm > up ] <- NA
  
  Q.weight <- quantile(as.numeric(species.i$weight_g), probs=c(.25, .75), na.rm = TRUE)
  iqr.weight <- IQR(species.i$weight_g, na.rm = TRUE)
  up.weight <- Q.weight[2] + 3*iqr.weight
  species.i$weight_g[species.i$weight_g > up.weight ] <- NA
  low.weight <- 1.9
  species.i$weight_g[species.i$weight_g < low.weight ] <- NA
  
  species.i.results [[i]] <- species.i
  
} #i

species.i.results.df <- rbindlist(species.i.results, fill=TRUE)
newid_test_1 <- species.i.results.df
#effort follows same code

# add count of data sets
data <- add_count(data, common_name, waterbody_type, method, watername_method_yearID)
data <- add_count(data, watername_method_yearID)
colnames(data)[25] <- "count"
data$count <- as.numeric(as.character(data$count))
write.csv(data,  "name", row.names = FALSE)

#Add Gabelhouse categories for PSD calculations using FSA package
data_browntrout %<>% mutate(gcat=psdAdd(total_length_mm,common_name, what="incremental"))
data_browntrout_test <- filterD(data_browntrout,gcat!="substock") 
write.csv(data_browntrout, "name")

# How to find references for standard length and weight equations 
weight <- WSlit
length <- PSDlit

write.csv(length, "length_table_Gabelhouse.csv")
write.csv(weight, "weight_table_reference.csv")
