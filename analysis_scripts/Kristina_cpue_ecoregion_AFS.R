library(tidyverse)
library(dplyr)
library(plyr)
library(purrr)
library(FSA)
library(magrittr)
library(reprex)
library(data.table)

#data <- read.csv("C:/Users/etracy1/Desktop/Backup/R_directory/AFS/StandardMethods/AFS_states/AFS_full_data_new_effort_031724.csv")
# 3/17/24 re run for with new effort data
#data <- read.csv("AFS_fishdata_FINAL_112121.csv")
#data <- read.csv("C:/Users/etracy1/Desktop/Backup/R_directory/AFS/StandardMethods/AFS_states/AZ_test.csv")
#data <- read_xlsx("C:/Users/etracy1/Desktop/Backup/R_directory/AFS/StandardMethods/AFS_states/AFS_test_data1.xlsx")
# re-run 3/24/24 
data <- read.csv("C:/Users/eetracy/Desktop/AFS_outlier_data_cleaned_052824.csv")

data <- data %>%
  mutate(common_name = case_when(
    common_name == "Spotted Bass" ~ "Spotted Bass (overall)",
    common_name == "Muskellunge" ~ "Muskellunge (overall)",
    common_name == "Paddlefish" ~ "Paddlefish (overall)",
    common_name == "Walleye" ~ "Walleye (overall)",
    common_name == "Rainbow Trout" & waterbody_type %in% c("small_standing_waters", 
                                                           "large_standing_waters", 
                                                           "two_story_standing_waters") ~ "Rainbow Trout (lentic)",
    common_name == "Rainbow Trout" & waterbody_type %in% c("rivers", 
                                                           "wadeable_streams") ~ "Rainbow Trout (lotic)",
    common_name == "Cutthroat Trout" & waterbody_type %in% c("small_standing_waters", 
                                                             "large_standing_waters", 
                                                             "two_story_standing_waters") ~ "Cutthroat Trout (lentic)",
    common_name == "Cutthroat Trout" & waterbody_type %in% c("rivers", 
                                                             "wadeable_streams") ~ "Cutthroat Trout (lotic)",
    TRUE ~ common_name
  ))

data$weight_g <- as.numeric(as.character(data$weight_g))
data$total_length_mm <- as.numeric(as.character(data$total_length_mm))
data$effort <- as.numeric(as.character(data$effort))
#data$total_m2 <- as.numeric(as.character(data$total_m2))
data$total_m2_sum <- as.numeric(as.character(data$total_m2_sum))
data$count <- as.numeric(as.character(data$count))
`%notin%` <- Negate(`%in%`)

#summarizing by percentile 
p <- c(0.05, 0.25, 0.50, 0.75, 0.95)
p_names <- map_chr(p, ~paste0(.x*100,"%"))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm.= TRUE))%>%
  set_names(nm = p_names)
p_funs
options(scipen=10)

# code for CPUE
data_trial.df <- data.frame(data)
data_trial.df <- data_trial.df[!is.na(data_trial.df$ecoregion),]
data_trial.df <- data_trial.df[!is.na(data_trial.df$waterbody_type),]
data_trial.df <- data_trial.df[(data_trial.df$ecoregion != "NULL"),]
data_trial.df <- data_trial.df[(data_trial.df$ecoregion != 0),]
#I need to remove the Sonora data because its going to remove all 4 data points and error
data_trial.df <- data_trial.df[data_trial.df$ecoregion!=11,]
data_trial.df <- data_trial.df[data_trial.df$ecoregion!=14,]
#data_trial.df1 <- data_trial.df1[data_trial.df1$ecoregion!=5,]

eco.results <- list()

for(s in unique(data_trial.df$ecoregion)){
  state.s <- data_trial.df[data_trial.df$ecoregion==s,]
  state.s <- state.s[!is.na(state.s$common_name),]
  stock.results.CPUE <- list()
  stock.results.CPUE_time <- list()
  
  for(i in unique(state.s$common_name)){
    species.i <- state.s[state.s$common_name==i,]
    results.species.method.CPUE <- list()
    results.species.method.CPUE_time <- list()
    
    for(j in unique(species.i$method)){
      species.i.method.j <- species.i[species.i$method==j,]
      species.method.type.CPUE.results <- list()
      species.method.type.CPUE_time.results <- list()
      
      for(w in unique(species.i.method.j$waterbody_type)){
        species.i.method.j.type.w <- species.i.method.j[species.i.method.j$waterbody_type==w,]
        
        #CPUE for fish/hour
        if("boat_electrofishing" %in% species.i.method.j.type.w$method 
           | "raft_electrofishing" %in% species.i.method.j.type.w$method
           | "small_mesh_trawl" %in% species.i.method.j.type.w$method
           | "large_mesh_trawl" %in% species.i.method.j.type.w$method){
          electro_data <- species.i.method.j.type.w
          electro_data$effort <- electro_data$effort/3600
          electro_data$CPUE <- electro_data$count/electro_data$effort
          
          electro.CPUE <- electro_data%>%
            group_by(common_name, method, waterbody_type, watername_method_yearID)%>%
            summarize_at(vars(CPUE),funs(mean,se), na.rm=TRUE)
          colnames(electro.CPUE)[5] <- "Mean_by_ID"
          
          electro.CPUE <- electro.CPUE[!is.na(electro.CPUE$Mean_by_ID),]
          CPUE_electro_ID <- electro.CPUE%>%
            dplyr::mutate(count=n()) %>%
            group_by(common_name,method,waterbody_type, count)%>%
            summarize_at(vars(Mean_by_ID),funs(!!!p_funs, mean, se), na.rm=TRUE)
          
          CPUE_electro_ID$metric <- "CPUE"
          species.method.type.CPUE.results[[w]] <- as.data.frame(CPUE_electro_ID)
        }
        #CPUE for fish/net night
        if("gill_net_fall" %in% species.i.method.j.type.w$method 
           |"gill_net_spring" %in% species.i.method.j.type.w$method
           | "hoop_net" %in% species.i.method.j.type.w$method 
           | "small_catfish_hoopnet" %in% species.i.method.j.type.w$method  
           | "large_catfish_hoopnet" %in% species.i.method.j.type.w$method
           | "seine" %in% species.i.method.j.type.w$method 
           | "drifting_trammel_net" %in% species.i.method.j.type.w$method
           | "bag_seine" %in% species.i.method.j.type.w$method
           | "stream_seine" %in% species.i.method.j.type.w$method) {
          gill_data <- species.i.method.j.type.w
          gill_data$effort <- as.numeric(as.character(gill_data$effort))
          gill_data$count <- as.numeric(gill_data$count)
          gill_data$CPUE <- gill_data$count/gill_data$effort
          
          gill_data_CPUE <- gill_data%>%
            group_by(common_name, method, waterbody_type, watername_method_yearID)%>%
            summarize_at(vars(CPUE),funs(mean,se), na.rm=TRUE)
          colnames(gill_data_CPUE)[5] <- "Mean_by_ID"
          
          gill_data_CPUE <- gill_data_CPUE[!is.na(gill_data_CPUE$Mean_by_ID),]
          CPUE_gill_ID <- gill_data_CPUE%>%
            dplyr::mutate(count=n()) %>%
            group_by(common_name,method,waterbody_type, count)%>%
            summarize_at(vars(Mean_by_ID),funs(!!!p_funs, mean, se), na.rm=TRUE)
          
          CPUE_gill_ID$metric <- "CPUE"
          species.method.type.CPUE.results[[w]] <- as.data.frame(CPUE_gill_ID) }
        
        # CPUE fish/100m2 and fish/hour pass 1 only
        if("backpack_electrofishing" %in% species.i.method.j.type.w$method
           | "snorkel" %in% species.i.method.j.type.w$method
           | "tow_barge_electrofishing" %in% species.i.method.j.type.w$method) {
          backpack_data <- species.i.method.j.type.w
          backpack_data$total_m2_sum <- as.numeric(as.character(backpack_data$total_m2_sum))
          backpack_data$count <- as.numeric(backpack_data$count)
          
          #number of fish *100/ divided by total disance sampled m2 to convert to fish/100m2
          backpack_data$CPUE_distance <- ((backpack_data$count*100)/backpack_data$total_m2)
          
          #effort converted to hours
          #in some cases both distance and time were reported and will be both be averaged separately
          backpack_data$effort <- backpack_data$effort/3600
          backpack_data$CPUE <- backpack_data$count/backpack_data$effort
          
          backpack_CPUE_distance <- backpack_data%>%
            group_by(common_name, method, waterbody_type, watername_method_yearID)%>%
            summarize_at(vars(CPUE_distance),funs(mean,se), na.rm=TRUE)
          colnames(backpack_CPUE_distance)[5] <- "Mean_by_ID"
          
          backpack_CPUE_distance <- backpack_CPUE_distance[!is.na(backpack_CPUE_distance$Mean_by_ID),]
          CPUE_backpack_ID <- backpack_CPUE_distance%>%
            dplyr::mutate(count=n()) %>%
            group_by(common_name,method,waterbody_type,count)%>%
            summarize_at(vars(Mean_by_ID),funs(!!!p_funs, mean, se), na.rm=TRUE)
          
          CPUE_backpack_ID$metric <- "CPUE_distance"
          species.method.type.CPUE.results[[w]] <- as.data.frame(CPUE_backpack_ID)
          
          ## Now repeat for time
          
          backpack_CPUE_time <- backpack_data%>%
            group_by(common_name, method, waterbody_type, watername_method_yearID)%>%
            summarize_at(vars(CPUE),funs(mean,se), na.rm=TRUE)
          colnames(backpack_CPUE_time)[5] <- "Mean_by_ID"
          
          backpack_CPUE_time <- backpack_CPUE_time[!is.na(backpack_CPUE_time$Mean_by_ID),]
          CPUE_time_backpack_ID <- backpack_CPUE_time%>%
            dplyr::mutate(count=n()) %>%
            group_by(common_name,method,waterbody_type,count)%>%
            summarize_at(vars(Mean_by_ID),funs(!!!p_funs, mean, se), na.rm=TRUE)
          
          CPUE_time_backpack_ID$metric <- "CPUE"
          species.method.type.CPUE_time.results[[w]] <- as.data.frame(CPUE_time_backpack_ID)}
        
      } ## w
      species.method.type.CPUE.results.df <- rbindlist(species.method.type.CPUE.results, idcol = "waterbody_type", fill=TRUE)
      results.species.method.CPUE[[j]] <- species.method.type.CPUE.results.df
      species.method.type.CPUE_time.results.df <- rbindlist(species.method.type.CPUE_time.results, idcol = "waterbody_type", fill=TRUE)
      results.species.method.CPUE_time[[j]] <- species.method.type.CPUE_time.results.df
      
    } ## j
    results.species.method.CPUE.df <- rbindlist(results.species.method.CPUE, idcol = "method", fill=TRUE)
    stock.results.CPUE[[i]] <- results.species.method.CPUE.df
    results.species.method.CPUE_time.df <- rbindlist(results.species.method.CPUE_time, idcol = "method", fill=TRUE)
    stock.results.CPUE_time[[i]] <- results.species.method.CPUE_time.df
    
  } ## i
  CPUE_distance <- rbindlist(stock.results.CPUE, idcol = "common_name",  fill=TRUE)
  CPUE_time <- rbindlist(stock.results.CPUE_time, idcol = "common_name",  fill=TRUE)
  
  eco.s.results <- rbind.fill(CPUE_distance, CPUE_time)
  eco.s.results$ecoregion <- s
  eco.results[[s]] <- eco.s.results
  
} ##s
eco.CPUE.results <- rbindlist(eco.results)

# Assess differences with current summarized data
old_summary_df <- read.csv("C:/Users/eetracy/Desktop/standardized_fish_data.csv")

old_summary_eco <- old_summary_df %>% 
  filter(stringr::str_detect(area, "\\d"), 
         metric == "CPUE") %>% 
  mutate(area = as.numeric(stringr::str_split_i(area, " ", 1)))

names(old_summary_eco)[names(old_summary_eco) == "X5."]  <- "5%"
names(old_summary_eco)[names(old_summary_eco) == "X25."] <- "25%"
names(old_summary_eco)[names(old_summary_eco) == "X50."] <- "50%"
names(old_summary_eco)[names(old_summary_eco) == "X75."] <- "75%"
names(old_summary_eco)[names(old_summary_eco) == "X95."] <- "95%"

new_summary_eco <- eco.CPUE.results %>% 
  mutate(mean = round(mean, 1), 
         se   = round(se, 1),
         `5%`  = round(`5%`, 1),
         `25%` = round(`25%`, 1),
         `50%` = round(`50%`, 1),
         `75%` = round(`75%`, 1),
         `95%` = round(`95%`, 1),
         method         = stringr::str_replace_all(method, "_", " "), 
         waterbody_type = stringr::str_replace_all(waterbody_type, "_", " "), 
         common_name = case_when(
           common_name == "Brown Trout (lotic)"      ~ "Brown Trout",
           common_name == "Brook Trout (lotic)"      ~ "Brook Trout",
           common_name == "Brown Trout (lentic)"     ~ "Brown Trout",
           common_name == "Brook Trout (lentic)"     ~ "Brook Trout",
           common_name == "Rainbow Trout (lotic)"    ~ "Rainbow Trout",
           common_name == "Rainbow Trout (lentic)"   ~ "Rainbow Trout",
           common_name == "Cutthroat Trout (lotic)"  ~ "Cutthroat Trout",
           common_name == "Cutthroat Trout (lentic)" ~ "Cutthroat Trout",
           common_name == "Walleye"                  ~ "Walleye (overall)",
           common_name == "Muskellunge"              ~ "Muskellunge (overall)",
           common_name == "Paddlefish"               ~ "Paddlefish (overall)",
           common_name == "Spotted Bass"             ~ "Spotted Bass (overall)",
           TRUE ~ common_name)) %>% 
  filter(count > 4)
names(new_summary_eco)[names(new_summary_eco) == "ecoregion"] <- "area"

# Join on area as well
# Join on area as well
comp_summary_eco <- full_join(old_summary_eco, new_summary_eco, 
                              by = c("common_name", "method", "waterbody_type", 
                                     "mean", "se", "metric", "area",
                                     "5%", "25%", "50%", "75%", "95%"), 
                              keep = TRUE) 

length(which(is.na(comp_summary_eco$common_name.x)))
length(which(is.na(comp_summary_eco$common_name.y)))

old_only_eco <- comp_summary_eco %>% 
  filter(is.na(common_name.y)) %>% 
  select(contains(".x"), N, area.x) %>%   # N not count, keep area
  rename_with(~ gsub(".x$", "", .x))

new_only_eco <- comp_summary_eco %>% 
  filter(is.na(common_name.x)) %>% 
  select(contains(".y"), count, area.y) %>% 
  rename_with(~ gsub(".y$", "", .x))

no_match_eco <- full_join(old_only_eco, new_only_eco, 
                          by = c("N" = "count", "waterbody_type", 
                                 "common_name", "area"), 
                          keep = TRUE) %>% 
  filter(is.na(common_name.x) | is.na(common_name.y))

length(which(!is.na(no_match_eco$common_name.x)))
length(which(!is.na(no_match_eco$common_name.y)))

old_only_final_eco <- no_match_eco %>% 
  filter(is.na(common_name.y)) %>% 
  select(contains(".x"), N, area.x) %>% 
  rename_with(~ gsub(".x", "", .x))

new_only_final_eco <- no_match_eco %>% 
  filter(is.na(common_name.x)) %>% 
  select(contains(".y"), count, area.y) %>% 
  rename_with(~ gsub(".y", "", .x))
