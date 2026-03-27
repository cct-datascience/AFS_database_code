library(tidyverse)
library(dplyr)
library(plyr)
library(purrr)
library(FSA)
library(magrittr)
library(data.table)

data <- read.csv("analysis_scripts/input_data/AFS_effort_cleaned_NV_Ontario_update_03232026.csv")

# I just did a find replace of total_m2 with total_m2_sum 

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

# code for length-frequency, relative weight and CPUE (boat electrofishing) in loop
data_trial.df <- data.frame(data)
data_trial.df <- data_trial.df[!is.na(data_trial.df$waterbody_type),]
data_trial.df <- data_trial.df %>% 
  mutate(effort = case_when(method == "backpack_electrofishing" & waterbody_type == "wadeable_streams" ~ NA, 
                            method == "backpack_electrofishing" & waterbody_type == "rivers" ~ NA, 
                            method == "tow_barge_electrofishing" & waterbody_type == "wadeable_streams" ~ NA, 
                            method == "tow_barge_electrofishing" & waterbody_type == "rivers" ~ NA, 
                            TRUE ~ effort))

stock.results.CPUE <- list()
stock.results.CPUE_time <- list()

for(i in unique(data_trial.df$common_name)){
  species.i <- data_trial.df[data_trial.df$common_name==i,]
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
        #effort gets converted from seconds to hours
        electro_data$effort <- electro_data$effort/3600
        #fish count per sampleID gets divided by effort which had previously been summed over multiple day for sampleID
        electro_data$CPUE <- electro_data$count/electro_data$effort
        
       #data must be grouped by common name, method, waterbody type and ID to ensure its calculated for example for a bluegill (common name) by boat electrofishing (method) in a large lake (water type) in one sampling period (ID)
        #mean cpue by ID for only electrofishing
        electro.CPUE <- electro_data%>%
          group_by(common_name, method, waterbody_type, watername_method_yearID)%>%
          summarize_at(vars(CPUE),list(mean = mean, se = se), na.rm=TRUE)
        colnames(electro.CPUE)[5] <- "Mean_by_ID"
        
        #not sure if this is needed? Seems to be creating N count of rows in a column
        electro.CPUE <- electro.CPUE[!is.na(electro.CPUE$Mean_by_ID),]
        CPUE_electro_ID <- electro.CPUE%>%
          dplyr::mutate(count=n()) %>%
          group_by(common_name, method, waterbody_type, count)%>%
          summarize_at(vars(Mean_by_ID),funs(!!!p_funs, mean, se), na.rm=TRUE)
        
        CPUE_electro_ID$metric <- "CPUE"
        species.method.type.CPUE.results[[w]] <- as.data.frame(CPUE_electro_ID)
      }
      #CPUE for fish/net night 
      #effort in this case is often 1 net night instead of hours
      if("gill_net_fall" %in% species.i.method.j.type.w$method  
         |"gill_net_spring" %in% species.i.method.j.type.w$method 
         | "hoop_net" %in% species.i.method.j.type.w$method 
         | "small_catfish_hoopnet" %in% species.i.method.j.type.w$method  
         | "large_catfish_hoopnet" %in% species.i.method.j.type.w$method
         | "seine" %in% species.i.method.j.type.w$method 
         | "bag_seine" %in% species.i.method.j.type.w$method
         | "drifting_trammel_net" %in% species.i.method.j.type.w$method
         | "stream_seine" %in% species.i.method.j.type.w$method) {
        gill_data <- species.i.method.j.type.w
        gill_data$effort <- as.numeric(as.character(gill_data$effort))
        gill_data$count <- as.numeric(gill_data$count)
        gill_data$CPUE <- gill_data$count/gill_data$effort
        
        gill_data_CPUE <- gill_data%>%
          group_by(common_name, method, waterbody_type, watername_method_yearID)%>%
          summarize_at(vars(CPUE),list(mean = mean, se = se), na.rm=TRUE)
        colnames(gill_data_CPUE)[5] <- "Mean_by_ID"
        
        gill_data_CPUE <- gill_data_CPUE[!is.na(gill_data_CPUE$Mean_by_ID),]
        CPUE_gill_ID <- gill_data_CPUE%>%
          dplyr::mutate(count=n()) %>%
          group_by(common_name,method,waterbody_type, count)%>%
          summarize_at(vars(Mean_by_ID),funs(!!!p_funs, mean, se), na.rm=TRUE)
        
        CPUE_gill_ID$metric <- "CPUE"
        species.method.type.CPUE.results[[w]] <- as.data.frame(CPUE_gill_ID) }
      
      #CPUE fish/100m2 and fish/hour pass 1 only
      #total_m2_sum has been calculated from multiplying sample width x length
      if("backpack_electrofishing" %in% species.i.method.j.type.w$method
         | "tow_barge_electrofishing" %in% species.i.method.j.type.w$method
         | "snorkel" %in% species.i.method.j.type.w$method) {
        backpack_data <- species.i.method.j.type.w
        backpack_data$total_m2_sum <- as.numeric(as.character(backpack_data$total_m2_sum))
        backpack_data$count <- as.numeric(backpack_data$count)
        
        #number of fish *100/ divided by total disance sampled m2 to convert to fish/100m2
        backpack_data$CPUE_distance <- ((backpack_data$count*100)/backpack_data$total_m2_sum)
        
        #effort converted to hours
        #in some cases both distance and time were reported and will both be averaged separately
        backpack_data$effort <- backpack_data$effort/3600
        backpack_data$CPUE <- backpack_data$count/backpack_data$effort
        
        backpack_CPUE_distance <- backpack_data%>%
          group_by(common_name, method, waterbody_type, watername_method_yearID)%>%
          summarize_at(vars(CPUE_distance),list(mean = mean, se = se), na.rm=TRUE)
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
          summarize_at(vars(CPUE),list(mean = mean, se = se), na.rm=TRUE)
        colnames(backpack_CPUE_time)[5] <- "Mean_by_ID"
        
        backpack_CPUE_time <- backpack_CPUE_time[!is.na(backpack_CPUE_time$Mean_by_ID),]
        CPUE_time_backpack_ID <- backpack_CPUE_time%>%
          dplyr::mutate(count=n()) %>%
          group_by(common_name,method,waterbody_type,count)%>%
          summarize_at(vars(Mean_by_ID),funs(!!!p_funs, mean, se), na.rm=TRUE)
        
        CPUE_time_backpack_ID$metric <- "CPUE"
        species.method.type.CPUE_time.results[[w]] <- as.data.frame(CPUE_time_backpack_ID)}
      
    } ##w
    species.method.type.CPUE.results.df <- rbindlist(species.method.type.CPUE.results, idcol = "waterbody_type", fill=TRUE)
    results.species.method.CPUE[[j]] <- species.method.type.CPUE.results.df
    species.method.type.CPUE_time.results.df <- rbindlist(species.method.type.CPUE_time.results, idcol = "waterbody_type", fill=TRUE)
    results.species.method.CPUE_time[[j]] <- species.method.type.CPUE_time.results.df
  
    } ##j
  results.species.method.CPUE.df <- rbindlist(results.species.method.CPUE, idcol = "method", fill=TRUE)
  stock.results.CPUE[[i]] <- results.species.method.CPUE.df
  results.species.method.CPUE_time.df <- rbindlist(results.species.method.CPUE_time, idcol = "method", fill=TRUE)
  stock.results.CPUE_time[[i]] <- results.species.method.CPUE_time.df

}

CPUE.all <- rbindlist(stock.results.CPUE)
CPUE_time.all <- rbindlist(stock.results.CPUE_time)

NorthAmerica.results.CPUE <- rbind.fill(CPUE.all, CPUE_time.all)

# Clean up format of summarized data and save out
new_summary_df <- NorthAmerica.results.CPUE %>% 
  mutate(mean = round(mean, 1), 
         se = round(se, 1), 
         `5%` = round(`5%`, 1),
         `25%` = round(`25%`, 1),
         `50%` = round(`50%`, 1),
         `75%` = round(`75%`, 1),
         `95%` = round(`95%`, 1),
         method = stringr::str_replace_all(method, "_", " "), 
         waterbody_type = stringr::str_replace_all(waterbody_type, "_", " "), 
         metric = stringr::str_replace_all(metric, "_", " "), 
         common_name = case_when(common_name == "Brown Trout (lotic)"      ~ "Brown Trout",
                                 common_name == "Brook Trout (lotic)"      ~ "Brook Trout",
                                 common_name == "Brown Trout (lentic)"     ~ "Brown Trout",
                                 common_name == "Brook Trout (lentic)"     ~ "Brook Trout",
                                 common_name == "Rainbow Trout (lotic)"    ~ "Rainbow Trout",
                                 common_name == "Rainbow Trout (lentic)"   ~ "Rainbow Trout",
                                 common_name == "Cutthroat Trout (lotic)"  ~ "Cutthroat Trout",
                                 common_name == "Cutthroat Trout (lentic)" ~ "Cutthroat Trout",
                                 common_name == "Walleye (overall)"        ~ "Walleye",
                                 common_name == "Spotted Bass (overall)" ~ "Spotted Bass",
                                 common_name == "Paddlefish (overall)" ~ "Paddlefish", 
                                 common_name == "Muskellunge (overall)" ~ "Muskellunge", 
                                 TRUE ~ common_name), 
         gcat = "", 
         area = "North America") %>% 
  filter(count > 4, 
         mean != "Inf") %>% 
  dplyr::rename(N = count, 
                X5. = `5%`, 
                X25. = `25%`, 
                X50. = `50%`, 
                X75. = `75%`, 
                X95. = `95%`)
readr::write_csv(new_summary_df, "analysis_scripts/output_data/effort_northamerica.csv")

# Assess differences with current summarized data
old_summary_df <- read.csv("app/standardized_fish_data.csv")

old_summary_single_na <- old_summary_df %>%
  filter(area == "North America",
         str_detect(metric, "CPUE"))


comp_summary <- full_join(old_summary_single_na, new_summary_df,
                          by = c("common_name", "method", "waterbody_type", "gcat", "mean", "se", "metric"),
                          keep = TRUE)

length(which(is.na(comp_summary$common_name.x)))
length(which(is.na(comp_summary$common_name.y)))

old_only <- comp_summary %>%
  filter(is.na(common_name.y)) %>%
  select(contains(".x")) %>%
  rename_with(~ gsub(".x$", "", .x))
new_only <- comp_summary %>%
  filter(is.na(common_name.x)) %>%
  select(contains(".y")) %>%
  rename_with(~ gsub(".y$", "", .x))

no_match_comp <- full_join(old_only, new_only,
                           by = c("N" = "n",
                                  "waterbody_type",
                                  "common_name",
                                  #"method",
                                  "gcat"),
                           keep = TRUE) %>%
  filter(is.na(common_name.x) | is.na(common_name.y))

# these below have no match
# first is old summary data with no match; second is new summary data with no match
length(which(!is.na(no_match_comp$common_name.x)))
length(which(!is.na(no_match_comp$common_name.y)))

old_only_final <- no_match_comp %>%
  filter(is.na(common_name.y)) %>%
  select(contains(".x"), N) %>%
  rename_with(~ gsub(".x", "", .x))
new_only_final <- no_match_comp %>%
  filter(is.na(common_name.x)) %>%
  select(contains(".y"), n) %>%
  rename_with(~ gsub(".y", "", .x))
