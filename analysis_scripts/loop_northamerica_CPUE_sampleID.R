library(tidyverse)
library(dplyr)
library(plyr)
library(purrr)
library(FSA)
library(magrittr)
library(data.table)

setwd ("C:/Users/etracy1/Desktop/Backup/R_directory/AFS/StandardMethods/AFS_states")
write.csv(df_effort_day, "C:/Users/etracy1/Desktop/Backup/R_directory/AFS/StandardMethods/AFS_states/AFS_full_data_new_effort_032424.csv")
data <- read.csv("C:/Users/etracy1/Desktop/Backup/R_directory/AFS/StandardMethods/AFS_states/AFS_full_data_new_effort_032424.csv")
# 3/17/24 re run for with new effort data

# re-run 3/29/24 
data <- data_method1

# data test 4/3/24
data <- read.csv("C:/Users/etracy1/Desktop/pina_blanca.csv")


#data test 5/28/24
data <- read.csv("C:/Users/etracy1/Desktop/Backup/R_directory/AFS/StandardMethods/AFS_outlier_data_cleaned_052824.csv")

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
          summarize_at(vars(CPUE),funs(mean,se), na.rm=TRUE)
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
          summarize_at(vars(CPUE),funs(mean,se), na.rm=TRUE)
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

#rbind together separate NA, Eco, and State excel
NorthAmerica <- read.csv("Test_results_NA_112122.csv")
ecoregion <- read.csv("Test_results_eco_112122.csv")
state <- read.csv("Test_results_state_112122.csv")

all <- rbind.fill(NorthAmerica.results.CPUE, eco.CPUE.results, state.CPUE.results)
#write.csv(all, "New_effort_AFSresults_031724.csv", row.names = FALSE)

write.csv(all, "C:/Users/etracy1/Desktop/Backup/R_directory/AFS/StandardMethods/CPUE_all_060324.csv", row.names = FALSE)
