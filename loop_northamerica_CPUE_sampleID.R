library(tidyverse)
library(dplyr)
library(plyr)
library(purrr)
library(FSA)
library(magrittr)
library(data.table)

setwd
data <- read.csv("name")

data$weight_g <- as.numeric(as.character(data$weight_g))
data$total_length_mm <- as.numeric(as.character(data$total_length_mm))
data$effort <- as.numeric(as.character(data$effort))
data$total_m2 <- as.numeric(as.character(data$total_m2))
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
         | "tow_barge_electrofishing" %in% species.i.method.j.type.w$method
         | "raft_electrofishing" %in% species.i.method.j.type.w$method
         | "trawl" %in% species.i.method.j.type.w$method){
        electro_data <- species.i.method.j.type.w
        electro_data$effort <- electro_data$effort/3600
        electro_data$CPUE <- electro_data$count/electro_data$effort
        
        electro.CPUE <- electro_data%>%
          group_by(common_name, method, waterbody_type, watername_method_yearID)%>%
          summarize_at(vars(CPUE),funs(mean,se), na.rm=TRUE)
        colnames(electro.CPUE)[5] <- "Mean_by_ID"
        
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
        
        CPUE_gill_ID <- gill_data_CPUE%>%
          dplyr::mutate(count=n()) %>%
          group_by(common_name,method,waterbody_type, count)%>%
          summarize_at(vars(Mean_by_ID),funs(!!!p_funs, mean, se), na.rm=TRUE)
        
        CPUE_gill_ID$metric <- "CPUE"
        species.method.type.CPUE.results[[w]] <- as.data.frame(CPUE_gill_ID) }
      
      #CPUE fish/100m2 and fish/hour pass 1 only
      if("backpack_electrofishing" %in% species.i.method.j.type.w$method
         | "snorkel" %in% species.i.method.j.type.w$method) {
        backpack_data <- species.i.method.j.type.w
        #backpack_data <- backpack_data %>%
         # filter(!pass %in% c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26" ))
        backpack_data$total_m2 <- as.numeric(as.character(backpack_data$total_m2))
        backpack_data$count <- as.numeric(backpack_data$count)
        backpack_data$CPUE_distance <- backpack_data$count/backpack_data$total_m2
        
        backpack_data$effort <- backpack_data$effort/3600
        backpack_data$CPUE <- backpack_data$count/backpack_data$effort
        
        backpack_CPUE_distance <- backpack_data%>%
          group_by(common_name, method, waterbody_type, watername_method_yearID)%>%
          summarize_at(vars(CPUE_distance),funs(mean,se), na.rm=TRUE)
        colnames(backpack_CPUE_distance)[5] <- "Mean_by_ID"
        
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
