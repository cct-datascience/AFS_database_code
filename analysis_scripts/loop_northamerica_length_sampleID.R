library(dplyr)
library(plyr)
library(purrr)
library(FSA)
library(data.table)

data <- read.csv("analysis_scripts/input_data/AFS_final_01132026.csv")

data$weight_g <- as.numeric(as.character(data$weight_g))
data$total_length_mm <- as.numeric(as.character(data$total_length_mm))
data$effort <- as.numeric(as.character(data$effort))
data$total_m2 <- as.numeric(as.character(data$total_m2))
data$count <- as.numeric(as.character(data$count))
`%notin%` <- Negate(`%in%`)

# code for length-frequency, relative weight and CPUE (boat electrofishing) in loop
data_trial.df <- data.frame(data)
data_trial.df <- data_trial.df[!is.na(data_trial.df$total_length_mm),]
data_trial.df <- data_trial.df[!is.na(data_trial.df$waterbody_type),]

stock.results.PSD <- list()

for(i in unique(data_trial.df$common_name)){
  species.i <- data_trial.df[data_trial.df$common_name==i,]
  results.species.method <- list()
  
  for(j in unique(species.i$method)){
    species.i.method.j <- species.i[species.i$method==j,]
    results.species.method.type <- list()
    
    for(w in unique(species.i.method.j$waterbody_type)){
      species.i.method.j.type.w <- species.i.method.j[species.i.method.j$waterbody_type==w,]
      species.method.type.id.results <- list()
      
      for(u in unique(species.i.method.j.type.w$watername_method_yearID)){
        species.i.method.j.type.w.id.u <- species.i.method.j.type.w[species.i.method.j.type.w$watername_method_yearID==u,]
        
        #length frequency
        freq <- xtabs(~common_name+gcat, data = species.i.method.j.type.w.id.u)
        psd <- prop.table(freq,margin=1)*100
        psd <-as.data.frame(psd)
        psd$gcat <- as.character(psd$gcat)
        
        #This adds a row with a 0 for each category that doesnt have a result
        if("stock" %notin% psd$gcat){psd[nrow(psd)+1,] <- data.frame(common_name=i, gcat="stock",Freq=0)} 
        if("quality" %notin% psd$gcat){psd[nrow(psd)+1,] <- data.frame(common_name=i, gcat="quality",Freq=0)}
        if("preferred" %notin% psd$gcat){psd[nrow(psd)+1,] <- data.frame(common_name=i, gcat="preferred",Freq=0)}
        if("memorable" %notin% psd$gcat){psd[nrow(psd)+1,] <- data.frame(common_name=i, gcat="memorable",Freq=0)}
        if("trophy" %notin% psd$gcat){psd[nrow(psd)+1,] <- data.frame(common_name=i, gcat="trophy",Freq=0)}
        
        species.method.type.id.results[[u]] <- psd
        
      } ## u
      species.method.type.id.results.df <- rbindlist(species.method.type.id.results, idcol = "watername_method_yearID",  fill=TRUE)
      
      species.method.type.id.results.final <- species.method.type.id.results.df%>%
        group_by(gcat) %>%
        summarize_at(vars(Freq), list(mean = mean, se = se), na.rm=TRUE)
      
      colnames(species.method.type.id.results.final)[2] <- "mean"
      species.method.type.id.results.final$metric <- "Length Frequency"
      
      #I think a lot of this is making sure N is total unique Sample IDs so all N should be the same for each category
      stock.count <- length(unique(species.method.type.id.results.df$watername_method_yearID[species.method.type.id.results.df$gcat=="stock"]))
      quality.count <- length(unique(species.method.type.id.results.df$watername_method_yearID[species.method.type.id.results.df$gcat=="quality"]))
      preferred.count <- length(unique(species.method.type.id.results.df$watername_method_yearID[species.method.type.id.results.df$gcat=="preferred"]))
      memorable.count <- length(unique(species.method.type.id.results.df$watername_method_yearID[species.method.type.id.results.df$gcat=="memorable"]))
      trophy.count <- length(unique(species.method.type.id.results.df$watername_method_yearID[species.method.type.id.results.df$gcat=="trophy"]))
      
      all.count <- c(stock.count, quality.count, preferred.count, memorable.count, trophy.count) ## Make vector of each gcat count 
      
      #this makes it so its always in this order
      gcat.order <- c("stock", "quality", "preferred", "memorable", "trophy")
      species.method.type.id.results.final <- species.method.type.id.results.final[match(gcat.order, species.method.type.id.results.final$gcat),] 
      
      species.method.type.id.results.final$n <- all.count ## add in count to full df
      ### QUESTION: What should be the count by category - 
      # if we want to change this we would use the same format as relative weight and not add 0s 
      
      results.species.method.type[[w]] <-  species.method.type.id.results.final
      
    } ## w
    species.method.type.results.df <- rbindlist(results.species.method.type, idcol = "waterbody_type",  fill=TRUE)
    results.species.method[[j]] <-  species.method.type.results.df
    
  } # j
  results.species.method.df <- rbindlist(results.species.method, idcol= "method", fill=TRUE)
  stock.results.PSD[[i]] <- results.species.method.df
  
} # i
NorthAmerica.results.length <- rbindlist(stock.results.PSD, idcol="common_name", fill=TRUE)

# Assess differences with current summarized data
old_summary_df <- read.csv("app/standardized_fish_data.csv")

old_summary_single_na <- old_summary_df %>% 
  filter(area == "North America", 
         metric == "Length Frequency")

new_summary_df <- NorthAmerica.results.length %>% 
  mutate(mean = round(mean, 1), 
         se = round(se, 1), 
         method = stringr::str_replace_all(method, "_", " "), 
         waterbody_type = stringr::str_replace_all(waterbody_type, "_", " "), 
         common_name = case_when(common_name == "Brown Trout (lotic)" ~ "Brown Trout",
                                 common_name == "Brook Trout (lotic)" ~ "Brook Trout",
                                 common_name == "Brown Trout (lentic)" ~ "Brown Trout",
                                 common_name == "Brook Trout (lentic)" ~ "Brook Trout",
                                 TRUE ~ common_name), 
         gcat = case_when(gcat == "stock" ~ "Stock-Quality", 
                          gcat == "quality" ~ "Quality-Preferred", 
                          gcat == "preferred" ~ "Preferred-Memorable", 
                          gcat == "memorable" ~ "Memorable-Trophy", 
                          gcat == "trophy" ~ "Trophy", 
                          TRUE ~ "ERROR")) %>% 
  filter(n > 4)
unique(new_summary_df$gcat)

comp_summary <- full_join(old_summary_single_na, new_summary_df, 
                          by = c("common_name", "method", "waterbody_type", "gcat", "mean", "se", "metric"), 
                          keep = TRUE) 

length(which(is.na(comp_summary$common_name.x)))
length(which(is.na(comp_summary$common_name.y)))

old_only <- comp_summary %>% 
  filter(is.na(common_name.y)) %>% 
  select(contains(".x"), N, area) %>% 
  rename_with(~ gsub(".x$", "", .x))
new_only <- comp_summary %>% 
  filter(is.na(common_name.x)) %>% 
  select(contains(".y"), n) %>% 
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

readr::write_csv(old_only_final, "analysis_scripts/no_match/northamerica_length_previous.csv")
readr::write_csv(new_only_final, "analysis_scripts/no_match/northamerica_length_current.csv")

#write.csv(NorthAmerica.results.length, "C:/Users/etracy1/Desktop/final_AFS_2026/northamerica_length_results_01132026.csv", row.names = FALSE)
#write.csv(lengthall, "C:/Users/etracy1/Desktop/Backup/R_directory/AFS/StandardMethods/length_test.csv", row.names = FALSE)
#all_rainbowtrout <- rbind.fill(NorthAmerica.results.length, eco.length.results, state.length.results)
#write.csv(NorthAmerica.results.length, "C:/Users/etracy1/Desktop/Backup/length_test.csv", row.names = FALSE)


