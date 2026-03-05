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
state.results.PSD <- list()

# for loop testing purposes (todo: delete when no longer needed)
# data_trial.df <- data_trial.df %>% 
#   filter(state %in% c("Illinois", "West Virginia", "Minnesota", "Idaho"))
# better example will have multiple gcat categories
# s <- unique(data_trial.df$state)[1]
# i <- unique(state.s$common_name)[27]
# j <- unique(species.i$method)[3]
# w <- unique(species.i.method.j$waterbody_type)[1]
# u <- unique(species.i.method.j.type.w$watername_method_yearID)[3]

# todo: replace loop w/ groupby? 
for(s in unique(data_trial.df$state)){
  state.s <- data_trial.df[data_trial.df$state==s,]
  #todo: move this outside of loop with rest of removing NA
  state.s <- state.s[!is.na(state.s$common_name),]
  stock.results.PSD <- list()
  
  for(i in unique(state.s$common_name)){
    species.i <- state.s[state.s$common_name==i,]
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
        
        # adding a row to psd df with frequency of 0 for every category that isn't there already
        if("stock" %notin% psd$gcat){
          psd[nrow(psd)+1,] <- data.frame(common_name=i, gcat="stock",Freq=0)} 
        if("quality" %notin% psd$gcat){
          psd[nrow(psd)+1,] <- data.frame(common_name=i, gcat="quality",Freq=0)}
        if("preferred" %notin% psd$gcat){
          psd[nrow(psd)+1,] <- data.frame(common_name=i, gcat="preferred",Freq=0)}
        if("memorable" %notin% psd$gcat){
          psd[nrow(psd)+1,] <- data.frame(common_name=i, gcat="memorable",Freq=0)}
        if("trophy" %notin% psd$gcat){
          psd[nrow(psd)+1,] <- data.frame(common_name=i, gcat="trophy",Freq=0)}
        
        species.method.type.id.results[[u]] <- psd
       
        } ## u
        species.method.type.id.results.df <- rbindlist(species.method.type.id.results, 
                                                       idcol = "watername_method_yearID",  
                                                       fill=TRUE)
        # is species.method.type.id.results.df supposed to be species.i.method.j.type.w.id.u?
        species.method.type.id.results.final <- species.method.type.id.results.df%>%
          group_by(gcat) %>%
          summarize_at(vars(Freq), list(mean = mean, se = se), na.rm=TRUE)
        
        colnames(species.method.type.id.results.final)[2] <- "mean"
        species.method.type.id.results.final$metric <- "Length Frequency"
        
        stock.count <- length(unique(species.method.type.id.results.df$watername_method_yearID[species.method.type.id.results.df$gcat=="stock"]))
        quality.count <- length(unique(species.method.type.id.results.df$watername_method_yearID[species.method.type.id.results.df$gcat=="quality"]))
        preferred.count <- length(unique(species.method.type.id.results.df$watername_method_yearID[species.method.type.id.results.df$gcat=="preferred"]))
        memorable.count <- length(unique(species.method.type.id.results.df$watername_method_yearID[species.method.type.id.results.df$gcat=="memorable"]))
        trophy.count <- length(unique(species.method.type.id.results.df$watername_method_yearID[species.method.type.id.results.df$gcat=="trophy"]))
        
        all.count <- c(stock.count, quality.count, preferred.count, memorable.count, trophy.count) ## Make vector of each gcat count 
        
        gcat.order <- c("stock", "quality", "preferred", "memorable", "trophy")
        species.method.type.id.results.final <- species.method.type.id.results.final[match(gcat.order, species.method.type.id.results.final$gcat),] 
        
        species.method.type.id.results.final$n <- all.count ## add in count to full df
        
        results.species.method.type[[w]] <-  species.method.type.id.results.final
        
      } ## w
      species.method.type.id.results.df <- rbindlist(results.species.method.type, idcol = "waterbody_type",  fill=TRUE)
      results.species.method[[j]] <-  species.method.type.id.results.df
      
    } # j
    results.species.method.df <- rbindlist(results.species.method, idcol= "method", fill=TRUE)
    stock.results.PSD[[i]] <- results.species.method.df
  
  } ## i
  PSD <- rbindlist(stock.results.PSD, idcol = "common_name",  fill=TRUE)
  state.s.results <- rbind.fill(PSD)
  state.s.results$state <- s
  state.results.PSD[[s]] <- state.s.results
  
} ## s
state.length.results <- rbindlist(state.results.PSD)

# latter two come from other two state scripts
#Final.state <- rbind.fill(state.length.results, state.weight.results, state.CPUE.results)

#write.csv(Final.state, "C:/Users/etracy1/Desktop/Backup/R_directory/AFS/StandardMethods/AFS_state_averages_092524.csv", row.names = FALSE)

# Assess differences with current summarized data
old_summary_df <- read.csv("app/standardized_fish_data.csv")

old_summary_state <- old_summary_df %>% 
  filter(area %notin% c("Ontario", "North America", "10 North American Deserts", "12 Southern Semi-Arid Highlands", "13 Temperate Sierras", "15 Tropical Wet Forests Northern Forests", "4 Hudson Plain", "5 Northern Forests", "6 Northwestern Forested Mountains", "7 Marine West Coast Forest", "8 Eastern Temperate Forests", "9 Great Plains"),  
         metric == "Length Frequency")

new_summary_df <- state.length.results %>% 
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

comp_summary <- full_join(old_summary_state, new_summary_df, 
                          by = c("area" = "state", "common_name", "method", "waterbody_type", "gcat", "mean", "se", "metric"), 
                          keep = TRUE) 

length(which(is.na(comp_summary$common_name.x)))
length(which(is.na(comp_summary$common_name.y)))

old_only <- comp_summary %>% 
  filter(is.na(common_name.y)) %>% 
  select(contains(".x"), N, area) %>% 
  rename_with(~ gsub(".x$", "", .x))
new_only <- comp_summary %>% 
  filter(is.na(common_name.x)) %>% 
  select(contains(".y"), n, state) %>% 
  rename_with(~ gsub(".y$", "", .x))

no_match_comp <- full_join(old_only, new_only, 
                           by = c("N" = "n", 
                                  "area" = "state", 
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
  select(contains(".x"), N, area) %>% 
  rename_with(~ gsub(".x$", "", .x))
new_only_final <- no_match_comp %>% 
  filter(is.na(common_name.x)) %>% 
  select(contains(".y"), n, state) %>% 
  rename_with(~ gsub(".y$", "", .x))

readr::write_csv(old_only_final, "analysis_scripts/no_match/state_length_previous.csv")
readr::write_csv(new_only_final, "analysis_scripts/no_match/state_length_current.csv")


