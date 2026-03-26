library(tidyverse)
library(plyr)
library(dplyr)
library(purrr)
library(FSA)
library(magrittr)
library(data.table)

data <- read.csv("analysis_scripts/input_data/AFS_lengthweight_cleaned_NV_Ontario_update_03232026.csv")

data <- data %>%
  mutate(common_name = case_when(
    common_name == "Spotted Bass" ~ "Spotted Bass (overall)",
    TRUE ~ common_name
  ))
data$weight_g <- as.numeric(as.character(data$weight_g))
data$total_length_mm <- as.numeric(as.character(data$total_length_mm))
data$effort <- as.numeric(as.character(data$effort))
data$total_m2 <- as.numeric(as.character(data$total_m2))
data$count <- as.numeric(as.character(data$count))
`%notin%` <- Negate(`%in%`)

#summarizing by percentile 
p <- c(0.05, 0.25, 0.50, 0.75, 0.95)
p_names <- map_chr(p, ~paste0(.x*100,"%"))
p_funs <- purrr::map(p, ~partial(quantile, probs = .x, na.rm.= TRUE))%>%
  set_names(nm = p_names)
p_funs
options(scipen=10)

# code for length-frequency, relative weight and CPUE (boat electrofishing) in loop
data_trial.df <- data.frame(data)
data_trial.df <- data_trial.df[!is.na(data_trial.df$weight_g),]
data_trial.df <- data_trial.df[!is.na(data_trial.df$waterbody_type),]

state.results <- list()

for(s in unique(data_trial.df$state)){
  state.s <- data_trial.df[data_trial.df$state==s,]
  state.s <- state.s[!is.na(state.s$common_name),]
  stock.results.wr <- list()
  
  for(i in unique(state.s$common_name)){
    species.i <- state.s[state.s$common_name==i,]
    results.species.method.wr <- list()
    #ws.species.i <- wsVal(i)
    
    for(j in unique(species.i$method)){
      species.i.method.j <- species.i[species.i$method==j,]
      results.species.method.type <- list()
      
      for(w in unique(species.i.method.j$waterbody_type)){
        species.i.method.j.type.w <- species.i.method.j[species.i.method.j$waterbody_type==w,]
        results.species.method.type.id <- list()
        
        for(u in unique(species.i.method.j.type.w$watername_method_yearID)){
          species.i.method.j.type.w.id.u <- species.i.method.j.type.w[species.i.method.j.type.w$watername_method_yearID==u,]
        
        #Relative Weight
          if(str_detect(i, "lentic")){
            species.i.method.j.type.w.id.u <- species.i.method.j.type.w.id.u %>% 
              mutate(base_common_name = str_remove(common_name, " \\(lentic\\)"))
            weight_all <- mutate(species.i.method.j.type.w.id.u, 
                                 Wr=wrAdd(weight_g, total_length_mm, base_common_name, 
                                          WsOpts=list("Brook Trout"=list(group="overall"), 
                                                      "Brown Trout"=list(group="lentic"), 
                                                      "Cutthroat Trout"=list(group="lentic"), 
                                                      "Rainbow Trout"=list(group="lentic"))))
          } else if(str_detect(i, "lotic")){
            species.i.method.j.type.w.id.u <- species.i.method.j.type.w.id.u %>% 
              mutate(base_common_name = str_remove(common_name, " \\(lotic\\)"))
            weight_all <- mutate(species.i.method.j.type.w.id.u, 
                                 Wr=wrAdd(weight_g, total_length_mm, base_common_name, 
                                          WsOpts=list("Brook Trout"=list(group="overall"), 
                                                      "Brown Trout"=list(group="lotic"), 
                                                      "Cutthroat Trout"=list(group="lotic"), 
                                                      "Rainbow Trout"=list(group="lotic"))))
          } else if(str_detect(i, "overall")){
            species.i.method.j.type.w.id.u <- species.i.method.j.type.w.id.u %>% 
              mutate(base_common_name = str_remove(common_name, " \\(overall\\)"))
            weight_all <- mutate(species.i.method.j.type.w.id.u, 
                                 Wr=wrAdd(weight_g, total_length_mm, base_common_name, 
                                          WsOpts=list("Walleye"=list(group="overall"), 
                                                      "Spotted Bass"=list(group="overall"), 
                                                      "Paddlefish"=list(group="overall"), 
                                                      "Muskellunge"=list(group="overall"))))
          } else {
            weight_all <- mutate(species.i.method.j.type.w.id.u, Wr=wrAdd(weight_g, total_length_mm, common_name))
          }

          weight_all$Wr[weight_all$Wr<30 | weight_all$Wr>230] <- NA
          
          #can I exclude individual relative weight outliers 
          weight.means <- weight_all %>%
            group_by(gcat)%>%
            summarize_at(vars(Wr), list(Wr = mean), na.rm=TRUE)
          
          weight.means$watername_method_yearID <- u
          results.species.method.type.id[[u]] <- weight.means
        
      } ## u
        results.species.method.type.id.df <- rbindlist(results.species.method.type.id)
        
        results.species.method.type.id.final <- results.species.method.type.id.df%>%
          group_by(gcat)%>%
          summarize_at(vars(Wr),funs(!!!p_funs, mean, se), na.rm=TRUE)
        
        results.species.method.type.id.final$metric <- "Relative Weight"
        
        stock.count <- length(unique(results.species.method.type.id.df$watername_method_yearID[results.species.method.type.id.df$gcat=="stock"]))
        quality.count <- length(unique(results.species.method.type.id.df$watername_method_yearID[results.species.method.type.id.df$gcat=="quality"]))
        preferred.count <- length(unique(results.species.method.type.id.df$watername_method_yearID[results.species.method.type.id.df$gcat=="preferred"]))
        memorable.count <- length(unique(results.species.method.type.id.df$watername_method_yearID[results.species.method.type.id.df$gcat=="memorable"]))
        trophy.count <- length(unique(results.species.method.type.id.df$watername_method_yearID[results.species.method.type.id.df$gcat=="trophy"]))
        
        all.count <- c(stock.count, quality.count, preferred.count, memorable.count, trophy.count) ## Make vector of each gcat count 
        
        if("stock" %notin% results.species.method.type.id.final$gcat){results.species.method.type.id.final[nrow(results.species.method.type.id.final)+1,] <- data.frame( gcat="stock", NA,NA,NA,NA,NA,NA,NA,metric="Relative Weight")} 
        if("quality" %notin% results.species.method.type.id.final$gcat){results.species.method.type.id.final[nrow(results.species.method.type.id.final)+1,] <-data.frame( gcat="quality", NA,NA,NA,NA,NA,NA,NA,metric="Relative Weight")} 
        if("preferred" %notin% results.species.method.type.id.final$gcat){results.species.method.type.id.final[nrow(results.species.method.type.id.final)+1,] <- data.frame( gcat="preferred", NA,NA,NA,NA,NA,NA,NA,metric="Relative Weight")}  
        if("memorable" %notin% results.species.method.type.id.final$gcat){results.species.method.type.id.final[nrow(results.species.method.type.id.final)+1,] <- data.frame( gcat="memorable", NA,NA,NA,NA,NA,NA,NA,metric="Relative Weight")} 
        if("trophy" %notin% results.species.method.type.id.final$gcat){results.species.method.type.id.final[nrow(results.species.method.type.id.final)+1,] <- data.frame( gcat="trophy", NA,NA,NA,NA,NA,NA,NA,metric="Relative Weight")} 
        
        gcat.order <- c("stock", "quality", "preferred", "memorable", "trophy")
        results.species.method.type.id.final <- results.species.method.type.id.final[match(gcat.order, results.species.method.type.id.final$gcat),] 
        
        results.species.method.type.id.final$n <- all.count ## add in count to full df
        
        results.species.method.type[[w]] <-  results.species.method.type.id.final
      
      } ## w
      species.method.type.wr.results.df <- rbindlist(results.species.method.type, idcol = "waterbody_type", fill=TRUE)
      results.species.method.wr[[j]] <- species.method.type.wr.results.df
      
    } ## j
    results.species.method.wr.df <- rbindlist(results.species.method.wr, idcol= "method", fill=TRUE)
    stock.results.wr[[i]] <- results.species.method.wr.df

  } ##i
  Wr <- rbindlist(stock.results.wr, idcol="common_name", fill=TRUE)
  state.s.results <- rbind.fill(Wr)
  state.s.results$state <- s
  state.results[[s]] <- state.s.results
  print(s)
} ##s
state.weight.results <- rbindlist(state.results)

# Clean up format of summarized data and save out
new_summary_df <- state.weight.results %>% 
  filter(!is.na(mean)) %>% 
  mutate(mean = round(mean, 1), 
         se = round(se, 1),
         `5%` = round(`5%`, 1),
         `25%` = round(`25%`, 1),
         `50%` = round(`50%`, 1),
         `75%` = round(`75%`, 1),
         `95%` = round(`95%`, 1),
         method = stringr::str_replace_all(method, "_", " "), 
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
           common_name == "Walleye (overall)"        ~ "Walleye",
           common_name == "Spotted Bass (overall)" ~ "Spotted Bass",
           common_name == "Paddlefish (overall)" ~ "Paddlefish", 
           common_name == "Muskellunge (overall)" ~ "Muskellunge", 
           TRUE ~ common_name), 
         gcat = case_when(gcat == "stock" ~ "Stock-Quality", 
                          gcat == "quality" ~ "Quality-Preferred", 
                          gcat == "preferred" ~ "Preferred-Memorable", 
                          gcat == "memorable" ~ "Memorable-Trophy", 
                          gcat == "trophy" ~ "Trophy", 
                          TRUE ~ "ERROR")) %>% 
  filter(n > 4) %>% 
  dplyr::rename(N = n, 
                area = state, 
                X5. = `5%`, 
                X25. = `25%`, 
                X50. = `50%`, 
                X75. = `75%`, 
                X95. = `95%`)
unique(new_summary_df$gcat)
readr::write_csv(new_summary_df, "analysis_scripts/output_data/weights_state.csv")

# # Assess differences with current summarized data
old_summary_df <- read.csv("app/standardized_fish_data.csv")

old_summary_state <- old_summary_df %>%
  filter(area %notin% c("North America", "10 North American Deserts", "12 Southern Semi-Arid Highlands", "13 Temperate Sierras", "15 Tropical Wet Forests Northern Forests", "4 Hudson Plain", "5 Northern Forests", "6 Northwestern Forested Mountains", "7 Marine West Coast Forest", "8 Eastern Temperate Forests", "9 Great Plains"),
         metric == "Relative Weight")

comp_summary <- full_join(old_summary_state, new_summary_df,
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
                           by = c("N",
                                  "area",
                                  "waterbody_type",
                                  "common_name",
                                  "method",
                                  "gcat"),
                           keep = TRUE) %>%
  #filter(!is.na(common_name.x), !is.na(common_name.y))
  filter(is.na(common_name.x) | is.na(common_name.y))

# these below have no match
# first is old summary data with no match; second is new summary data with no match
length(which(!is.na(no_match_comp$common_name.x)))
length(which(!is.na(no_match_comp$common_name.y)))

old_only_final <- no_match_comp %>%
  filter(is.na(common_name.y)) %>%
  select(contains(".x")) %>%
  rename_with(~ gsub(".x", "", .x))
new_only_final <- no_match_comp %>%
  filter(is.na(common_name.x)) %>%
  select(contains(".y")) %>%
  rename_with(~ gsub(".y", "", .x))

