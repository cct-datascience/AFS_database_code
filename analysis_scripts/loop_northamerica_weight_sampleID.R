library(tidyverse)
library(dplyr)
library(plyr)
library(purrr)
library(FSA)
library(magrittr)
library(data.table)

#data <- read.csv("AFS_fishdata_FINAL_112121.csv")
#data <- read.csv("AZ_test.csv")

# re-run 3/26/24 
data <- read.csv("C:/Users/etracy1/Desktop/Backup/R_directory/AFS/StandardMethods/AFS_data_weight_outlier_032424.csv")

#Rainbow Trout name change
data_lakes <- data[data$waterbody_type =="small_standing_waters" | data$waterbody_type =="large_standing_waters"| data$waterbody_type =="two_story_standing_waters",]
data_lakes$common_name[data_lakes$common_name=="Rainbow Trout"] <- "Rainbow Trout (lentic)"
data_rivers <- data[data$waterbody_type =="rivers" | data$waterbody_type =="wadeable_streams",]
data_rivers$common_name[data_rivers$common_name=="Rainbow Trout"] <- "Rainbow Trout (lotic)"
data_weight <- rbind(data_lakes, data_rivers)

data$common_name[data$common_name=="Brook Trout (lotic)"] <- "Brook Trout"

data_subset  <- filter(data, common_name== "Brook Trout")


data <- data_subset


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
options(scipen=10)

# code for length-frequency, relative weight and CPUE (boat electrofishing) in loop
data_trial.df <- data.frame(data)
data_trial.df <- data_trial.df[!is.na(data_trial.df$weight_g),]
data_trial.df <- data_trial.df[!is.na(data_trial.df$total_length_mm),]
data_trial.df <- data_trial.df[!is.na(data_trial.df$waterbody_type),]
stock.results.wr <- list()

for(i in unique(data_trial.df$common_name)){
  species.i <- data_trial.df[data_trial.df$common_name==i,]
  results.species.method <- list()
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
        weight <- mutate(species.i.method.j.type.w.id.u, Wr=wrAdd(weight_g, total_length_mm, common_name, remove.submin=TRUE))
        
        # Rename your mutated data frame so as to make it clear that it contains potential outliers
        
        weight_all <- mutate(species.i.method.j.type.w.id.u, Wr=wrAdd(weight_g, total_length_mm, common_name))
      
        weight_all$Wr[weight_all$Wr<30 | weight_all$Wr>230] <- NA
        
        #can I exclude individual relative weight outliers 
        weight.means <- weight_all %>%
          group_by(gcat)%>%
          summarize_at(vars(Wr),funs( mean), na.rm=TRUE)
        
        weight.means$watername_method_yearID <- u
        results.species.method.type.id[[u]] <- weight.means
        
        
      } ## u
      results.species.method.type.id.df <- rbindlist(results.species.method.type.id)
       
      results.species.method.type.id.final <- results.species.method.type.id.df%>%
        group_by(gcat)%>%
        summarize_at(vars(Wr),funs(!!!p_funs, mean, se), na.rm=TRUE)
 
      results.species.method.type.id.final$metric <- "Relative Weight"
    
    # This is similar to length frequency just separating into different categories         
   stock.count <- length(unique(results.species.method.type.id.df$watername_method_yearID[results.species.method.type.id.df$gcat=="stock"]))
   quality.count <- length(unique(results.species.method.type.id.df$watername_method_yearID[results.species.method.type.id.df$gcat=="quality"]))
   preferred.count <- length(unique(results.species.method.type.id.df$watername_method_yearID[results.species.method.type.id.df$gcat=="preferred"]))
   memorable.count <- length(unique(results.species.method.type.id.df$watername_method_yearID[results.species.method.type.id.df$gcat=="memorable"]))
   trophy.count <- length(unique(results.species.method.type.id.df$watername_method_yearID[results.species.method.type.id.df$gcat=="trophy"]))
   
 all.count <- c(stock.count, quality.count, preferred.count, memorable.count, trophy.count) ## Make vector of each gcat count 
  
 #this adds all categories but puts NAs in them when no data present (this can be switched in position with the code above to show count of sampleID per category vs. total sampleID)
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
    species.method.type.results.df <- rbindlist(results.species.method.type, idcol = "waterbody_type",  fill=TRUE)
    results.species.method[[j]] <-  species.method.type.results.df
    
  } # j
  results.species.method.df <- rbindlist(results.species.method, idcol= "method", fill=TRUE)
  stock.results.wr[[i]] <- results.species.method.df
  
} # i
NorthAmerica.results.weight <- rbindlist(stock.results.wr, idcol="common_name", fill=TRUE)

weight_all <- rbind.fill(NorthAmerica.results.weight, eco.weight.results, state.weight.results)

#write.csv(NorthAmerica.results.CPUE, "CPUE_test_012124.csv")
write.csv(weight_all, "C:/Users/etracy1/Desktop/Backup/R_directory/AFS/StandardMethods/brook_trout_weight.csv", row.names = FALSE)

