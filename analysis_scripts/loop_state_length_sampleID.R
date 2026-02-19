library(tidyverse)
library(dplyr)
library(plyr)
library(purrr)
library(FSA)
library(magrittr)
library(data.table)

data <- read.csv("AFS_fishdata_FINAL_112121.csv")
data <- read.csv("AZ_test.csv")

# rerun 032524
data <- data_length

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
data_trial.df <- data_trial.df[!is.na(data_trial.df$total_length_mm),]
data_trial.df <- data_trial.df[!is.na(data_trial.df$waterbody_type),]
state.results.PSD <- list()

for(s in unique(data_trial.df$state)){
  state.s <- data_trial.df[data_trial.df$state==s,]
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
          summarize_at(vars(Freq),funs(mean, se), na.rm=TRUE)
        
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

Final.state <- rbind.fill(state.length.results, state.weight.results, state.CPUE.results)

write.csv(Final.state, "C:/Users/etracy1/Desktop/Backup/R_directory/AFS/StandardMethods/AFS_state_averages_092524.csv", row.names = FALSE)

