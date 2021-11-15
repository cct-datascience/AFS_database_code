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
          weight <- mutate(species.i.method.j.type.w.id.u, Wr=wrAdd(weight_g, total_length_mm, common_name))
          weight.means <- weight %>%
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
  
} ##s
state.weight.results <- rbindlist(state.results)
