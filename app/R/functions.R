time_methods <- c("boat_electrofishing", "raft_electrofishing")
number_methods <- c("gill_net_fall", "gill_net_spring", "drifting_trammel_net", 
                    "large_catfish_hoopnet", "bag_seine", "stream_seine", 
                    "backpack_electrofishing", "tow_barge_electrofishing")

calculate_cpue <- function(df){
  stopifnot(c("type", "area", "common_name", "method", "waterbody_type", 
              "year", "effort") %in% colnames(df)) #todo check these
  cpue <- df %>% 
    group_by(type, area, waterbody_name, year, method, waterbody_type, common_name) %>% 
    mutate(gcat = FSA::psdAdd(total_length, common_name, what = "incremental")) %>% 
    filter(gcat != "substock") %>%
    summarize(n_fish = n(), 
              effort_sampleID = mean(effort)) %>% 
    mutate(effort = case_when(method %in% time_methods ~ effort_sampleID / 3600, 
                              method %in% number_methods ~ effort_sampleID)) %>% 
    mutate(cpue = n_fish / effort) %>% # n is number of fish
    group_by(type, area, common_name, method, waterbody_type) %>%
    summarize_at(vars(cpue), list(mean = mean, se = se,
                                  `5%` = ~quantile(x = ., probs = 0.05),
                                  `25%` = ~quantile(x = ., probs = 0.25),
                                  `50%` = ~quantile(x = ., probs = 0.50),
                                  `75%` = ~quantile(x = ., probs = 0.75),
                                  `95%` = ~quantile(x = ., probs = 0.95)),
                 na.rm = TRUE) %>%
    mutate(metric = "CPUE")
  return(cpue)
}

calculate_lf <- function(df){
  stopifnot(c("type", "area", "common_name", "method", "waterbody_type", 
              "year", "total_length") %in% colnames(df))
  lf <- df %>% 
    mutate(gcat = FSA::psdAdd(total_length, common_name, what = "incremental")) %>% 
    group_by(type, area, common_name, method, waterbody_type, year) %>% 
    count(gcat) %>% 
    mutate(lenfreq = (n / sum(n)) * 100) %>% 
    group_by(type, area, common_name, method, waterbody_type, gcat) %>% 
    summarize_at(vars(lenfreq), list(mean = mean, se = FSA::se), na.rm = TRUE) %>% 
    mutate(metric = "Length Frequency", 
           gcat = factor(gcat, levels = c("stock", "quality", "preferred",
                                          "memorable", "trophy"))) %>%
    filter(!is.na(gcat))
  return(lf)
}

calculate_rw <- function(df){
  stopifnot(c("type", "area", "common_name", "method", "waterbody_type", "year", 
              "total_length", "weight") %in% colnames(df))
  rw <- df %>% 
    mutate(gcat = FSA::psdAdd(total_length, common_name, what = "incremental"), 
           relweight = FSA::wrAdd(wt = weight, len = total_length, spec = common_name), 
           relweight = as.numeric(relweight)) %>% 
    group_by(type, area, common_name, method, waterbody_type, gcat) %>% 
    summarize_at(vars(relweight), list(mean = mean, se = FSA::se, 
                                       `5%` = ~quantile(x = ., probs = 0.05, na.rm = TRUE),
                                       `25%` = ~quantile(x = ., probs = 0.25, na.rm = TRUE),
                                       `50%` = ~quantile(x = ., probs = 0.50, na.rm = TRUE),
                                       `75%` = ~quantile(x = ., probs = 0.75, na.rm = TRUE),
                                       `95%` = ~quantile(x = ., probs = 0.95, na.rm = TRUE)), na.rm = TRUE) %>% 
    mutate(mean = ifelse(is.nan(mean), NA, mean), 
           metric = "Relative Weight", 
           gcat = factor(gcat, levels = c("stock", "quality", "preferred",
                                          "memorable", "trophy"))) %>%
    filter(!is.na(gcat))
  return(rw)
}
