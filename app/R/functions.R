time_methods   <- c("boat_electrofishing", "raft_electrofishing")
number_methods <- c("gill_net_fall", "gill_net_spring", "drifting_trammel_net",
                    "large_catfish_hoopnet", "bag_seine", "stream_seine",
                    "backpack_electrofishing", "tow_barge_electrofishing")

# PSD group arguments defined once and reused across all calculations
.psd_overall <- list("Brook Trout" = list(group = "overall"))
.psd_lentic  <- list("Brown Trout"     = list(group = "lentic"),
                     "Brook Trout"     = list(group = "lentic"),
                     "Cutthroat Trout" = list(group = "lentic"),
                     "Rainbow Trout"   = list(group = "lentic"))
.psd_lotic   <- list("Brown Trout"     = list(group = "lotic"),
                     "Brook Trout"     = list(group = "lotic"),
                     "Cutthroat Trout" = list(group = "lotic"),
                     "Rainbow Trout"   = list(group = "lotic"))

# Wr group arguments (Brook Trout uses "overall" for Wr even in lentic/lotic)
.wr_lentic <- list("Brown Trout"     = list(group = "lentic"),
                   "Brook Trout"     = list(group = "overall"),
                   "Cutthroat Trout" = list(group = "lentic"),
                   "Rainbow Trout"   = list(group = "lentic"))
.wr_lotic  <- list("Brown Trout"     = list(group = "lotic"),
                   "Brook Trout"     = list(group = "overall"),
                   "Cutthroat Trout" = list(group = "lotic"),
                   "Rainbow Trout"   = list(group = "lotic"))

# Shared helper: filter by FSA_group and assign PSD length category
.filter_gcat <- function(df, fsa_val, psd_grp = NULL) {
  df <- if (is.na(fsa_val)) filter(df, is.na(FSA_group)) else filter(df, FSA_group == fsa_val)
  if (is.null(psd_grp)) {
    mutate(df, gcat = FSA::psdAdd(total_length, common_name, what = "incremental"))
  } else {
    mutate(df, gcat = FSA::psdAdd(total_length, common_name, what = "incremental", group = psd_grp))
  }
}

# ---- CPUE ----------------------------------------------------------------

.cpue_block <- function(df, fsa_val, psd_grp) {
  .filter_gcat(df, fsa_val, psd_grp) %>%
    filter(gcat != "substock") %>%
    group_by(type, area, waterbody_name, year, method, waterbody_type, common_name, transect_or_net_set) %>%
    summarize(n_fish = n(), effort_transect = mean(effort), .groups = "drop_last") %>%
    mutate(effort       = case_when(method %in% time_methods   ~ effort_transect / 3600,
                                    method %in% number_methods ~ effort_transect),
           cpue_transect = n_fish / effort) %>%
    group_by(type, area, common_name, method, waterbody_type) %>%
    summarize(N     = n(),
              mean  = mean(cpue_transect, na.rm = TRUE),
              se    = se(cpue_transect, na.rm = TRUE),
              `5%`  = quantile(cpue_transect, probs = 0.05, na.rm = TRUE),
              `25%` = quantile(cpue_transect, probs = 0.25, na.rm = TRUE),
              `50%` = quantile(cpue_transect, probs = 0.50, na.rm = TRUE),
              `75%` = quantile(cpue_transect, probs = 0.75, na.rm = TRUE),
              `95%` = quantile(cpue_transect, probs = 0.95, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(metric = "CPUE")
}

calculate_cpue <- function(df) {
  if (all(is.na(df$effort))) {
    return(tibble(type = character(), area = character(), common_name = character(),
                  method = character(), waterbody_type = character(),
                  mean = double(), se = double(),
                  `5%` = double(), `25%` = double(), `50%` = double(),
                  `75%` = double(), `95%` = double(), metric = character()))
  }
  stopifnot(c("type", "area", "common_name", "method", "waterbody_type",
              "year", "effort", "transect_or_net_set") %in% colnames(df))
  bind_rows(
    .cpue_block(df, "overall", .psd_overall),
    .cpue_block(df, "lentic",  .psd_lentic),
    .cpue_block(df, "lotic",   .psd_lotic),
    .cpue_block(df, NA,        NULL)
  )
}

# ---- Length Frequency -----------------------------------------------------

.lf_block <- function(df, fsa_val, psd_grp) {
  .filter_gcat(df, fsa_val, psd_grp) %>%
    filter(gcat != "substock") %>%
    group_by(type, area, common_name, method, waterbody_type, year) %>%
    count(gcat) %>%
    mutate(lenfreq = (n / sum(n)) * 100) %>%
    group_by(type, area, common_name, method, waterbody_type, gcat) %>%
    summarize_at(vars(lenfreq), list(mean = mean, se = FSA::se), na.rm = TRUE) %>%
    mutate(metric = "Length Frequency",
           gcat   = factor(gcat, levels = c("stock", "quality", "preferred",
                                            "memorable", "trophy"))) %>%
    ungroup()
}

calculate_lf <- function(df) {
  stopifnot(c("type", "area", "common_name", "method", "waterbody_type",
              "year", "total_length") %in% colnames(df))
  bind_rows(
    .lf_block(df, "overall", .psd_overall),
    .lf_block(df, "lentic",  .psd_lentic),
    .lf_block(df, "lotic",   .psd_lotic),
    .lf_block(df, NA,        NULL)
  )
}

# ---- Relative Weight -------------------------------------------------------

.rw_block <- function(df, fsa_val, psd_grp, wr_grp = NULL) {
  df2 <- .filter_gcat(df, fsa_val, psd_grp)
  df2 <- if (is.null(wr_grp)) {
    mutate(df2, relweight = as.numeric(FSA::wrAdd(wt = weight, len = total_length, spec = common_name)))
  } else {
    mutate(df2, relweight = as.numeric(FSA::wrAdd(wt = weight, len = total_length, spec = common_name, WsOpts = wr_grp)))
  }
  df2 %>%
    group_by(type, area, common_name, method, waterbody_type, gcat) %>%
    summarize(N     = n(),
              mean  = mean(relweight, na.rm = TRUE),
              se    = FSA::se(relweight, na.rm = TRUE),
              `5%`  = quantile(relweight, probs = 0.05, na.rm = TRUE),
              `25%` = quantile(relweight, probs = 0.25, na.rm = TRUE),
              `50%` = quantile(relweight, probs = 0.50, na.rm = TRUE),
              `75%` = quantile(relweight, probs = 0.75, na.rm = TRUE),
              `95%` = quantile(relweight, probs = 0.95, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(mean   = ifelse(is.nan(mean), NA, mean),
           metric = "Relative Weight",
           gcat   = factor(gcat, levels = c("stock", "quality", "preferred",
                                            "memorable", "trophy"))) %>%
    filter(!is.na(gcat))
}

calculate_rw <- function(df) {
  if (all(is.na(df$weight))) {
    return(tibble(type = character(), area = character(), common_name = character(),
                  method = character(), waterbody_type = character(), gcat = factor(),
                  mean = double(), se = double(),
                  `5%` = double(), `25%` = double(), `50%` = double(),
                  `75%` = double(), `95%` = double(), metric = character()))
  }
  stopifnot(c("type", "area", "common_name", "method", "waterbody_type", "year",
              "total_length", "weight") %in% colnames(df))
  bind_rows(
    .rw_block(df, "overall", .psd_overall, NULL),
    .rw_block(df, "lentic",  .psd_lentic,  .wr_lentic),
    .rw_block(df, "lotic",   .psd_lotic,   .wr_lotic),
    .rw_block(df, NA,        NULL,          NULL)
  )
}

# ---- Total N (stock-size and above only) ----------------------------------

calculate_total_n <- function(df) {
  bind_rows(
    .filter_gcat(df, "overall", .psd_overall) %>% filter(gcat != "substock"),
    .filter_gcat(df, "lentic",  .psd_lentic)  %>% filter(gcat != "substock"),
    .filter_gcat(df, "lotic",   .psd_lotic)   %>% filter(gcat != "substock"),
    .filter_gcat(df, NA,        NULL)          %>% filter(gcat != "substock")
  ) %>%
    group_by(type, area, common_name, method, waterbody_type) %>%
    summarize(N = n(), .groups = "drop")
}
