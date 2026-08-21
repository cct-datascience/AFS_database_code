time_methods   <- c("boat_electrofishing", "raft_electrofishing")
number_methods <- c("gill_net_fall", "gill_net_spring", "drifting_trammel_net",
                    "large_catfish_hoopnet", "bag_seine", "stream_seine",
                    "backpack_electrofishing", "tow_barge_electrofishing")

# CPUE unit label for a given collection method (and waterbody type, for bag_seine
# which uses a different unit in small standing waters vs. rivers)
cpue_unit_label <- function(method, waterbody_type = NA_character_) {
  case_when(
    method %in% c("boat_electrofishing", "raft_electrofishing") ~ "fish/hr",
    method %in% c("gill_net_fall", "gill_net_spring") ~ "fish/net night",
    method == "drifting_trammel_net" ~ "fish/100-m drift",
    method %in% c("large_catfish_hoopnet", "large_catfish_hoop_net", "hoop_net") ~ "fish/24-hr set",
    method == "bag_seine" & waterbody_type == "small_standing_waters" ~ "fish/0.25 arc",
    method == "bag_seine" ~ "fish/0.5 arc",
    method == "stream_seine" ~ "fish/10-15m haul",
    method %in% c("backpack_electrofishing", "tow_barge_electrofishing") ~ "fish/100m²",
    TRUE ~ NA_character_
  )
}

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

# Build the universe of every net/transect actually fished (independent of
# species), then cross it with each species present so every species gets a
# row for every net -- zero-filled where that species wasn't caught there.
# Effort was still expended at a zero-catch net, so it must count toward both
# the number of transects/nets averaged and total effort, or CPUE is
# systematically overestimated for any species not caught in every single
# transect/net (the norm, not the exception, in real survey data).
.per_net_with_zeros <- function(df, filtered) {
  net_universe <- df %>%
    group_by(type, area, waterbody_name, year, method, waterbody_type, transect_or_net_set) %>%
    summarize(effort = mean(effort), .groups = "drop")

  species_list <- filtered %>%
    distinct(type, area, waterbody_name, year, method, waterbody_type, common_name)

  per_net_counts <- filtered %>%
    group_by(type, area, waterbody_name, year, method, waterbody_type, common_name, transect_or_net_set) %>%
    summarize(n_fish = n(), .groups = "drop")

  species_list %>%
    left_join(net_universe, by = c("type", "area", "waterbody_name", "year", "method", "waterbody_type"),
              relationship = "many-to-many") %>%
    left_join(per_net_counts, by = c("type", "area", "waterbody_name", "year", "method", "waterbody_type",
                                     "common_name", "transect_or_net_set")) %>%
    mutate(n_fish = coalesce(n_fish, 0L))
}

.cpue_block <- function(df, fsa_val, psd_grp) {
  filtered <- .filter_gcat(df, fsa_val, psd_grp) %>% filter(gcat != "substock")

  .per_net_with_zeros(df, filtered) %>%
    mutate(effort       = case_when(method %in% time_methods   ~ effort / 3600,
                                    method %in% number_methods ~ effort),
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
    mutate(metric = "CPUE", cpue_calc = "transect",
           cpue_method = "Per-Transect Mean (± SE)")
}

calculate_cpue <- function(df) {
  if (all(is.na(df$effort))) {
    return(tibble(type = character(), area = character(), common_name = character(),
                  method = character(), waterbody_type = character(),
                  mean = double(), se = double(),
                  `5%` = double(), `25%` = double(), `50%` = double(),
                  `75%` = double(), `95%` = double(), metric = character(),
                  cpue_calc = character(), cpue_method = character()))
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

# ---- CPUE (pooled total: total fish / total effort, no SE) ----------------

.cpue_total_block <- function(df, fsa_val, psd_grp) {
  filtered <- .filter_gcat(df, fsa_val, psd_grp) %>% filter(gcat != "substock")

  .per_net_with_zeros(df, filtered) %>%
    mutate(effort = case_when(method %in% time_methods   ~ effort / 3600,
                              method %in% number_methods ~ effort)) %>%
    group_by(type, area, common_name, method, waterbody_type) %>%
    summarize(total_fish   = sum(n_fish),
              total_effort = sum(effort),
              N            = total_fish,
              mean         = total_fish / total_effort,
              se           = NA_real_,
              `5%`         = NA_real_,
              `25%`        = NA_real_,
              `50%`        = NA_real_,
              `75%`        = NA_real_,
              `95%`        = NA_real_,
              .groups = "drop") %>%
    select(-total_fish, -total_effort) %>%
    mutate(metric = "CPUE", cpue_calc = "total",
           cpue_method = paste0("Total Catch of ", common_name, "/Total Effort"))
}

calculate_cpue_total <- function(df) {
  if (all(is.na(df$effort))) {
    return(tibble(type = character(), area = character(), common_name = character(),
                  method = character(), waterbody_type = character(),
                  N = double(), mean = double(), se = double(),
                  `5%` = double(), `25%` = double(), `50%` = double(),
                  `75%` = double(), `95%` = double(), metric = character(),
                  cpue_calc = character(), cpue_method = character()))
  }
  stopifnot(c("type", "area", "common_name", "method", "waterbody_type",
              "year", "effort", "transect_or_net_set") %in% colnames(df))
  bind_rows(
    .cpue_total_block(df, "overall", .psd_overall),
    .cpue_total_block(df, "lentic",  .psd_lentic),
    .cpue_total_block(df, "lotic",   .psd_lotic),
    .cpue_total_block(df, NA,        NULL)
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

.wr_add_safe <- function(wt, len, spec) {
  # Some species (e.g. Walleye) have more than one standard-weight equation on
  # file in FSA, which makes a plain wrAdd() call error out rather than guess.
  # Retry with the "overall" equation as a sensible default in that case,
  # instead of letting the error crash the entire calculation for every
  # species/metric sharing this reactive.
  tryCatch(
    as.numeric(FSA::wrAdd(wt = wt, len = len, spec = spec)),
    error = function(e) {
      tryCatch(
        as.numeric(FSA::wrAdd(wt = wt, len = len, spec = spec,
                              WsOpts = setNames(list(list(group = "overall")), spec[1]))),
        error = function(e2) rep(NA_real_, length(wt))
      )
    }
  )
}

.rw_block <- function(df, fsa_val, psd_grp, wr_grp = NULL) {
  df2 <- .filter_gcat(df, fsa_val, psd_grp)
  df2 <- if (is.null(wr_grp)) {
    df2 %>%
      group_by(common_name) %>%
      mutate(relweight = .wr_add_safe(weight, total_length, common_name)) %>%
      ungroup()
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
