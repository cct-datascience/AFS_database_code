time_methods   <- c("boat_electrofishing", "raft_electrofishing")
number_methods <- c("gill_net_fall", "gill_net_spring", "drifting_trammel_net",
                    "large_catfish_hoopnet", "bag_seine", "stream_seine",
                    "backpack_electrofishing", "tow_barge_electrofishing")

# ---- Multi-year plot labeling -----------------------------------------------

# Labels each row from bind_rows(uu_processed(), filtered3(), .id="id") as
# "Standardized" or, for the user's own data, either ALL_YEARS_LABEL (the pooled
# row) or the literal year -- lets the Results Plots show every year the
# user selected side by side, the same as the Results Table, instead of
# being restricted to a single series.
# How the pooled all-years series is labeled on plots.
ALL_YEARS_LABEL <- "All years combined"

label_data_source <- function(df) {
  df %>% mutate(data_source = case_when(
    data_source == "1" & is.na(year) ~ ALL_YEARS_LABEL,
    data_source == "1" ~ as.character(year),
    data_source == "2" ~ "Standardized"
  ))
}

# Stable order for the (possibly several) user-data series: ALL_YEARS_LABEL
# first if present, then years ascending. Standardized is deliberately not
# included -- each plot already has its own established neutral color for
# it and lists it separately.
user_source_levels <- function(df) {
  labels <- df %>% filter(data_source != "Standardized") %>% distinct(data_source) %>% pull(data_source)
  is_all <- labels == ALL_YEARS_LABEL
  labels[order(!is_all, suppressWarnings(as.numeric(ifelse(is_all, NA, labels))))]
}

# ---- Publication-style plot colors ---------------------------------------------
# Grayscale only, so plots print correctly in black-and-white journals and read
# the same for colorblind viewers. The standardized (geographic average) series
# is always solid black so it stands out; the user's own series are white (all
# years combined, or a single survey) and then lighter-to-darker grays by year,
# each outlined in black. Point plots also vary the marker shape per series, so
# series stay distinguishable without relying on shade alone.
STANDARDIZED_FILL <- "black"

user_series_fills <- function(user_levels) {
  fills <- setNames(rep("white", length(user_levels)), user_levels)
  years <- user_levels[user_levels != ALL_YEARS_LABEL]
  # A single series (one year, no pooled row) is plain white.
  if (length(years) > 0 && length(user_levels) > 1) {
    fills[years] <- if (length(years) == 1) "grey70" else grey.colors(length(years), start = 0.85, end = 0.45)
  }
  fills
}

# Marker shapes (21-25 take a fill and a black outline). The standardized
# series uses a filled circle (21); user series use the other shapes in turn.
STANDARDIZED_SHAPE <- 21
user_series_shapes <- function(user_levels) {
  setNames(rep(c(22, 24, 23, 25), length.out = length(user_levels)), user_levels)
}

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

# psdAdd() computes Gabelhouse/PSD length categories for a whole vector of
# species at once; if any one of them hits a case it can't handle (a species
# needing a `group=` this bucket didn't supply, a species with no PSDlit
# entry at all, or any other internal FSA error), it throws for the ENTIRE
# call -- which previously took down every metric, every plot, and every
# download for the whole upload, not just the one problem species. Falling
# back to NA (with a warning so the cause is still visible in the logs) lets
# everything else compute normally instead.
.psd_add_safe <- function(len, spec, group = NULL) {
  tryCatch(
    if (is.null(group)) {
      FSA::psdAdd(len, spec, what = "incremental")
    } else {
      FSA::psdAdd(len, spec, what = "incremental", group = group)
    },
    error = function(e) {
      warning("psdAdd() failed for species [", paste(unique(spec), collapse = ", "),
              "]: ", conditionMessage(e), call. = FALSE)
      factor(rep(NA_character_, length(len)),
             levels = c("substock", "stock", "quality", "preferred", "memorable", "trophy"))
    }
  )
}

# Shared helper: filter by FSA_group and assign PSD length category
.filter_gcat <- function(df, fsa_val, psd_grp = NULL) {
  df <- if (is.na(fsa_val)) filter(df, is.na(FSA_group)) else filter(df, FSA_group == fsa_val)
  mutate(df, gcat = .psd_add_safe(total_length, common_name, psd_grp))
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

.summarize_cpue <- function(grouped) {
  grouped %>%
    summarize(N     = n(),
              mean  = mean(cpue_transect, na.rm = TRUE),
              se    = se(cpue_transect, na.rm = TRUE),
              `5%`  = quantile(cpue_transect, probs = 0.05, na.rm = TRUE),
              `25%` = quantile(cpue_transect, probs = 0.25, na.rm = TRUE),
              `50%` = quantile(cpue_transect, probs = 0.50, na.rm = TRUE),
              `75%` = quantile(cpue_transect, probs = 0.75, na.rm = TRUE),
              `95%` = quantile(cpue_transect, probs = 0.95, na.rm = TRUE),
              .groups = "drop")
}

# Produces both a pooled row (year = NA, identical to averaging across every
# year in the upload -- unchanged from before per-year breakdowns existed)
# and one row per year, so a multi-year upload can be viewed as one combined
# trend or year by year without re-uploading.
.cpue_block <- function(df, fsa_val, psd_grp) {
  filtered <- .filter_gcat(df, fsa_val, psd_grp) %>% filter(gcat != "substock")

  base <- .per_net_with_zeros(df, filtered) %>%
    mutate(effort        = case_when(method %in% time_methods   ~ effort / 3600,
                                     method %in% number_methods ~ effort),
           cpue_transect = n_fish / effort)

  pooled  <- base %>% group_by(type, area, common_name, method, waterbody_type) %>%
    .summarize_cpue() %>% mutate(year = NA_integer_)
  by_year <- base %>% group_by(type, area, common_name, method, waterbody_type, year) %>%
    .summarize_cpue()

  bind_rows(pooled, by_year) %>%
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

.summarize_cpue_total <- function(grouped) {
  grouped %>%
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
    select(-total_fish, -total_effort)
}

.cpue_total_block <- function(df, fsa_val, psd_grp) {
  filtered <- .filter_gcat(df, fsa_val, psd_grp) %>% filter(gcat != "substock")

  base <- .per_net_with_zeros(df, filtered) %>%
    mutate(effort = case_when(method %in% time_methods   ~ effort / 3600,
                              method %in% number_methods ~ effort))

  pooled  <- base %>% group_by(type, area, common_name, method, waterbody_type) %>%
    .summarize_cpue_total() %>% mutate(year = NA_integer_)
  by_year <- base %>% group_by(type, area, common_name, method, waterbody_type, year) %>%
    .summarize_cpue_total()

  bind_rows(pooled, by_year) %>%
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
  per_year <- .filter_gcat(df, fsa_val, psd_grp) %>%
    filter(gcat != "substock") %>%
    group_by(type, area, common_name, method, waterbody_type, year) %>%
    count(gcat) %>%
    mutate(lenfreq = (n / sum(n)) * 100) %>%
    ungroup()

  # Pooled: average of each year's proportion, same as before per-year
  # breakdowns existed (se reflects year-to-year variability).
  pooled <- per_year %>%
    group_by(type, area, common_name, method, waterbody_type, gcat) %>%
    summarize_at(vars(lenfreq), list(mean = mean, se = FSA::se), na.rm = TRUE) %>%
    ungroup() %>%
    mutate(year = NA_integer_)

  # By year: that year's own proportion is the only value there is, so
  # there's nothing to take a standard error across.
  by_year <- per_year %>%
    transmute(type, area, common_name, method, waterbody_type, gcat, year,
              mean = lenfreq, se = NA_real_)

  bind_rows(pooled, by_year) %>%
    mutate(metric = "Length Frequency",
           gcat   = factor(gcat, levels = c("stock", "quality", "preferred",
                                            "memorable", "trophy")))
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

.summarize_rw <- function(grouped) {
  grouped %>%
    summarize(N     = n(),
              mean  = mean(relweight, na.rm = TRUE),
              se    = FSA::se(relweight, na.rm = TRUE),
              `5%`  = quantile(relweight, probs = 0.05, na.rm = TRUE),
              `25%` = quantile(relweight, probs = 0.25, na.rm = TRUE),
              `50%` = quantile(relweight, probs = 0.50, na.rm = TRUE),
              `75%` = quantile(relweight, probs = 0.75, na.rm = TRUE),
              `95%` = quantile(relweight, probs = 0.95, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(mean = ifelse(is.nan(mean), NA, mean))
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

  pooled  <- df2 %>% group_by(type, area, common_name, method, waterbody_type, gcat) %>%
    .summarize_rw() %>% mutate(year = NA_integer_)
  by_year <- df2 %>% group_by(type, area, common_name, method, waterbody_type, gcat, year) %>%
    .summarize_rw()

  bind_rows(pooled, by_year) %>%
    mutate(metric = "Relative Weight",
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
  base <- bind_rows(
    .filter_gcat(df, "overall", .psd_overall) %>% filter(gcat != "substock"),
    .filter_gcat(df, "lentic",  .psd_lentic)  %>% filter(gcat != "substock"),
    .filter_gcat(df, "lotic",   .psd_lotic)   %>% filter(gcat != "substock"),
    .filter_gcat(df, NA,        NULL)          %>% filter(gcat != "substock")
  )

  pooled  <- base %>% group_by(type, area, common_name, method, waterbody_type) %>%
    summarize(N = n(), .groups = "drop") %>% mutate(year = NA_integer_)
  by_year <- base %>% group_by(type, area, common_name, method, waterbody_type, year) %>%
    summarize(N = n(), .groups = "drop")

  bind_rows(pooled, by_year)
}
