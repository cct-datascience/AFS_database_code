# Percentiles for length frequency (incremental PSD categories) and for
# traditional PSD and PSD-P, by species x method x waterbody type, for North
# America, each ecoregion, and each state.
#
# The original loop_*_length_sampleID.R scripts compute each survey's
# percentage of stock-length fish in each category and keep only the mean
# and SE across surveys. This script does the same per-survey calculation on
# the same input file and adds the 5th, 25th, 50th, 75th, and 95th
# percentiles, so the webtool's one-page report can flag size structure the
# way it flags CPUE and relative weight. Its means reproduce the Length
# Frequency means in standardized_fish_data_03272026.csv (checked by
# check_length_percentiles() below).
#
# Input: AFS_final_01132026.csv (AFS_outlier_data_cleaned_052824.csv with
# Ontario removed and Nevada relabeled to boat_electrofishing -- the file the
# length scripts read). When the NV/Ontario-updated length/weight file is
# adopted for the standardized data, point `input_file` at it and re-run.
#
# Output: app/standardized_length_percentiles.csv

suppressMessages({library(dplyr); library(readr); library(tidyr); library(stringr)})

input_file  <- "C:/Users/Bonar/OneDrive - University of Arizona/Documents/00 documents from work computer/Standard Sampling Book Second Edition/Erins data chapter/all large data file iterations/AFS_final_01132026.csv"
app_dir     <- "C:/Users/Bonar/OneDrive - University of Arizona/Documents/AFS_database_code_10_08_2026/app"
output_file <- file.path(app_dir, "standardized_length_percentiles.csv")

gcats <- c("stock", "quality", "preferred", "memorable", "trophy")
gcat_short <- c(stock = "S-Q", quality = "Q-P", preferred = "P-M", memorable = "M-T", trophy = "T")

d <- read_csv(input_file,
              col_select = c(state, ecoregion, ecoregion_name, common_name, method, waterbody_type,
                             watername_method_yearID, gcat, total_length_mm),
              col_types = cols(.default = col_character()), progress = FALSE) %>%
  filter(!is.na(total_length_mm), !is.na(waterbody_type), !is.na(common_name), gcat %in% gcats) %>%
  # The standardized data name Brook/Brown Trout without the FSA
  # "(lentic)"/"(lotic)" split; waterbody type already separates them.
  mutate(common_name = str_remove(common_name, " \\((lentic|lotic)\\)$"))

# Each survey's percentage of stock-length fish in each category (0 for a
# category with no fish), as in the loop scripts.
per_survey <- d %>%
  distinct(state, ecoregion, ecoregion_name, common_name, method, waterbody_type, watername_method_yearID) %>%
  # one row per survey: a survey belongs to one state/ecoregion
  group_by(common_name, method, waterbody_type, watername_method_yearID) %>% slice(1) %>% ungroup() %>%
  inner_join(d %>% count(common_name, method, waterbody_type, watername_method_yearID, gcat),
             by = c("common_name", "method", "waterbody_type", "watername_method_yearID")) %>%
  complete(nesting(state, ecoregion, ecoregion_name, common_name, method, waterbody_type, watername_method_yearID),
           gcat = gcats, fill = list(n = 0)) %>%
  group_by(common_name, method, waterbody_type, watername_method_yearID) %>%
  mutate(pct = 100 * n / sum(n)) %>% ungroup()

# Traditional PSD (quality and larger) and PSD-P (preferred and larger) per survey.
per_survey_psd <- per_survey %>%
  group_by(state, ecoregion, ecoregion_name, common_name, method, waterbody_type, watername_method_yearID) %>%
  summarise(PSD = sum(pct[gcat != "stock"]), `PSD-P` = sum(pct[gcat %in% c("preferred", "memorable", "trophy")]),
            .groups = "drop") %>%
  pivot_longer(c(PSD, `PSD-P`), names_to = "gcat", values_to = "pct")

all_values <- bind_rows(per_survey %>% mutate(gcat = unname(gcat_short[gcat])) %>% select(-n), per_survey_psd)

summarise_area <- function(df) {
  df %>% group_by(area, common_name, method, waterbody_type, gcat) %>%
    summarise(N = n_distinct(watername_method_yearID), mean = mean(pct),
              `5%` = quantile(pct, .05), `25%` = quantile(pct, .25), `50%` = quantile(pct, .50),
              `75%` = quantile(pct, .75), `95%` = quantile(pct, .95), .groups = "drop")
}

# Ecoregion areas: the `ecoregion` column is the Level I code, labeled in
# the standardized data as "<code> <Level I name>" (e.g. "8 Eastern
# Temperate Forests") -- the labels are taken from that file so they match.
# Missing/NULL/0 codes are excluded, as in the ecoregion loop script.
eco_labels <- read_csv(file.path(app_dir, "standardized_fish_data_03272026.csv"), show_col_types = FALSE) %>%
  distinct(area) %>% filter(str_detect(area, "^[0-9]+ ")) %>%
  mutate(code = str_extract(area, "^[0-9]+")) %>% distinct(code, .keep_all = TRUE)
eco <- all_values %>%
  filter(!is.na(ecoregion), ecoregion != "NULL", ecoregion != "0") %>%
  inner_join(eco_labels, by = c("ecoregion" = "code"))

out <- bind_rows(
  summarise_area(all_values %>% mutate(area = "North America")),
  summarise_area(eco),
  summarise_area(all_values %>% filter(!is.na(state)) %>% mutate(area = state))
) %>% mutate(across(c(mean, `5%`, `25%`, `50%`, `75%`, `95%`), ~ round(.x, 1)))

write_csv(out, output_file)
cat("wrote", nrow(out), "rows to", output_file, "\n")

# Compares this script's means and N with the Length Frequency rows the
# webtool already uses.
check_length_percentiles <- function() {
  m <- read_csv(file.path(app_dir, "standardized_fish_data_03272026.csv"), show_col_types = FALSE) %>%
    filter(metric == "Length Frequency") %>%
    mutate(gcat = recode(gcat, "Stock-Quality" = "S-Q", "Quality-Preferred" = "Q-P", "Preferred-Memorable" = "P-M",
                         "Memorable-Trophy" = "M-T", "Trophy" = "T"),
           method = str_replace_all(method, " ", "_"), waterbody_type = str_replace_all(waterbody_type, " ", "_"))
  j <- m %>% inner_join(out, by = c("area", "common_name", "method", "waterbody_type", "gcat"), suffix = c("_app", "_new"))
  cat("app LF rows:", nrow(m), "| matched:", nrow(j),
      "| mean within 0.15:", sum(abs(j$mean_app - j$mean_new) <= 0.15),
      "| N equal:", sum(j$N_app == j$N_new), "\n")
  invisible(list(unmatched = anti_join(m, out, by = c("area", "common_name", "method", "waterbody_type", "gcat")),
                 differ = j %>% filter(abs(mean_app - mean_new) > 0.15 | N_app != N_new)))
}
chk <- check_length_percentiles()
