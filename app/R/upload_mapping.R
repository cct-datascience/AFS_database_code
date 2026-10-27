# Helpers for the "smart upload" mapping step in Compare Your Data: guess how a
# user's raw column headers and species names correspond to the required
# schema/vocabulary, so the user only has to confirm/correct a suggestion
# instead of reformatting their file by hand.

# ---- Column header matching -------------------------------------------------

# Common alternate spellings for each required column. Deliberately generic
# terms only appear where they're unambiguous for that field.
column_aliases <- list(
  state               = c("state", "province", "state_province", "st"),
  ecoregion           = c("ecoregion", "eco_region", "ecoregion_name", "epa_ecoregion"),
  waterbody_name       = c("waterbody_name", "waterbody", "water_body", "water_body_name",
                            "water_name", "lake", "lake_name", "site", "site_name", "location", "waterway"),
  common_name         = c("common_name", "species", "species_name", "fish_species", "commonname",
                          "spp", "spp_code", "species_code", "sp_code", "scientific_name",
                          "taxa", "taxon"),
  method              = c("method", "gear", "gear_type", "collection_method", "sampling_method"),
  waterbody_type      = c("waterbody_type", "water_type", "habitat_type", "wb_type", "habitat"),
  year                = c("year", "yr", "sample_year", "survey_year", "collection_year"),
  transect_or_net_set = c("transect_or_net_set", "transect", "transect_id", "net_set",
                           "net_id", "set_id", "sample_id", "station", "station_id"),
  effort              = c("effort", "sampling_effort", "duration", "effort_units", "effort_unit",
                          "seconds", "sec", "effort_seconds", "effort_sec", "shock_seconds",
                          "shock_time", "shocking_seconds", "seconds_shocked", "pedal_time", "on_time"),
  total_length        = c("total_length", "length", "tl", "total_length_mm", "length_mm"),
  weight              = c("weight", "wt", "weight_g", "mass"),
  collection_date     = c("collection_date", "date", "sample_date", "survey_date", "date_collected", "date_sampled",
                          "start_date"),
  latitude            = c("latitude", "lat", "y"),
  longitude           = c("longitude", "long", "lon", "lng", "x")
)

# ---- Automatic coordinate detection ------------------------------------------

# Finds latitude/longitude in the raw upload without the user pointing at a
# column: by name first, then by scanning column VALUES for numbers in a
# plausible North America range (handles an oddly-named single lat/lon pair),
# and finally by combining a degrees/minutes/seconds split across separate
# columns into decimal degrees. Returns list(lat = <numeric vector|NULL>,
# lon = <numeric vector|NULL>) -- NULL for either means it wasn't found.
detect_coordinates <- function(df) {
  headers <- names(df)
  as_num  <- function(col) suppressWarnings(as.numeric(df[[col]]))
  in_range <- function(v, lo, hi) !is.na(v) & v >= lo & v <= hi

  # 1) Name-based match (state/ecoregion column_aliases), sanity-checked
  # against a plausible range so a wrongly-matched column isn't trusted.
  lat_col <- match_column(headers, "latitude")
  lon_col <- match_column(headers, "longitude")
  if (!is.na(lat_col) && !is.na(lon_col)) {
    lat <- as_num(lat_col); lon <- as_num(lon_col)
    if (any(in_range(lat, -90, 90)) && any(in_range(lon, -180, 180))) {
      return(list(lat = lat, lon = lon))
    }
  }

  # 2) Value-range scan across every column: latitude ~15-72degN, longitude
  # ~50-180degW covers all of North America (incl. Alaska/Hawaii), regardless
  # of what the column happens to be named.
  is_mostly_numeric <- function(col) {
    v <- df[[col]]
    if (is.numeric(v)) return(TRUE)
    mean(!is.na(suppressWarnings(as.numeric(v)))) > 0.8
  }
  numeric_cols <- Filter(is_mostly_numeric, headers)

  matches_range <- function(cols, lo, hi) {
    Filter(function(col) {
      v <- as_num(col)
      any(!is.na(v)) && mean(in_range(v, lo, hi)) > 0.9
    }, cols)
  }
  lat_candidates <- matches_range(numeric_cols, 15, 72)
  lon_candidates <- matches_range(setdiff(numeric_cols, lat_candidates), -180, -50)

  if (length(lat_candidates) > 0 && length(lon_candidates) > 0) {
    return(list(lat = as_num(lat_candidates[1]), lon = as_num(lon_candidates[1])))
  }

  # 3) Degrees/minutes/seconds split across separate columns (e.g.
  # lat_deg/lat_min/lat_sec, lon_degrees/lon_minutes/lon_seconds).
  dms <- .detect_dms_coordinates(df, headers)
  if (!is.null(dms)) return(dms)

  list(lat = NULL, lon = NULL)
}

# Looks for <axis>_<deg|min|sec> style column triples for latitude and
# longitude and combines them into decimal degrees. Assumes latitude is
# North and longitude is West unless a same-row direction column (values
# like "N"/"S" or "E"/"W") says otherwise -- true for every location in
# North America, which is what this app covers.
.detect_dms_coordinates <- function(df, headers) {
  norm <- .normalize_name(headers)

  find_part <- function(axis_pat, part_pat) {
    hit <- headers[grepl(axis_pat, norm) & grepl(part_pat, norm)]
    if (length(hit) >= 1) hit[1] else NA_character_
  }
  # By VALUES, not name: a naive name match (e.g. "ns" as a substring) is too
  # easily triggered by an unrelated header that happens to contain those
  # letters -- "Lon_Sec" normalizes to "lonsec", which contains "ns". A
  # direction column's actual values are a small, distinctive set, so check
  # those instead.
  find_dir_col <- function(candidate_cols, valid_dirs) {
    for (col in candidate_cols) {
      v <- toupper(trimws(as.character(df[[col]])))
      v <- v[!is.na(v) & v != ""]
      if (length(v) > 0 && all(v %in% valid_dirs)) return(col)
    }
    NA_character_
  }

  lat_deg <- find_part("lat", "deg"); lat_min <- find_part("lat", "min")
  lat_sec <- find_part("lat", "sec")
  lon_deg <- find_part("lon", "deg"); lon_min <- find_part("lon", "min")
  lon_sec <- find_part("lon", "sec")

  other_cols <- setdiff(headers, c(lat_deg, lat_min, lat_sec, lon_deg, lon_min, lon_sec))
  lat_dir <- find_dir_col(other_cols, c("N", "S", "NORTH", "SOUTH"))
  lon_dir <- find_dir_col(other_cols, c("E", "W", "EAST", "WEST"))

  if (is.na(lat_deg) || is.na(lon_deg)) return(NULL)

  to_decimal <- function(deg_col, min_col, sec_col, dir_col, negate_dirs, default_sign) {
    deg <- suppressWarnings(as.numeric(df[[deg_col]]))
    min <- if (!is.na(min_col)) suppressWarnings(as.numeric(df[[min_col]])) else 0
    sec <- if (!is.na(sec_col)) suppressWarnings(as.numeric(df[[sec_col]])) else 0
    dec <- abs(deg) + coalesce(min, 0) / 60 + coalesce(sec, 0) / 3600
    sign <- if (!is.na(dir_col)) {
      ifelse(toupper(substr(as.character(df[[dir_col]]), 1, 1)) %in% negate_dirs, -1, 1)
    } else if (any(deg < 0, na.rm = TRUE)) {
      sign(deg)
    } else {
      default_sign
    }
    dec * sign
  }

  list(
    # Default sign when there's no direction column and degrees are given
    # unsigned: North America is entirely north latitude (+) and, aside from
    # a sliver of the Aleutians, west longitude (-).
    lat = to_decimal(lat_deg, lat_min, lat_sec, lat_dir, negate_dirs = "S", default_sign = 1),
    lon = to_decimal(lon_deg, lon_min, lon_sec, lon_dir, negate_dirs = "W", default_sign = -1)
  )
}

# ---- State/ecoregion from a lat/long coordinate -----------------------------

# Point-in-polygon lookup against the state/province and ecoregion polygons
# loaded in app.R (states_lookup, ecoregions_trans, ecoregion_code_lookup) --
# for a user whose file has coordinates but no state/ecoregion column. Points
# that don't fall inside any polygon (bad coordinates, or just offshore/at a
# boundary) come back NA rather than erroring, same as an unmapped field.
# states_poly/ecoregions_poly/eco_code_lookup are passed in explicitly (built
# once in app.R at startup) rather than referenced as globals from here --
# this file is sourced separately from app.R, and a helper here can't rely on
# top-level objects app.R defines later being visible in its scope.
lookup_state_ecoregion <- function(lat, lon, states_poly, ecoregions_poly, eco_code_lookup) {
  pts <- tibble(.row = seq_along(lat), lat = suppressWarnings(as.numeric(lat)),
               lon = suppressWarnings(as.numeric(lon)))
  valid <- !is.na(pts$lat) & !is.na(pts$lon)

  out <- tibble(.row = pts$.row, state = NA_character_, ecoregion = NA_character_)
  if (!any(valid)) return(out)

  pts_sf <- sf::st_as_sf(pts[valid, ], coords = c("lon", "lat"), crs = 4326)

  state_hit <- sf::st_join(pts_sf, states_poly)$name
  eco_code  <- sf::st_join(pts_sf, ecoregions_poly["NA_L1CODE"])$NA_L1CODE
  eco_hit   <- unname(eco_code_lookup[eco_code])

  out$state[valid]     <- state_hit
  out$ecoregion[valid] <- eco_hit
  out
}

# ---- Dates --------------------------------------------------------------

# Accepts whatever date format a user's column happens to use, trying a wide
# net of common orders. Returns NA (not an error) for anything unparseable,
# so a bad/blank date just falls back to whatever else was available.
parse_flexible_date <- function(date_value) {
  x <- trimws(as.character(date_value))
  # Excel drops the leading zero of a digits-only month-first date, so
  # 04212020 arrives as 4212020; put it back.
  x <- ifelse(grepl("^[0-9]{7}$", x), paste0("0", x), x)
  lubridate::parse_date_time(
    x,
    orders = c("ymd", "mdy", "dmy", "Ymd HMS", "mdy HMS", "ymd HMS", "BdY", "dBY", "my", "Ym")
  )
}

# A `year` column can be filled in from a collection date when there isn't one.
year_from_date <- function(date_value) {
  lubridate::year(parse_flexible_date(date_value))
}

# ---- Gill net season from a collection date ---------------------------------

# AFS convention (also documented in the app's upload instructions): a survey
# from January through June is "spring" gill netting, July through December is
# "fall" -- i.e. Jan 1-Jun 30 -> gill_net_spring, Jul 1-Dec 31 -> gill_net_fall.
gillnet_season_from_date <- function(date_value) {
  month <- lubridate::month(parse_flexible_date(date_value))
  ifelse(is.na(month), NA_character_,
         ifelse(month <= 6, "gill_net_spring", "gill_net_fall"))
}

.normalize_name <- function(x) {
  gsub("[^a-z0-9]", "", tolower(x))
}

.unit_words <- c("mm", "millimeter", "millimeters", "millimetre", "millimetres",
                 "cm", "centimeter", "centimeters", "centimetre", "centimetres",
                 "in", "inch", "inches", "g", "gm", "gms", "gr", "gram", "grams",
                 "kg", "kgs", "kilogram", "kilograms", "oz", "ounce", "ounces",
                 "lb", "lbs", "pound", "pounds")

# Header with any whole-word length/weight unit removed ("TL (mm)" -> "TL").
.strip_unit_words <- function(x) {
  vapply(strsplit(tolower(x), "[^a-z0-9]+"), function(tok) {
    paste(tok[tok != "" & !tok %in% .unit_words], collapse = "_")
  }, character(1))
}

# Whether a column's VALUES look like the field it's being matched to. A
# header match alone can be wrong -- "gear" is one letter from "year", and a
# blank "durationhr" column is still named like a duration -- and a bad guess
# for year or effort silently blanks out every result. Blank columns never
# fit; fields not listed below only need some non-blank values.
column_fits_field <- function(values, field) {
  v <- trimws(as.character(values))
  v <- v[!is.na(v) & v != ""]
  if (length(v) == 0) return(FALSE)
  v <- head(v, 500)  # plenty to judge by, and keeps date parsing quick
  num <- suppressWarnings(as.numeric(v))
  share_num <- mean(!is.na(num))
  switch(field,
    year            = share_num > 0.9 && mean(num >= 1900 & num <= 2100, na.rm = TRUE) > 0.9,
    effort          = ,
    total_length    = ,
    weight          = share_num > 0.9 && any(num > 0, na.rm = TRUE),
    # Dates may be plain digits ("11132010"), so check that they parse to a
    # plausible year rather than that they aren't numbers.
    collection_date = {
      yr <- lubridate::year(suppressWarnings(parse_flexible_date(v)))
      mean(!is.na(yr) & yr >= 1900 & yr <= 2100) > 0.9
    },
    common_name     = ,
    waterbody_name  = share_num < 0.5,
    TRUE)
}

# Best-guess raw header for a required field, or NA if nothing clears the bar.
# With `df` (the uploaded data), only columns whose values fit the field (see
# column_fits_field()) are considered, so a wrong name match falls through to
# the next candidate or to "no column" instead.
match_column <- function(raw_headers, canonical_field, alias_table = column_aliases, df = NULL) {
  aliases <- alias_table[[canonical_field]]
  if (is.null(aliases)) aliases <- canonical_field
  if (!is.null(df)) {
    raw_headers <- Filter(function(h) column_fits_field(df[[h]], canonical_field), raw_headers)
  }

  norm_headers <- .normalize_name(raw_headers)
  norm_aliases <- .normalize_name(aliases)

  hit <- raw_headers[norm_headers %in% norm_aliases]
  if (length(hit) > 0) return(hit[1])

  # Same again with length/weight unit words dropped from the header, so
  # "TL_mm", "Length (cm)", "Wt_lbs", "Weight (kg)" match like "tl",
  # "length", "wt", "weight" (the unit itself is picked up separately by
  # guess_unit_from_header()).
  hit <- raw_headers[.normalize_name(.strip_unit_words(raw_headers)) %in% norm_aliases]
  if (length(hit) > 0) return(hit[1])

  # Fuzzy fallback: whole-string edit distance normalized by the LONGER of
  # the two strings, not agrep(). agrep's max.distance is a fraction of the
  # pattern's length and matches an approximate SUBSTRING anywhere in the
  # target, so a short alias like "date" or "st" ends up "found" inside
  # almost any longer, unrelated header (e.g. "date" ~ "waterbody_name").
  # Normalizing by the longer string instead means a short alias can only
  # match a similarly-short header, which is what a typo correction should do.
  # A header that's an exact alias of a DIFFERENT field belongs to that
  # field, so it's never a typo of this one ("waterbody_type" is not a
  # misspelled "waterbody_name", nor "gear" a misspelled "year").
  other_aliases <- .normalize_name(unlist(alias_table[setdiff(names(alias_table), canonical_field)]))
  fuzzy_ok <- which(!norm_headers %in% other_aliases)
  best <- .closest_match(norm_aliases, norm_headers[fuzzy_ok], max_ratio = 0.25)
  if (!is.na(best)) return(raw_headers[fuzzy_ok[best]])

  NA_character_
}

# Index into `candidates` closest to any of `patterns` by edit distance
# normalized against the longer string, or NA if nothing clears max_ratio.
.closest_match <- function(patterns, candidates, max_ratio) {
  if (length(candidates) == 0) return(NA_integer_)
  best_idx <- NA_integer_
  best_ratio <- Inf
  for (p in patterns) {
    if (nchar(p) == 0) next
    d <- utils::adist(p, candidates)[1, ]
    ratio <- d / pmax(nchar(p), nchar(candidates))
    idx <- which.min(ratio)
    if (length(idx) > 0 && ratio[idx] < best_ratio) {
      best_ratio <- ratio[idx]
      best_idx <- idx
    }
  }
  if (best_ratio <= max_ratio) best_idx else NA_integer_
}

# ---- Species name matching --------------------------------------------------

# Scientific names and unambiguous common nicknames for the canonical species
# list. Generic terms shared by multiple species in the list (bare "bass",
# "trout", "catfish", "carp", "perch", "crappie", "pickerel", "sucker",
# "bullhead") are left out on purpose -- a wrong silent guess on those would
# be worse than falling through to fuzzy-match/manual review.
species_aliases <- list(
  "Arctic Grayling"            = c("Thymallus arcticus", "Grayling"),
  "Bigmouth Buffalo"           = c("Ictiobus cyprinellus", "Buffalofish"),
  "Black Bullhead"             = c("Ameiurus melas"),
  "Black Crappie"              = c("Pomoxis nigromaculatus", "Speckled Perch", "Calico Bass"),
  "Blue Catfish"                = c("Ictalurus furcatus", "Blue Cat"),
  "Bluegill"                    = c("Lepomis macrochirus", "Bream", "Brim", "BG"),
  "Brook Trout"                 = c("Salvelinus fontinalis", "Brookie", "Speckled Trout"),
  "Brown Bullhead"              = c("Ameiurus nebulosus"),
  "Brown Trout"                 = c("Salmo trutta", "German Brown"),
  "Bull Trout"                  = c("Salvelinus confluentus"),
  "Burbot"                      = c("Lota lota", "Eelpout", "Lawyer"),
  "Chain Pickerel"              = c("Esox niger"),
  "Channel Catfish"             = c("Ictalurus punctatus", "Channel Cat"),
  "Common Carp"                 = c("Cyprinus carpio", "European Carp"),
  "Cutthroat Trout"             = c("Oncorhynchus clarkii", "Cutthroat", "Cutt"),
  "Flathead Catfish"            = c("Pylodictis olivaris", "Mud Cat", "Shovelhead Cat"),
  "Freshwater Drum"             = c("Aplodinotus grunniens", "Sheepshead", "Gaspergou"),
  "Gizzard Shad"                = c("Dorosoma cepedianum"),
  "Grass Carp"                  = c("Ctenopharyngodon idella", "White Amur"),
  "Green Sunfish"               = c("Lepomis cyanellus"),
  "Lake Trout"                  = c("Salvelinus namaycush", "Laker", "Mackinaw"),
  "Largemouth Bass"             = c("Micropterus salmoides", "Micropterus nigricans", "LMB", "Bucketmouth"),
  "Longnose Gar"                = c("Lepisosteus osseus"),
  "Muskellunge"                 = c("Esox masquinongy", "Muskie", "Musky"),
  "Northern Pike"                = c("Esox lucius", "Jackfish"),
  "Paddlefish"                   = c("Polyodon spathula", "Spoonbill"),
  "Palmetto Bass"                = c("Hybrid Striped Bass", "Sunshine Bass"),
  "Pumpkinseed"                  = c("Lepomis gibbosus", "Punkinseed"),
  "Rainbow Trout"                = c("Oncorhynchus mykiss", "Steelhead"),
  "Redear Sunfish"               = c("Lepomis microlophus", "Shellcracker"),
  "River Carpsucker"             = c("Carpiodes carpio"),
  "Rock Bass"                    = c("Ambloplites rupestris", "Redeye", "Goggle-eye"),
  "Sauger"                       = c("Sander canadensis"),
  "Shorthead Redhorse"           = c("Moxostoma macrolepidotum", "Redhorse"),
  "Silver Carp"                  = c("Hypophthalmichthys molitrix", "Flying Carp"),
  "Smallmouth Bass"              = c("Micropterus dolomieu", "SMB", "Bronzeback", "Smallie"),
  "Spotted Bass"                 = c("Micropterus punctulatus", "Kentucky Bass"),
  "Spotted Gar"                  = c("Lepisosteus oculatus"),
  "Striped Bass (landlocked)"    = c("Morone saxatilis", "Striper", "Rockfish"),
  "Utah Chub"                    = c("Gila atraria"),
  "Walleye"                      = c("Sander vitreus", "Yellow Pike", "Walleyed Pike", "WE"),
  "Warmouth"                     = c("Lepomis gulosus"),
  "White Bass"                   = c("Morone chrysops", "Sand Bass"),
  "White Crappie"                = c("Pomoxis annularis"),
  "White Perch"                  = c("Morone americana"),
  "White Sucker"                 = c("Catostomus commersonii"),
  "Yellow Bass"                  = c("Morone mississippiensis"),
  "Yellow Bullhead"              = c("Ameiurus natalis"),
  "Yellow Perch"                 = c("Perca flavescens", "Lake Perch", "Ringed Perch", "YP")
)

# Many state/agency datasets code species with the standard 4-letter field
# code: first two letters of genus + first two letters of species epithet
# (e.g. Micropterus salmoides -> "MISA" for Largemouth Bass -- note this is
# NOT Smallmouth Bass, which is Micropterus dolomieu -> "MIDO"; it's easy to
# mix the two up by hand, which is exactly why these are derived from the
# scientific name already on file above rather than typed in separately).
# Generated once here instead of hand-transcribed, and any code that would
# collide across two different species is dropped rather than guessed.
.genus_species_code <- function(scientific_name) {
  parts <- strsplit(scientific_name, " ")[[1]]
  if (length(parts) != 2) return(NA_character_)
  toupper(paste0(substr(parts[1], 1, 2), substr(parts[2], 1, 2)))
}

.codes_by_species <- vapply(species_aliases, function(aliases) {
  sci <- aliases[grepl("^[A-Z][a-z]+ [a-z]+$", aliases)]
  if (length(sci) == 0) return(NA_character_)
  .genus_species_code(sci[1])
}, character(1))

.dupe_codes <- unique(.codes_by_species[duplicated(.codes_by_species) & !is.na(.codes_by_species)])
.codes_by_species[.codes_by_species %in% .dupe_codes] <- NA_character_

for (.sp in names(.codes_by_species)) {
  if (!is.na(.codes_by_species[[.sp]])) {
    species_aliases[[.sp]] <- c(species_aliases[[.sp]], .codes_by_species[[.sp]])
  }
}
rm(.sp, .codes_by_species, .dupe_codes)

.normalize_species <- function(x) {
  gsub("[^a-z0-9 ]", "", tolower(trimws(x)))
}

# Best-guess canonical species for a raw value, with a confidence tag so the
# UI can flag guesses that need a closer look ("exact" > "alias" > "fuzzy" >
# "none").
match_species <- function(raw_value, canonical_list, alias_table = species_aliases) {
  if (is.na(raw_value) || trimws(raw_value) == "") {
    return(list(match = NA_character_, confidence = "none"))
  }

  norm_val <- .normalize_species(raw_value)
  norm_canonical <- .normalize_species(canonical_list)

  idx <- which(norm_canonical == norm_val)
  if (length(idx) > 0) return(list(match = canonical_list[idx[1]], confidence = "exact"))

  for (canon in names(alias_table)) {
    if (!(canon %in% canonical_list)) next
    norm_aliases <- .normalize_species(alias_table[[canon]])
    if (norm_val %in% norm_aliases) return(list(match = canon, confidence = "alias"))
  }

  # Whole-string edit distance normalized by the longer string (see
  # .closest_match / match_column) rather than agrep, for the same reason:
  # a short raw value like "carp" is a substring of several unrelated
  # canonical names (e.g. "River Carpsucker"), and agrep's fraction-of-
  # pattern-length tolerance would call that a confident match.
  best <- .closest_match(norm_val, norm_canonical, max_ratio = 0.2)
  if (!is.na(best)) return(list(match = canonical_list[best], confidence = "fuzzy"))

  list(match = NA_character_, confidence = "none")
}

# ---- Transect numbers from effort --------------------------------------------

# Some files have no transect/net-set ID column, but every fish row carries its
# own transect's effort (e.g. 612 seconds of shocking), and the rows for one
# transect sit together. A new transect starts wherever that effort value
# changes from the row above (or the year/date changes). Numbered 1, 2, 3...
# within each year. A blank effort is treated as part of the transect above it.
# Limitation: two back-to-back transects with exactly the same effort can't be
# told apart this way and are counted as one.
transects_from_effort <- function(effort, year = NULL, date = NULL) {
  n <- length(effort)
  if (n == 0) return(character(0))
  if (is.null(year)) year <- rep(NA, n)
  if (is.null(date)) date <- rep(NA, n)

  eff  <- .fill_down(effort)
  key  <- paste(year, as.character(date), eff, sep = "\r")
  new_run <- c(TRUE, key[-1] != key[-n])

  # Restart numbering in each year.
  yr_key <- ifelse(is.na(year), "none", as.character(year))
  out <- integer(n)
  counts <- list()
  current <- 0L
  for (i in seq_len(n)) {
    if (new_run[i]) {
      k <- yr_key[i]
      counts[[k]] <- (if (is.null(counts[[k]])) 0L else counts[[k]]) + 1L
      current <- counts[[k]]
    }
    out[i] <- current
  }
  as.character(out)
}

# Carry the last non-missing value forward (leading NAs stay NA).
.fill_down <- function(x) {
  idx <- cumsum(!is.na(x))
  ifelse(idx == 0, NA, x[!is.na(x)][pmax(idx, 1)])
}

# ── Length/weight units ─────────────────────────────────────────────────────
# The analysis needs total length in mm and weight in g. Uploads may be in
# other units, so the user picks the unit on the Map & Review Data tab and
# the values are converted here.
length_units <- c("Millimeters (mm)" = "mm", "Centimeters (cm)" = "cm", "Inches (in)" = "in")
weight_units <- c("Grams (g)" = "g", "Kilograms (kg)" = "kg", "Ounces (oz)" = "oz", "Pounds (lb)" = "lb")

.to_mm <- c(mm = 1, cm = 10, `in` = 25.4)
.to_g  <- c(g = 1, kg = 1000, oz = 28.349523125, lb = 453.59237)

length_to_mm <- function(x, unit) x * unname(.to_mm[unit %||% "mm"])
weight_to_g  <- function(x, unit) x * unname(.to_g[unit %||% "g"])

# Unit hinted at by a column header, e.g. "TL_cm" -> "cm", "Weight (lbs)" ->
# "lb"; NA when the header doesn't say. Matched on whole tokens so e.g. the
# "g" in "length" or the "in" in "inch" count only where they stand alone.
guess_unit_from_header <- function(header, type = c("length", "weight")) {
  type <- match.arg(type)
  if (is.null(header) || is.na(header) || header == "") return(NA_character_)
  tokens <- strsplit(tolower(header), "[^a-z]+")[[1]]
  tokens <- tokens[tokens != ""]
  lookup <- if (type == "length") {
    list(mm = c("mm", "millimeter", "millimeters", "millimetre", "millimetres"),
         cm = c("cm", "centimeter", "centimeters", "centimetre", "centimetres"),
         `in` = c("in", "inch", "inches"))
  } else {
    list(g = c("g", "gm", "gms", "gram", "grams", "gr"),
         kg = c("kg", "kgs", "kilogram", "kilograms", "kilo", "kilos"),
         oz = c("oz", "ounce", "ounces"),
         lb = c("lb", "lbs", "pound", "pounds"))
  }
  # "in" alone is ambiguous in a header ("length in mm"), so only trust it
  # when no other length unit appears.
  hits <- names(lookup)[vapply(lookup, function(a) any(tokens %in% a), logical(1))]
  if (type == "length" && length(hits) > 1) hits <- setdiff(hits, "in")
  if (length(hits) == 1) hits else NA_character_
}
