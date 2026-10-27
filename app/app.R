library(shiny)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(leaflet)
library(DT)
library(leafpop)
library(FSA)
library(knitr)
library(ggtext)
library(readxl)
# jsonlite is used (via jsonlite::) but not attached with library() -- it
# exports its own validate(), which would silently shadow shiny::validate().
# library(periscope)

# ── APA (7th ed.) style for figures and tables ───────────────────────────────
# APA puts the figure/table number (bold) and title (italic, title case) ABOVE
# the figure or table, keeps titles out of the image itself, and puts any
# explanation in a "Note." paragraph below. Plots use plain black sans-serif
# text, no gridlines, and no legend box; tables use horizontal rules only.
apa_heading <- function(kind, number, title) {
  div(class = "apa-heading",
      div(strong(paste(kind, number))),
      div(em(title)))
}
apa_note <- function(...) div(class = "apa-note", em("Note."), " ", ...)

# Markdown -> HTML without the wrapping <p>, so a caption can run on inline
# after "Note." in the same paragraph.
md_inline <- function(text) {
  HTML(gsub("</?p>", "", markdown::markdownToHTML(text = text, fragment.only = TRUE)))
}

theme_apa <- function(base_size = 18) {
  theme_classic(base_size = base_size, base_family = "sans") +
    theme(text        = element_text(color = "black"),
          axis.text   = element_text(color = "black"),
          axis.line   = element_line(color = "black", linewidth = 0.6),
          axis.ticks  = element_line(color = "black", linewidth = 0.6),
          legend.background = element_rect(fill = "white", color = NA),
          legend.key  = element_blank(),
          legend.title = element_text(face = "plain"))
}

# Results tables in APA layout: top rule, rule under the column headings,
# bottom rule, nothing else (no vertical lines, stripes, or shading).
apa_table_css <- "
.apa-heading { font-size: 17px; margin: 18px 0 10px 0; line-height: 1.5; }
.apa-note { font-size: 15px; margin-top: 8px; }
table.dataTable.apa-table { border-collapse: collapse !important; border-top: 2px solid black !important;
  border-bottom: 2px solid black !important; font-size: 14px; }
table.dataTable.apa-table thead th { border-bottom: 1px solid black !important; border-top: none !important;
  font-weight: normal; background: white !important; }
table.dataTable.apa-table tbody td { border: none !important; background: white !important; box-shadow: none !important; }
table.dataTable.apa-table tbody tr { background: white !important; }
table.dataTable.apa-nowrap th, table.dataTable.apa-nowrap td { white-space: nowrap; }
table.dataTable.apa-table thead th { vertical-align: bottom; }
.dataTables_scrollBody table.dataTable.apa-table { border-top: none !important; }
.dataTables_scrollHead table.dataTable.apa-table { border-bottom: none !important; }
.dataTables_scrollBody table.dataTable.apa-table thead th { border-bottom: none !important; }
"

# Builds an APA-style datatable: headers given as HTML (so N, M, SE can be
# italic), numbers right-aligned with a consistent number of decimals.
# int_cols (e.g. N, year) are shown as whole numbers. scroll_x = FALSE is for
# narrow tables: they fit the page, wrap if the window is very small, show
# every row at once (no paging), and keep their APA row order (no sorting).
apa_datatable <- function(df, col_html, digits = 1, int_cols = c("N", "year"), page_length = 25,
                          scroll_x = TRUE) {
  num_names <- names(df)[vapply(df, is.numeric, logical(1))]
  int_names <- intersect(int_cols, num_names)
  dec_names <- setdiff(num_names, int_names)
  df <- df %>% mutate(across(where(is.character), ~ htmltools::htmlEscape(.x)))
  fits <- nrow(df) <= page_length
  dt <- datatable(df, class = paste("apa-table compact", if (scroll_x) "apa-nowrap"),
                  rownames = FALSE, escape = FALSE,
                  colnames = unname(col_html[names(df)]),
                  options = list(lengthChange = FALSE, pageLength = page_length,
                                 dom = if (fits) "t" else "tip", scrollX = scroll_x,
                                 ordering = scroll_x, autoWidth = FALSE,
                                 columnDefs = list(list(className = "dt-right",
                                                        targets = match(num_names, names(df)) - 1))))
  if (length(dec_names) > 0) dt <- formatRound(dt, dec_names, digits = digits)
  if ("N" %in% int_names) dt <- formatRound(dt, "N", digits = 0)
  if ("year" %in% int_names) dt <- formatRound(dt, "year", digits = 0, mark = "")
  dt
}

# Splits text written with <i>...</i> (and HTML-escaped characters) into
# plain/italic runs for Word: data.frame(text, italic).
html_italic_runs <- function(x) {
  tok <- regmatches(x, gregexpr("<i>.*?</i>|[^<]+|<", x, perl = TRUE))[[1]]
  text <- gsub("</?i>", "", tok)
  for (e in list(c("&lt;", "<"), c("&gt;", ">"), c("&quot;", "\""), c("&#39;", "'"), c("&amp;", "&"))) {
    text <- gsub(e[1], e[2], text, fixed = TRUE)
  }
  data.frame(text = text, italic = startsWith(tok, "<i>"))
}

# Writes the results tables to a .docx in APA (7th ed.) table style: bold
# "Table N", italic title on the next line, horizontal rules only (above and
# below the column headings, and at the bottom), and an italic "Note." below.
# Each table starts on its own page, as APA manuscripts require.
build_apa_docx <- function(tabs, col_html, file, font = "Times New Roman", size = 11) {
  fp <- function(...) officer::fp_text(font.family = font, font.size = size, ...)
  runs <- function(x, ...) {
    r <- html_italic_runs(x)
    lapply(seq_len(nrow(r)), function(j) officer::ftext(r$text[j], fp(italic = r$italic[j], ...)))
  }
  para <- function(chunks) do.call(officer::fpar, c(chunks, list(fp_p = officer::fp_par(line_spacing = 2))))
  rule <- function(w) officer::fp_border(color = "black", width = w)

  doc <- officer::read_docx()
  for (k in seq_along(tabs)) {
    t <- tabs[[k]]
    d <- t$data
    num <- names(d)[vapply(d, is.numeric, logical(1))]
    disp <- d %>%
      mutate(across(any_of("N"), ~ ifelse(is.na(.x), "", formatC(round(.x), format = "d", big.mark = ","))),
             across(any_of(setdiff(num, "N")), ~ ifelse(is.na(.x), "", formatC(.x, format = "f", digits = 1, big.mark = ","))))

    ft <- flextable::flextable(disp)
    for (col in names(disp)) {
      ft <- flextable::compose(ft, part = "header", j = col,
                               value = do.call(flextable::as_paragraph, lapply(seq_len(nrow(html_italic_runs(col_html[[col]]))), function(j) {
                                 r <- html_italic_runs(col_html[[col]])
                                 flextable::as_chunk(r$text[j], props = fp(italic = r$italic[j]))
                               })))
    }
    ft <- ft %>%
      flextable::font(fontname = font, part = "all") %>%
      flextable::fontsize(size = size, part = "all") %>%
      flextable::border_remove() %>%
      flextable::hline_top(part = "header", border = rule(1.5)) %>%
      flextable::hline_bottom(part = "header", border = rule(1)) %>%
      flextable::hline_bottom(part = "body", border = rule(1.5)) %>%
      flextable::align(j = setdiff(names(disp), num), align = "left", part = "all") %>%
      flextable::align(j = num, align = "center", part = "all") %>%
      flextable::padding(padding.top = 2, padding.bottom = 2, part = "all") %>%
      flextable::autofit() %>%
      flextable::set_table_properties(layout = "autofit")

    if (k > 1) doc <- officer::body_add_break(doc)
    doc <- officer::body_add_fpar(doc, para(list(officer::ftext(paste("Table", k), fp(bold = TRUE)))))
    doc <- officer::body_add_fpar(doc, para(runs(paste0("<i>", t$title, "</i>"))))
    doc <- flextable::body_add_flextable(doc, ft, align = "left")
    doc <- officer::body_add_fpar(doc, para(c(list(officer::ftext("Note.", fp(italic = TRUE)),
                                                   officer::ftext(" ", fp())),
                                              runs(t$note))))
  }
  print(doc, target = file)
}

# ── Figures (on screen APA-style; Word download in AFS journal style) ────────
# Each figure's title and note, written once (notes in markdown: *x* =
# italic) and used both on screen and in the Word download. Notes that depend
# on the data (sample sizes, area names) are built in the server instead.
fig_titles <- list(
  view_lf   = "Length Frequency by Proportional Size Distribution Category",
  view_rw   = "Relative Weight by Proportional Size Distribution Category",
  view_cpue = "Catch per Unit Effort",
  uu_lf     = "Length Frequency of Your Data Compared With the Standardized Average",
  uu_rw     = "Relative Weight of Your Data Compared With the Standardized Average",
  uu_cpue   = "Catch per Unit Effort of Your Data Compared With the Standardized Average")
# CPUE units for the one method being plotted (a caption shouldn't list
# every other method's units).
cpue_units_note <- function(method, waterbody_type) {
  if (length(method) != 1 || length(waterbody_type) != 1) return("")
  unit <- cpue_unit_label(method, waterbody_type)
  if (is.na(unit)) return("")
  long <- c("fish/hr" = "fish per hour", "fish/net night" = "fish per net night",
            "fish/100-m drift" = "fish per 100-m drift", "fish/24-hr set" = "fish per 24-h set",
            "fish/0.25 arc" = "fish per 0.25 arc", "fish/0.5 arc" = "fish per 0.5 arc",
            "fish/10-15m haul" = "fish per 10–15-m haul", "fish/100m²" = "fish per 100 m²")[[unit]]
  paste0("CPUE is expressed as ", long, " (", unit, ").")
}
psd_abbrev_note <- "S-Q = stock-quality; Q-P = quality-preferred; P-M = preferred-memorable; M-T = memorable-trophy; T = trophy"
fig_notes <- list(
  view_lf   = paste0("Bars show the mean percentage of fish in each proportional size distribution category; vertical lines indicate ± 1 standard error. ", psd_abbrev_note, "; *N* = number of surveys."),
  view_rw   = "Points indicate means and vertical lines indicate ± 1 standard error; the dashed line marks a relative weight of 100.",
  # The CPUE units sentence (cpue_units_note()) is added in the server, for the selected method.
  view_cpue = "*Mean ± SE* is the mean catch per unit effort (CPUE) across standardized surveys ± 1 standard error. *Median (IQR)* is the median, with the interquartile range (25th–75th percentile) as the horizontal line. *N* = number of standardized surveys.",
  # {wb} is replaced with the waterbody name (downloads) or "the uploaded
  # waterbody" (on screen), so a downloaded figure never says "your data".
  uu_lf     = paste0("Black bars are the standardized (geographic) average; white and gray bars are {wb} data. Vertical lines indicate ± 1 standard error. ", psd_abbrev_note, "; *n* = number of fish; *N* = number of standardized surveys."))

# Markdown italics (*x*) -> <i>x</i>, for html_italic_runs().
md_to_i <- function(md) gsub("\\*([^*]+)\\*", "<i>\\1</i>", md)

# Writes one figure to a .docx in AFS journal style (North American Journal
# of Fisheries Management, Transactions): the image (scaled to fit the
# 6.5-in text width), then one caption paragraph below it -- bold
# "Figure N." followed by the title and the explanation together. images:
# list of list(path, width, height) in inches -- more than one for a figure
# that spans several pages (the CPUE plot for many years).
build_figure_docx <- function(file, number, title, note_md, images, font = "Times New Roman", size = 12) {
  fp  <- function(...) officer::fp_text(font.family = font, font.size = size, ...)
  dbl <- officer::fp_par(line_spacing = 2)
  runs <- function(x) {
    r <- html_italic_runs(x)
    lapply(seq_len(nrow(r)), function(j) officer::ftext(r$text[j], fp(italic = r$italic[j])))
  }
  para <- function(chunks) do.call(officer::fpar, c(chunks, list(fp_p = dbl)))

  doc <- officer::read_docx()
  for (img in images) {
    w <- min(img$width, 6.5)
    doc <- officer::body_add_img(doc, src = img$path, width = w, height = img$height * w / img$width)
  }
  caption <- paste0(title, ".", if (!is.null(note_md) && nzchar(note_md)) paste0(" ", note_md) else "")
  doc <- officer::body_add_fpar(doc, para(c(list(officer::ftext(paste0("Figure ", number, "."), fp(bold = TRUE)),
                                                 officer::ftext(" ", fp())),
                                            runs(md_to_i(caption)))))
  print(doc, target = file)
}

# Module code
plotDownloadUI <- function(id, height = 400, width = "100%") {
  ns <- NS(id)
  tagList(
    plot_download_buttons(ns("download_plot"), ns("download_word")),
    fluidRow(
      plotOutput(ns('plot'), height = height, width = width)
    )

  )
}

# The two download buttons over every plot: the image alone (300 dpi PNG,
# no title -- what journals ask you to upload), and the figure as a Word
# document with its AFS-style caption (see build_figure_docx()).
plot_download_buttons <- function(png_id, word_id) {
  div(style = "text-align: right; margin-bottom: 6px;",
      downloadButton(png_id, "Download plot (PNG)", class = "btn-sm"),
      downloadButton(word_id, "Download figure with caption (Word)", class = "btn-sm"))
}

# apa: function returning list(number, title, note) for the Word download.
plotDownload <- function(input, output, session, plotFun, width = 9, height = 6, file_ext = "png", apa = NULL) {
  output$plot <- renderPlot({
    plotFun()
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("plot.", file_ext)
    },
    content = function(file) {
      ggsave(file, plotFun(), width = width, height = height, dpi = 300)
    }
  )

  output$download_word <- downloadHandler(
    filename = function() paste0("figure_", if (!is.null(apa)) apa()$number else "", ".docx"),
    content = function(file) {
      spec <- apa()
      png <- tempfile(fileext = ".png")
      ggsave(png, plotFun(), width = width, height = height, dpi = 300)
      build_figure_docx(file, spec$number, spec$title, spec$note,
                            list(list(path = png, width = width, height = height)))
    }
  )
}

# Read in summary fish metrics
metrics <- read_csv('standardized_fish_data_03272026.csv') %>%
  relocate(area) %>%
  relocate(N, .after = last_col()) %>%
  mutate(gcat = case_when(gcat == "Stock-Quality" ~ "S-Q",
                          gcat == "Quality-Preferred" ~ "Q-P",
                          gcat == "Preferred-Memorable" ~ "P-M",
                          gcat == "Memorable-Trophy" ~ "M-T",
                          gcat == "Trophy" ~ "T") %>%
           factor(levels = c("S-Q", "Q-P", "P-M", "M-T", "T"))) %>% 
  mutate(method = str_replace_all(method, " ", "_"), 
         waterbody_type = str_replace_all(waterbody_type, " ", "_"), 
         metric = str_replace_all(metric, "CPUE distance", "CPUE")) %>% 
  filter(method %in% c("boat_electrofishing", 
                       "raft_electrofishing", 
                       "gill_net_fall", 
                       "gill_net_spring", 
                       "drifting_trammel_net", 
                       "large_catfish_hoop_net", 
                       "hoop_net", 
                       "bag_seine", 
                       "stream_seine", 
                       "backpack_electrofishing", 
                       "tow_barge_electrofishing"))

# Percentiles across surveys for length frequency (each survey's % of
# stock-length fish in each size category) and for PSD and PSD-P, made by
# analysis_scripts/make_length_percentiles.R from the same fish-level data
# as the Length Frequency means above (that file has only mean and SE). A
# combination gets percentiles only where its number of surveys and mean
# match the file above, so the ranges always describe the surveys behind the
# averages shown.
length_pct <- read_csv("standardized_length_percentiles.csv", show_col_types = FALSE)
lf_key <- c("area", "common_name", "method", "waterbody_type")
lf_matched <- metrics %>%
  filter(metric == "Length Frequency") %>%
  mutate(gcat = as.character(gcat)) %>%
  inner_join(length_pct, by = c(lf_key, "gcat"), suffix = c("", ".pct")) %>%
  filter(N == N.pct, abs(mean - mean.pct) <= 0.15)
# Keep a combination only if all its size categories matched.
lf_ok <- lf_matched %>% count(across(all_of(lf_key))) %>%
  inner_join(metrics %>% filter(metric == "Length Frequency") %>% count(across(all_of(lf_key)), name = "n_all"), by = lf_key) %>%
  filter(n == n_all) %>% select(all_of(lf_key))
lf_fill <- lf_matched %>% semi_join(lf_ok, by = lf_key) %>%
  transmute(across(all_of(lf_key)), gcat, metric = "Length Frequency",
            p5 = `5%.pct`, p25 = `25%.pct`, p50 = `50%.pct`, p75 = `75%.pct`, p95 = `95%.pct`)
metrics <- metrics %>%
  mutate(.g = as.character(gcat)) %>%
  left_join(lf_fill, by = c(lf_key, ".g" = "gcat", "metric")) %>%
  mutate(`5%` = coalesce(`5%`, p5), `25%` = coalesce(`25%`, p25), `50%` = coalesce(`50%`, p50),
         `75%` = coalesce(`75%`, p75), `95%` = coalesce(`95%`, p95)) %>%
  select(-.g, -p5, -p25, -p50, -p75, -p95)
# PSD and PSD-P reference values (used by the one-page report).
psd_percentiles <- length_pct %>% filter(gcat %in% c("PSD", "PSD-P")) %>% semi_join(lf_ok, by = lf_key)

# Develop vectors of unique entries
uni.type <- c("North America", "Ecoregion", "State/Province")
uni.area <- sort(unique(metrics$area))
uni.spp <- sort(unique(metrics$common_name))
uni.method <- sort(unique(metrics$method))
uni.watertype <- sort(unique(metrics$waterbody_type))
uni.metric <- c("Length Frequency", "Relative Weight", "CPUE") 

# Read in ecoregions shapefiles
ecoregions <- read_sf("ecoregions1/NA_CEC_Eco_Level1.shp")
ecoregions_crs <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
ecoregions_trans <- ecoregions %>% 
  rmapshaper::ms_simplify() %>% 
  st_set_crs(ecoregions_crs) %>%
  st_transform("+proj=longlat +datum=WGS84")

# Read in lat/longs
# Lat/longs are the locations of sampling sites for datasets included in the summary metrics.
# They are used for the plot on the "Explore" page of the app and otherwise do not affect functionality.
# They are not included in this repo for data privacy reasons.
# To get the app to run, read in the `toy_locs.csv` file to populate this data object with fake data.

locs <- read_csv("sites.csv") %>%
# locs <- read_csv("toy_locs.csv") %>%
  select(-date) %>% # not parseable as is
  mutate(ecoregion = case_when(ecoregion == "13" ~ "13 Temperate Sierras", 
                               ecoregion == "10" ~ "10 North American Deserts",
                               ecoregion == "12" ~ "12 Southern Semi-Arid Highlands",
                               ecoregion == "15" ~ "15 Tropical Wet Forests Northern Forests",
                               ecoregion == "4" ~ "4 Hudson Plain",
                               ecoregion == "5" ~ "5 Northern Forests",
                               ecoregion == "6" ~ "6 Northwestern Forested Mountains",
                               ecoregion == "7" ~ "7 Marine West Coast Forest",
                               ecoregion == "8" ~ "8 Eastern Temperate Forests",
                               ecoregion == "9" ~ "9 Great Plains", 
                               ecoregion == "0" ~ "0 Water",
                               ecoregion == "3" ~ "3 Taiga",
                               ecoregion == "14" ~ "14 Tropical Dry Forests",
                               ecoregion == "11" ~ "11 Mediterranean California"), 
         common_name = case_when(common_name == "Brook Trout (lotic)" ~ "Brook Trout", 
                                 common_name == "Brook Trout (lentic)" ~ "Brook Trout", 
                                 common_name == "Brown Trout (lotic)" ~ "Brown Trout", 
                                 common_name == "Brown Trout (lentic)" ~ "Brown Trout", 
                                 TRUE ~ common_name))

# Read in example user upload data
ex <- read_csv("example_user_upload_data.csv")

# Create a "not in" function
`%nin%` <- negate(`%in%`)

# Helper to avoid repeating area-filter logic across three tabs
area_choices <- function(type) {
  if (type == "North America") return("North America")
  if (type == "Ecoregion")     return(uni.area[1:10])
  temp <- uni.area[uni.area != "North America"]
  temp[-1:-10]
}

# Fixed list of the 14 EPA Level 1 ecoregion names, independent of which ones
# happen to have standardized data loaded this year -- used to catch
# typos/misspellings in a user's uploaded `ecoregion` column. A mismatch
# there fails silently otherwise: the row just never matches a standardized
# area, with no indication it's a typo rather than a real database gap.
valid_ecoregions <- c("0 Water", "3 Taiga", "4 Hudson Plain", "5 Northern Forests",
                      "6 Northwestern Forested Mountains", "7 Marine West Coast Forest",
                      "8 Eastern Temperate Forests", "9 Great Plains",
                      "10 North American Deserts", "11 Mediterranean California",
                      "12 Southern Semi-Arid Highlands", "13 Temperate Sierras",
                      "14 Tropical Dry Forests", "15 Tropical Wet Forests Northern Forests")

# States/provinces the standardized database has comparison data for.
# Ecoregion area strings all start with a digit, so excluding those plus
# "North America" leaves just states/provinces.
valid_states <- uni.area[uni.area != "North America" & !str_detect(uni.area, "^[0-9]")]

# State/province polygons (US + Canada + Mexico, Natural Earth 1:50m, public
# domain) and a numeric-code lookup for the ecoregion polygons above -- both
# used to derive state/ecoregion from a lat/long the user provides instead of
# a state/ecoregion column. See download_state_data.R for how the RDS was built.
states_lookup <- readRDS("states1/na_states_provinces.rds")
ecoregion_code_lookup <- setNames(valid_ecoregions, str_extract(valid_ecoregions, "^[0-9]+"))
# Every state/province on the map, whether or not it has standardized data:
# a waterbody in a state without state data (e.g. California) can still be
# compared with its ecoregion and North America.
all_states <- sort(unique(c(states_lookup$name, valid_states)))
state_choices <- setNames(all_states, ifelse(all_states %in% valid_states, all_states,
                                             paste0(all_states, " (no state comparison data)")))

# For each value in `bad`, find the closest fuzzy match in `valid` (if any)
# to suggest as a likely intended spelling in the validation warning.
suggest_closest <- function(bad, valid) {
  vapply(bad, function(x) {
    hit <- agrep(x, valid, max.distance = 0.3, ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) hit[1] else NA_character_
  }, character(1))
}

# Read a user's uploaded file, dispatching on the original filename's
# extension (Shiny stores the upload at a temp path with no useful
# extension, so the real name -- input$upload$name -- must be passed in).
# Columns are read as-is here; header/value cleanup happens downstream in
# the mapping step (see R/upload_mapping.R), not here.
read_upload_file <- function(path, filename) {
  ext <- tolower(tools::file_ext(filename))
  if (ext %in% c("xlsx", "xls")) {
    read_excel(path)
  } else {
    read_csv(path, show_col_types = FALSE)
  }
}


#Shiny App

# Numbered step heading for the Compare Your Data sidebar.
step_heading <- function(n, text) {
  h4(span(style = "display:inline-block; background-color:#325d88; color:white; border-radius:50%; width:30px; height:30px; text-align:center; line-height:30px; margin-right:8px; font-size:18px;",
          n),
     text, style = "font-weight: bold;")
}

# Warning that comparisons are only valid for data collected with the AFS
# standard methods -- short in the sidebar, fuller on the Instructions tab.
afs_methods_warning <- function(compact = FALSE) {
  div(style = "background-color: #fdf3e1; border-left: 4px solid #d98b00; padding: 12px 15px; border-radius: 4px; margin-bottom: 15px;",
      h5(icon("triangle-exclamation"), " Use only data collected with AFS standard methods",
         style = "margin-top: 0; font-weight: bold;"),
      if (compact) {
        p("Your results are valid only if your fish were sampled following ",
          em("Standard Methods for Sampling North American Freshwater Fishes"),
          " (AFS, 2nd edition, 2024).",
          style = "margin-bottom: 0; font-size: 14px;")
      } else {
        p("The webtool compares your data with surveys that followed ",
          em("Standard Methods for Sampling North American Freshwater Fishes"),
          " (American Fisheries Society, 2nd edition, 2024). The comparison is valid only if your fish were sampled the same way: the same gear and gear settings, effort, season, and type of waterbody that the standard method for your method and waterbody type specifies. Data collected any other way can be uploaded, but the results will not be a fair comparison and should not be used.",
          style = "margin-bottom: 0; font-size: 14px;")
      })
}

# Remembers a confirmed column mapping in the browser's own localStorage (per
# browser/device, never sent to the server or any other viewer) so a user who
# repeatedly uploads similarly-formatted files doesn't have to redo Step A
# every time. Read once on load and sent back to Shiny as input$saved_column_mapping;
# written whenever the server pushes an updated mapping via the
# "save_column_mapping" custom message (see the observer near uu_clean()).
column_mapping_storage_js <- "
$(document).on('shiny:connected', function() {
  try {
    var saved = localStorage.getItem('afs_column_mapping');
    if (saved) Shiny.setInputValue('saved_column_mapping', saved);
  } catch (e) {}
});
Shiny.addCustomMessageHandler('save_column_mapping', function(mapping) {
  try { localStorage.setItem('afs_column_mapping', JSON.stringify(mapping)); } catch (e) {}
});
"

ui <- navbarPage(title = "AFS Standard Sampling App",
                 header = tagList(
                   tags$a(style='background-color:white;position:absolute;right:10px;top:15px;',tags$img(src='AFS_logo.png'), href = "https://fisheries.org/"),
                   tags$head(tags$script(HTML(column_mapping_storage_js)), tags$style(HTML(apa_table_css)))
                 ),
                 theme = bslib::bs_theme(bootswatch = "sandstone"),
                 tabPanel(tagList(icon("circle-info"), "About"),
                          wellPanel(
                            tags$head(tags$style(
                              type="text/css",
                              "#pics img {display: block; margin-left: auto; margin-right: auto; max-height: 90%; height: 90%; max-width: 100%, width: auto}"
                            )),
                            imageOutput("pics"),
                            h2("About Standard Sampling", align = "center"),
                            p("Standardization of sampling methods allows fisheries professionals to compare data across large spatial and temporal scales, encourages data sharing and improves communication. In light of these benefits, the American Fisheries Society published the first edition of ", em("Standard Methods for Sampling North American Freshwater Fishes", .noWS = "outside"), " in 2009. A new edition of these methods was published September 2024. The goal of this project, involving over 500 Canadian, Mexican, and United States fisheries scientists, was to recommend standardized freshwater fish sampling methods for North America.", style = "font-size:18px;"),
                            p("As a companion to these books, an online webtool was developed to compare fish data collected from an individual water body using AFS standard methods to rangewide, ecoregion, and state averages calculated from a database of thousands of fish surveys of millions of individual fish collected using the same AFS standard methods.", style = "font-size:18px;"),
                            p("Numerous fisheries professionals have adopted the AFS standard methods and many have noted the webtool as important for management. The webtool allows for comparison of fish metrics commonly used in management to assess population health including condition, length-frequency, and catch per unit effort using data collected using standard methods. When developing this version two of the webtool, we examined the strengths and weaknesses of methods used for requesting, analyzing, and displaying data in an online format. We used this information to achieve our goals of maximizing use and providing simple means to update this program for comparison of fisheries data.", style = "font-size:18px;"),
                            p("We hope that AFS standard sampling methods can be adopted by others, particularly those in data-poor regions, to maximize the ability to compare data.", style = "font-size:18px;"),
                            h2("Details About App Usage", align = "center"), 
                            p("Here, you can compare your data from an individual water body,", strong("collected using a standard AFS method"), "to averages across the range of the species, ecoregion, and statewide. This tool will help you assess if fish you have collected are high, average or low for the metric in question.", style = "font-size:18px;"),
                            p("We collected thousands of data sets on fishes from across Canada, the United States, and some from Mexico to build comparison summaries. A ", strong("data set (", .noWS = "after"), em("N"), strong(")", .noWS = "before"), " is defined as data collected with AFS standard gears and methods during routine monitoring programs of an entire fish community or a population of specific fish ", strong("in a single waterbody conducted once during a year"), ". For example, data collected from Bass Lake (a small standing water body) sampled by boat electrofishing for Largemouth Bass in 2014 would represent 1 data set. In some cases, data sets included multi-day surveys of the same waterbody (e.g., large reservoirs) where the effort was summed across all sampling days. This excludes surveys targeting specific size groups or those with other biases (e.g., egg counts, juveniles fish surveys).", style = "font-size:18px;"), 
                            p("You will see the following tabs in the bar at the top of the page: ", style = "font-size:18px;"), 
                            br(), 
                            p("- ", strong("About", .noWS = "outside"), ": gives an overview about the standard sampling webtool", style = "font-size:18px;"), 
                            p("- ", strong("Explore", .noWS = "outside"), ": displays desired subsets of the standard data in a map and a table", style = "font-size:18px;"), 
                            p("- ", strong("View", .noWS = "outside"), ": displays desired subsets of the standard data in graphical form", style = "font-size:18px;"), 
                            p("- ", strong("Compare Your Data", .noWS = "outside"), ": allows you to compare your data from a waterbody to standard data in rangewide, ecoregion, or state summaries", style = "font-size:18px;"), 
                            br(), 
                            p("Most of the app components can be translated into another language by going to ", 
                               a("Google Translate", href="https://translate.google.com/?sl=auto&tl=en&op=websites", .noWS = "outside"), 
                               " and entering the URL for the app, then selecting the desired language. Maps and plot can only currently be generated using the original version of the app. ", style = "font-size:18px;"), 
                            br(), 
                            p(strong("If you would like the text of the app bigger, you can increase the font size by hitting the control and plus (+) keys on Windows, or command and plus (+) keys on Macs."), style = "font-size:18px;"), 
                            br(),
                            h2("We Do Not Save Your Data", align = "center"),
                            p("Data you upload on the ", strong("Compare Your Data", .noWS = "outside"), " tab is ", strong("not saved", .noWS = "outside"), ". Your file is used only to calculate your results while you are using the webtool, and it is discarded when you close the webtool. We do not keep, look at, or share your data.", style = "font-size:18px;"),
                            p("In the future, we may ask users if they would be willing to contribute data to improve the coverage of the webtool, but we will ", strong("never save or use your data without your permission", .noWS = "outside"), ".", style = "font-size:18px;"),
                            p("(To save you time on repeat uploads, your web browser may remember how your column names were matched. That is kept only on your own computer, is never sent to us, and does not include any of your data.)", style = "font-size:16px; color:#555;"),
                            br(),
                            h2("Moving Forward", align = "center"),
                            p("These data sets were collected and compiled by ", 
                               a("Scott Bonar's lab", href="https://azcfwru.wixsite.com/azcfwru", .noWS = "outside"), 
                               ". Only summaries can be shared. Individual data sets cannot be shared with others because of legal restrictions on their use. As funding permits, additional data will be added to this app, including for more collection methods.", style = "font-size:18px;"), 
                            p("All feedback on this app is greatly appreciated, and can be provided by sending an email to ", 
                               a("Dr. Bonar", href="mailto:SBonar@ag.arizona.edu", .noWS = "outside"), 
                               " or by submitting an ", 
                               a("issue", href="https://github.com/cct-datascience/AFS_database_code/issues", .noWS = "outside"), 
                               " through the project's ", 
                               a("GitHub repository", href="https://github.com/cct-datascience/AFS_database_code", .noWS = "outside"), 
                               ", which is also where the code is located.", style = "font-size:18px;"), 
                            p("Developed in collaboration with the University of Arizona ",
                               a("CCT Data Science", href="https://datascience.cct.arizona.edu/"), "team.", style = "font-size:18px;"),
                            p("Sponsored by:", style = "font-size:18px;"), 
                            imageOutput("logo")
                          )
                 ),
                 
                 tabPanel(title = tagList(icon("map-location-dot"), "Explore"),
                          sidebarLayout(
                            sidebarPanel(
                              "Welcome to the American Fisheries Society Standard Sampling Database App!", 
                              br(),
                              br(),
                              "To explore standard fish data, select one or more options from each menu below.",
                              radioGroupButtons(inputId = "typechoice",
                                                label = "Show data by:",
                                                choices = uni.type,
                                                direction = "vertical",
                                                selected = "North America"),
                              uiOutput("dyn_area"),
                              selectInput(inputId = "sppchoice",
                                          label = "Select species:",
                                          choices = c("All", uni.spp),
                                          multiple = TRUE,
                                          selected = 'Common Carp'),
                              selectInput(inputId = "methodchoice",
                                          label = "Select method(s):",
                                          choices = c("All", uni.method),
                                          multiple = TRUE,
                                          selected = "boat_electrofishing"),
                              selectInput(inputId = "watertypechoice",
                                          label = "Select waterbody type(s):",
                                          choices = c("All", uni.watertype),
                                          multiple = TRUE,
                                          selected = "large_standing_waters"),
                              selectInput(inputId = "metricchoice",
                                          label = "Select metric(s):",
                                          choices = uni.metric,
                                          multiple = TRUE,
                                          selected = uni.metric),
                              downloadButton("filterdownload", "Download filtered"),
                              downloadButton("alldownload", "Download all")
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel(tagList(icon("map"), "Map"), 
                                         br(), 
                                         h6("Points show approximate locations of data, and colored areas indicate", 
                                            strong("EPA Ecoregions Level I"), 
                                            ". Display name of ecoregion at a location by clicking on map. See more details on the ", 
                                            a("EPA website", href="https://www.epa.gov/eco-research/ecoregions-north-america", .noWS = "outside"), 
                                            "."), 
                                         br(), 
                                         leafletOutput("plotSites",
                                                       width = 700,
                                                       height = 500),
                                         textOutput("report_absent1"),
                                         textOutput("report_absent2")), 
                                tabPanel(tagList(icon("table"), "Table"), 
                                         apa_heading("Table", 1, "Standardized Metrics for the Selected Areas, Species, Methods, and Waterbody Types"),
                                         DTOutput("filtertable"),
                                         apa_note(HTML("<i>N</i> = number of standardized surveys; <i>M</i> = mean; <i>SE</i> = standard error; 5th&ndash;95th = percentiles; PSD = proportional size distribution (S-Q = stock-quality, Q-P = quality-preferred, P-M = preferred-memorable, M-T = memorable-trophy, T = trophy).")))
                              )
                            )
                          )
                 ), 
                 
                 tabPanel(title = tagList(icon("table"), "View"),
                          sidebarLayout(
                            sidebarPanel(
                              "Welcome to the American Fisheries Society Standard Sampling Database App!", 
                              br(), 
                              br(),
                              "To view plots of standard fish data, select one option from each menu below.",
                              radioGroupButtons(inputId = "typechoice2",
                                                label = "Show data by:",
                                                choices = uni.type,
                                                direction = "vertical",
                                                selected = "North America"),
                              uiOutput("dyn_area2"),
                              selectInput(inputId = "sppchoice2",
                                          label = "Select species:",
                                          choices = uni.spp,
                                          selected ='Common Carp'),
                              selectInput(inputId = "methodchoice2",
                                          label = "Select method:",
                                          choices = uni.method,
                                          selected = "boat_electrofishing"),
                              selectInput(inputId = "watertypechoice2",
                                          label = "Select waterbody type:",
                                          choices = uni.watertype,
                                          selected = "large_standing_waters")
                            ),
                            mainPanel(apa_heading("Figure", 1, fig_titles$view_lf),
                                      plotDownloadUI("LF_plot"),
                                      apa_note(md_inline(fig_notes$view_lf)),
                                      hr(),
                                      apa_heading("Figure", 2, fig_titles$view_rw),
                                      plotDownloadUI("RW_plot"),
                                      apa_note(md_inline(fig_notes$view_rw), " ",
                                               uiOutput("rw_n_caption_view", inline = TRUE)),
                                      hr(),
                                      apa_heading("Figure", 3, fig_titles$view_cpue),
                                      plotDownloadUI("CPUE_plot", height = "340px"),
                                      apa_note(uiOutput("view_cpue_note", inline = TRUE))
                            )
                          )
                 ),
                 tabPanel(title = tagList(icon("arrow-right-arrow-left"), "Compare Your Data"),
                          sidebarLayout(
                            sidebarPanel(
                              # A first-time user lands here, so the very top
                              # says where to begin and that the steps run
                              # top to bottom.
                              div(style = "background-color: #325d88; color: white; padding: 14px 16px; border-radius: 6px; margin-bottom: 15px;",
                                  h3(icon("circle-play"), " START HERE", style = "margin: 0 0 6px 0; color: white; font-weight: bold;"),
                                  p("Compare your own fish survey data to standardized values across North America, your ecoregion, or your state.",
                                    style = "margin-bottom: 6px; font-size: 14px;"),
                                  p(strong("Follow Steps 1 to 5 below, in order."),
                                    " Detailed instructions are on the ", strong("Instructions"), " tab to the right.",
                                    style = "margin-bottom: 0; font-size: 14px;")
                              ),
                              afs_methods_warning(compact = TRUE),
                              div(style = "background-color: #eef5ea; border-left: 4px solid #5b8c3a; padding: 12px 15px; border-radius: 4px; margin-bottom: 15px;",
                                  h5(icon("lock"), " We do not save your data", style = "margin-top: 0;"),
                                  p("Your file is used only to calculate your results and is discarded when you close the webtool. We do not keep, look at, or share it. We may someday ask if you'd like to contribute data to improve the webtool, but we will never save or use your data without your permission.",
                                    style = "margin-bottom: 0; font-size: 14px;")
                              ),
                              div(style = "margin-bottom: 18px;",
                                  step_heading(1, "Get your file ready"),
                                  p("There are three ways to get your data in. Pick one:", style = "font-size: 14px; margin: 0 0 6px 32px;"),
                                  div(style = "font-size: 13px; color: #444; margin-left: 32px;",
                                    p(strong("Option 1: Upload your own data sheet as it is (easiest)."), br(),
                                      "Upload your CSV or Excel file in Step 2, in whatever format it's in. Then:",
                                      style = "margin-bottom: 2px;"),
                                    tags$ol(style = "padding-left: 20px; margin-bottom: 10px;",
                                      tags$li("The webtool guesses which of your column headings and species names match what it needs."),
                                      tags$li(strong("You check the guesses"), " on the ", strong("Map & Review Data"),
                                              " tab and correct any that are wrong. Please don't skip this; the webtool can't tell when a guess is wrong."),
                                      tags$li("The webtool puts your data into the correct format and loads it for you.")
                                    ),
                                    p(strong("Option 2: Use our template."), br(),
                                      "Download the template, copy and paste your data into the matching columns, save it as a ",
                                      strong("CSV (.csv) or Excel (.xlsx) file"), ", and upload it in Step 2 (choose ",
                                      em("Yes, it's already formatted"), ").",
                                      style = "margin-bottom: 4px;"),
                                    div(style = "margin-bottom: 10px;",
                                        downloadButton("download", "Download template (CSV)", class = "btn-sm")),
                                    p(strong("Option 3: Your file is already in the webtool's format."), br(),
                                      "For example, a file you downloaded from the webtool's Map & Review Data tab last time. It can be a ",
                                      strong("CSV (.csv) or Excel (.xlsx) file"), ". Upload it in Step 2 (choose ",
                                      em("Yes, it's already formatted"), ").",
                                      style = "margin-bottom: 0;")
                                  )
                              ),
                              div(style = "margin-bottom: 18px;",
                                  step_heading(2, "Upload your file"),
                                  # Formatted files (Options 2 and 3) skip the
                                  # Map & Review column/species checking
                                  # entirely -- see is_formatted() in the server.
                                  radioButtons("upload_format", "Is your file already in the webtool's format?",
                                               choices = c("No, it's my own data sheet (Option 1). I'll check the columns on the Map & Review Data tab." = "raw",
                                                           "Yes, it's already formatted (Option 2 template or Option 3 webtool file). Skip the column checking." = "formatted"),
                                               selected = "raw", width = "100%"),
                                  p("If your file already has all of the webtool's columns, this switches to ", em("Yes"),
                                    " by itself when you upload it, and you can go straight to the results.",
                                    style = "font-size: 13px; color: #555; margin-top: -8px;"),
                                  fileInput("upload", NULL,
                                            buttonLabel =  "Choose file...",
                                            multiple = FALSE,
                                            accept = c(".csv", ".xlsx", ".xls"),
                                            placeholder = "No file selected (CSV or Excel)"),
                                  p("Each upload must be one waterbody sampled with one collection method, but it can include as many years as you want (one survey per year).",
                                    style = "font-size: 13px; color: #555; margin-top: -6px;"),
                                  p(icon("circle-info"), strong(" Excel files: only the first sheet is read."),
                                    " Put your data on the first (leftmost) sheet of the workbook.",
                                    style = "font-size: 13px; color: #555;")
                              ),
                              uiOutput("uu_validation_warnings"),
                              div(style = "margin-bottom: 18px;",
                                  step_heading(3, "Check your data"),
                                  p("Open the ", strong("Map & Review Data"), " tab to the right. Fill in the Step A boxes (waterbody, method, state, ecoregion), then check every Step B and Step C guess and fix any that are wrong. Skip this step only if you chose ",
                                    em("Yes, it's already formatted"), " in Step 2.",
                                    style = "font-size: 13px; margin-left: 32px;")
                              ),
                              div(style = "margin-bottom: 8px;",
                                  step_heading(4, "Choose what to compare")
                              ),
                              uiOutput("dyn_type"),
                              uiOutput("dyn_area3"),
                              uiOutput("dyn_spp3"),
                              uiOutput("dyn_method3"),
                              uiOutput("dyn_watertype3"),
                              uiOutput("dyn_year3"),
                              hr(),
                              div(style = "margin-bottom: 8px;",
                                  step_heading(5, "See your results"),
                                  actionButton("open_lab_report", tagList(icon("file-medical"), " View one-page report"),
                                               class = "btn-primary btn-lg", style = "width: 100%; margin: 4px 0 8px; font-weight: bold;"),
                                  p("A one-page summary, like a medical lab report: each index with its typical range and a flag. Print it or save it as a PDF.",
                                    style = "font-size: 13px; margin-left: 4px;"),
                                  p("For full detail, see the ", strong("Results Table"), " and ", strong("Results Plots"), " tabs to the right.",
                                    style = "font-size: 13px; margin-left: 4px;")
                              ),
                              downloadButton("filterdownloaduu", "Download plot data")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel(tagList(icon("circle-info"), "Instructions"),
                                         h2(icon("book-open"), " Instructions", style = "font-weight: bold; margin-top: 18px;"),
                                         div(style = "background-color: #f7f3ee; border-left: 4px solid #325d88; padding: 12px 15px; border-radius: 4px; margin-bottom: 15px; font-size: 15px;",
                                             h4("Quick start", style = "margin-top: 0; font-weight: bold;"),
                                             p("Work down the ", strong("START HERE"), " steps in the column on the left:", style = "margin-bottom: 4px;"),
                                             tags$ol(style = "margin-bottom: 0;",
                                               tags$li(strong("Get your file ready."), " One row per fish, from one waterbody and one collection method."),
                                               tags$li(strong("Upload your file.")),
                                               tags$li(strong("Check your data"), " on the ", strong("Map & Review Data"), " tab and correct anything the webtool guessed wrong."),
                                               tags$li(strong("Choose what to compare:"), " area, species, method, waterbody type, and year."),
                                               tags$li(strong("See your results:"), " click ", strong("View one-page report"), " for a one-page summary like a medical lab report, or see the ", strong("Results Table"), " and ", strong("Results Plots"), " tabs for full detail.")
                                             )),
                                         afs_methods_warning(),
                                         h3("Detailed instructions", style = "font-weight: bold; margin-top: 22px;"),
                                         uiOutput("instructions"),
                                         DTOutput("example")),
                                tabPanel(tagList(icon("wand-magic-sparkles"), "Map & Review Data"),
                                         br(),
                                         h6("If no options are displayed, upload a dataset on the left first."),
                                         br(),
                                         uiOutput("formatted_notice"),
                                         uiOutput("whole_file_settings_ui"),
                                         hr(),
                                         uiOutput("uu_column_mapping_ui"),
                                         hr(),
                                         uiOutput("uu_species_mapping_ui"),
                                         hr(),
                                         h5("Preview of cleaned data"),
                                         p("This is exactly the data used for the analysis, and exactly the format the download below produces -- check it before moving to the Results tabs.",
                                           style = "font-size:13px; color:#555;"),
                                         downloadButton("download_cleaned", "Download cleaned data"),
                                         br(), br(),
                                         DTOutput("uu_clean_preview")),
                                tabPanel(tagList(icon("table"), "Results Table"),
                                         br(),
                                         h6("If no table is displayed, upload a dataset and choose your filters in the sidebar."),
                                         actionButton("open_lab_report2", tagList(icon("file-medical"), " View one-page report"),
                                                      class = "btn-primary", style = "margin: 6px 0 4px; font-weight: bold;"),
                                         br(), br(),
                                         # One narrow APA table per metric (see
                                         # results_tables() in the server).
                                         downloadButton("download_tables_word", "Download tables (Word, APA format)"),
                                         uiOutput("uu_results_tables"),
                                         hr(),
                                         downloadButton("download_cleaned_results", "Download your data in webtool format"),
                                         p("This is your uploaded data, cleaned up and saved as a CSV file in the exact column format this webtool requires -- reuse it next time (Option 3) to skip the mapping step. You can keep it as a CSV file or save it as an Excel (.xlsx) file; in Excel, keep the data on the first sheet.",
                                           style = "font-size: 13px; color:#555; margin-top: 6px;")),
                                tabPanel(tagList(icon("chart-column"), "Results Plots"), 
                                         br(), 
                                         h6("If no plots are displayed, upload a dataset to generate comparison plots."), 
                                         br(), 
                                         apa_heading("Figure", 1, fig_titles$uu_lf),
                                         plotDownloadUI("LF_plot_UU", height = 520),
                                         apa_note(md_inline(sub("{wb}", "the uploaded waterbody", fig_notes$uu_lf, fixed = TRUE))),
                                         hr(),
                                         apa_heading("Figure", 2, fig_titles$uu_rw),
                                         plotDownloadUI("RW_plot_UU", height = 520),
                                         apa_note(uiOutput("rw_caption", inline = TRUE), " ", uiOutput("rw_n_caption", inline = TRUE)),
                                         hr(),
                                         apa_heading("Figure", 3, fig_titles$uu_cpue),
                                         uiOutput("cpue_plot_container"),
                                         apa_note(uiOutput("cpue_caption", inline = TRUE)))
                              )
                            )
                            
                          ), 
                          
                 )
                 
)




server <- function(input, output, session) {
  
  wrap_br <- function(x, width = 22) {
    paste(strwrap(x, width = width), collapse = "<br>")
  }
  
  # The name typed/confirmed in Step A of the Map & Review Data tab; before
  # that box exists (the tab hasn't been opened yet), the name found in the
  # file's own waterbody name column, so results are labeled correctly either
  # way.
  # NA when the user gave no waterbody name.
  wb_name_given <- reactive({
    # Formatted uploads have no Step A box -- the name comes from the file's
    # own waterbody_name column (a stale typed name from an earlier upload
    # mustn't override it).
    if (isTruthy(input$upload) && is_formatted()) return(detected_wb_name()$name)
    typed <- input$waterbody_label
    if (!is.null(typed) && trimws(typed) != "") return(trimws(typed))
    if (isTruthy(input$upload)) detected_wb_name()$name else NA_character_
  })

  # State of the upload, straight from Step A or the formatted file (not
  # from uu_clean(), which itself uses wb_label()).
  upload_state <- reactive({
    s <- if (isTruthy(input$upload) && is_formatted()) {
      tryCatch(first(na.omit(formatted_upload()$data$state)), error = function(e) NA_character_)
    } else {
      input$state_override
    }
    if (isTruthy(s)) s else NA_character_
  })

  # Label for the user's data in every table, plot, and caption: the
  # waterbody name, or "<State> waterbody" when none was given, so
  # downloaded figures never say "your data".
  wb_label <- reactive({
    name <- wb_name_given()
    if (!is.na(name)) return(name)
    if (!is.na(upload_state())) paste(upload_state(), "waterbody") else "Waterbody"
  })

  # What a downloaded figure or table compares, put at the start of its
  # caption so a saved file can't be mistaken for another comparison. With
  # no waterbody name, the waterbody is identified by its state and type.
  # e.g. "Alamo Lake Largemouth Bass captured by boat electrofishing
  # compared to North American standards for large standing waters (>200 ha)."
  method_phrase <- function(x) {
    x <- str_replace_all(x, "_", " ")
    paste("AFS standard", sub("^gill net (spring|fall)$", "gill net (\\1)", x))
  }
  sentence_case <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))
  watertype_phrase <- function(x) {
    x <- str_replace_all(x, "_", " ")
    x <- sub("^small standing waters$", "small standing waters (<200 ha)", x)
    sub("^large standing waters$", "large standing waters (>200 ha)", x)
  }
  standards_phrase <- function(area) {
    if (identical(area, "North America")) return("North American standards")
    if (area %in% valid_ecoregions) return(paste0("standards for the ", area, " ecoregion"))
    paste(area, "standards")
  }
  # Title of a downloaded Compare figure or table, e.g. "Comparison of
  # length frequency of Alamo Lake Largemouth Bass collected using AFS
  # standard boat electrofishing with North American standards for large
  # standing waters (>200 ha)". It opens the figure caption (followed by the
  # explanation of symbols and abbreviations) and is the table title.
  comparison_title <- function(metric) {
    fish <- if (!is.na(wb_name_given())) paste(wb_name_given(), input$sppchoice3) else {
      st <- upload_state()
      paste0(input$sppchoice3, " from ",
             if (!is.na(st)) paste(if (grepl("^[AEIOUaeiou]", st)) "an" else "a", st, "waterbody") else "an unnamed waterbody")
    }
    paste0("Comparison of ", metric, " of ", fish, " collected using ", method_phrase(input$methodchoice3),
           " with ", standards_phrase(input$areachoice3), " for ", watertype_phrase(input$watertypechoice3))
  }
  # View tab (standardized data only), e.g. "North American standards for
  # length frequency of Largemouth Bass collected using AFS standard boat
  # electrofishing in small standing waters (<200 ha)".
  view_title <- function(metric) {
    sentence_case(paste0(standards_phrase(input$areachoice2), " for ", metric, " of ", input$sppchoice2,
                         " collected using ", method_phrase(input$methodchoice2), " in ",
                         watertype_phrase(input$watertypechoice2)))
  }

  # Years in the upload for the current species/method/area selection.
  user_years <- reactive({
    req(input$areachoice3, input$sppchoice3, input$methodchoice3, input$watertypechoice3)
    uu_raw() %>%
      filter(area == input$areachoice3,
             common_name == input$sppchoice3,
             method == input$methodchoice3,
             waterbody_type == input$watertypechoice3) %>%
      distinct(year) %>% filter(!is.na(year)) %>% arrange(year) %>% pull() %>% as.character()
  })

  # How a user-data series is named on plots/captions: "<waterbody> — 2023",
  # "<waterbody> — All years combined", or -- when the upload has only one
  # year, so "all years combined" would just repeat that year -- simply the
  # waterbody name.
  user_series_label <- function(lvl, sep = " — ") {
    single_year <- tryCatch(length(user_years()) <= 1, error = function(e) FALSE)
    if (lvl == ALL_YEARS_LABEL && single_year) wb_label() else paste0(wb_label(), sep, lvl)
  }
  
  output$pics <- renderImage({
    list(
      src = file.path("www/fish_circles2.png"), 
      contentType = "image/png", 
      height = 365, 
      width = 819
    )
  }, deleteFile = FALSE)
  
  output$logo <- renderImage({
    list(
      src = file.path("www/Standardsamplingsponsors.png"), 
      contentType = "image/png", 
      height = 151, 
      width = 741
    )
  }, deleteFile = FALSE)
  
  # Render a UI for selecting area depending on North America, Ecoregions, or State/Province
  output$dyn_area <- renderUI({
    temp <- area_choices(input$typechoice)
    selectInput("areachoice", "Select area:", choices = temp, selected = temp[1], multiple = TRUE)
  })
  
  # Render a UI for selecting area depending on North America, Ecoregions, or State/Province for tab2
  output$dyn_area2 <- renderUI({
    temp <- area_choices(input$typechoice2)
    selectInput("areachoice2", "Select area:", choices = temp, selected = temp[1])
  })
  
  # Provide downloadable template for user upload data
  output$download <- downloadHandler(
    filename = function() {
      "data_template.csv"
    },
    content = function(file) {
      file.copy("user_upload_data_template.csv", file)
    }
  )
  
  # ── Smart upload: map the user's raw columns/species names onto the ──────
  # required schema (see R/upload_mapping.R for the matching logic).
  #
  # waterbody_name, method, waterbody_type, state and ecoregion are NOT
  # mapped to a column at all -- a single upload is always one waterbody
  # sampled with one method in one habitat type in one state (per the app's
  # own instructions), so asking the user to point at a column that just
  # repeats the same value on every row is pure friction. Instead they're
  # picked once for the whole file in Step A (waterbody name pre-filled from
  # a waterbody name column when the file has one) (see whole_file_settings_ui and
  # field_satisfied() below; state/ecoregion default there from a coordinate
  # lookup when possible).
  canonical_fields <- c("common_name", "year", "transect_or_net_set", "effort",
                        "total_length", "weight", "collection_date")
  optional_fields <- c("effort", "weight", "collection_date")
  required_fields <- setdiff(canonical_fields, optional_fields)
  # Fields whose Step B dropdown offers "No column for this in my file": the
  # optional ones, plus year (can come from collection date) and transect
  # (can be numbered from effort) -- see field_satisfied().
  no_column_ok <- c("year", "transect_or_net_set", optional_fields)
  NO_COLUMN <- "__no_column__"
  # collection_date isn't part of the required output schema -- it's only
  # used to tell gill_net_spring from gill_net_fall (and to fill in year if
  # there's no year column). Latitude/longitude aren't mapped to a column
  # either -- detect_coordinates() (R/upload_mapping.R) finds them itself,
  # by name or by scanning values, so there's nothing for the user to pick.
  field_labels <- c(common_name = "Species (column with common name, scientific name, or abbreviation)",
                    year = "Year (if none, taken from Collection date)", transect_or_net_set = "Transect/net set ID (if none, numbered from Effort)",
                    effort = "Effort (column with seconds, net nights, area, haul, etc. -- should correspond to an effort unit in <em>Standard Methods</em> 2024)", total_length = "Total length",
                    weight = "Weight (optional)",
                    collection_date = "Collection date (optional — auto-detects gill net season/year)",
                    waterbody_name = "Waterbody name (Step A)",
                    waterbody_type = "Waterbody type (Step A)",
                    method = "AFS Standard Collection Method (Step A)",
                    state = "State/Province (Step A)",
                    ecoregion = "Ecoregion (Step A)")

  # Fixed vocabularies for the whole-file method/waterbody-type dropdowns --
  # only waterbody types the standardized database has comparison data for.
  fixed_methods <- c("boat_electrofishing", "raft_electrofishing", "gill_net_spring",
                     "gill_net_fall", "drifting_trammel_net", "large_catfish_hoopnet",
                     "bag_seine", "stream_seine", "backpack_electrofishing",
                     "tow_barge_electrofishing")
  fixed_watertypes <- c("large_standing_waters", "small_standing_waters",
                        "wadeable_streams", "rivers")

  # What the user sees in those dropdowns: "gill_net_spring" ->
  # "Gill net (spring)". The underlying value is unchanged.
  readable_choice <- function(x) {
    x <- gsub("_", " ", x)
    x <- sub("gill net spring", "gill net (spring)", x)
    x <- sub("gill net fall", "gill net (fall)", x)
    x <- sub("^small standing waters$", "small standing waters (< 200 ha surface area)", x)
    x <- sub("^large standing waters$", "large standing waters (> 200 ha surface area)", x)
    paste0(toupper(substring(x, 1, 1)), substring(x, 2))
  }

  # Reads the file exactly as uploaded (any headers, any extra columns) —
  # nothing downstream should read input$upload$datapath directly again.
  raw_upload <- reactive({
    req(input$upload)
    read_upload_file(input$upload$datapath, input$upload$name)
  })

  # ── Already-formatted uploads (Options 2 and 3) ─────────────────────────
  # When the user says their file is already in the webtool's format (the
  # template, or a cleaned file downloaded from the webtool), its columns are
  # read directly by name -- no column/species mapping on the Map & Review
  # Data tab, same as the webtool worked before the smart upload.
  is_formatted <- reactive(identical(input$upload_format, "formatted"))

  # Effort and transect/net set are needed only for CPUE, and weight only for
  # relative weight, so a lengths-only file (e.g. a backpack electrofishing
  # survey with no effort recorded) still gets length frequency and PSD.
  formatted_required <- c("state", "waterbody_name", "common_name", "method", "waterbody_type",
                          "year", "total_length")
  formatted_optional <- c("ecoregion", "weight", "effort", "transect_or_net_set")

  # list(data, missing, bad_species, bad_method, bad_watertype): data is NULL
  # when required columns are missing.
  formatted_upload <- reactive({
    raw <- raw_upload()
    # Forgive header case/spacing ("State", "Total Length").
    names(raw) <- gsub("[^a-z0-9]+", "_", tolower(trimws(names(raw))))
    missing <- setdiff(formatted_required, names(raw))
    empty <- list(data = NULL, missing = missing, bad_species = character(0),
                  bad_method = character(0), bad_watertype = character(0))
    if (length(missing) > 0) return(empty)

    out <- raw %>%
      select(any_of(c(formatted_required, formatted_optional))) %>%
      mutate(across(any_of(c("state", "ecoregion", "waterbody_name", "common_name",
                             "method", "waterbody_type", "transect_or_net_set")),
                    ~ str_squish(as.character(.x))),
             across(any_of(c("year", "effort", "total_length", "weight")),
                    ~ suppressWarnings(parse_number(as.character(.x)))))
    if (!"weight" %in% names(out)) out$weight <- NA_real_
    if (!"effort" %in% names(out)) out$effort <- NA_real_
    if (!"transect_or_net_set" %in% names(out)) out$transect_or_net_set <- NA_character_

    # Ecoregion may be given as just its number ("8" -> "8 Eastern Temperate Forests").
    if ("ecoregion" %in% names(out)) {
      is_code <- !is.na(out$ecoregion) & out$ecoregion %in% names(ecoregion_code_lookup)
      out$ecoregion[is_code] <- unname(ecoregion_code_lookup[out$ecoregion[is_code]])
    }

    # Species: only exact (case-insensitive) or known-alias matches are fixed
    # up to the standard name; anything else is left as-is and flagged.
    spp <- unique(na.omit(out$common_name))
    fixed <- vapply(spp, function(s) {
      m <- match_species(s, uni.spp)
      if (m$confidence %in% c("exact", "alias")) m$match else s
    }, character(1))
    out$common_name <- unname(fixed[out$common_name])

    out <- out %>%
      filter(!is.na(common_name), common_name != "") %>%
      select(state, any_of("ecoregion"), waterbody_name, common_name, method, waterbody_type,
             year, transect_or_net_set, effort, total_length, weight)
    if ("ecoregion" %in% names(out) && all(is.na(out$ecoregion))) out <- out %>% select(-ecoregion)

    list(data = out, missing = character(0),
         bad_species = setdiff(unique(out$common_name), uni.spp),
         bad_method = setdiff(unique(na.omit(out$method)), fixed_methods),
         bad_watertype = setdiff(unique(na.omit(out$waterbody_type)), fixed_watertypes))
  })

  # A file that already has every webtool column (the template, or a file
  # the webtool itself produced) is switched to "already formatted" on
  # upload, so the user goes straight to the results instead of being sent
  # to Map & Review because Step 2 was left on its default. They can switch
  # it back; this only runs when a new file is uploaded.
  observeEvent(input$upload, {
    headers <- gsub("[^a-z0-9]+", "_", tolower(trimws(names(raw_upload()))))
    if (all(formatted_required %in% headers) && !is_formatted()) {
      updateRadioButtons(session, "upload_format", selected = "formatted")
      showNotification("Your file is already in the webtool's format, so the column checking was skipped. Go straight to the Results Table and Results Plots tabs.",
                       type = "message", duration = 10)
    }
  })

  # Map & Review Data tab, formatted mode: the mapping steps are hidden, so
  # say why (or which columns are missing).
  output$formatted_notice <- renderUI({
    req(input$upload, is_formatted())
    f <- formatted_upload()
    if (length(f$missing) > 0) {
      div(style = "background-color:#fdf2e9; border-left:4px solid #e67e22; padding:10px 14px; border-radius:4px; font-size:14px;",
          icon("triangle-exclamation"), strong(" Your file is missing some of the webtool's columns: "),
          paste(f$missing, collapse = ", "), ".",
          p("Either add those columns to your file, or in Step 2 on the left choose ",
            strong("No, it's my own data sheet"), " to match your columns here instead.", style = "margin:6px 0 0;"))
    } else {
      div(style = "background-color:#eef5ea; border-left:4px solid #5b8c3a; padding:10px 14px; border-radius:4px; font-size:14px;",
          icon("circle-check"), strong(" Your file is already in the webtool's format,"),
          " so no column checking is needed. Check the preview below, then go to the Results tabs.",
          p("If this isn't right, choose ", strong("No, it's my own data sheet"), " in Step 2 on the left to check the columns yourself.",
            style = "margin:6px 0 0; color:#555;"))
    }
  })

  # Parsed once from the browser's localStorage (see column_mapping_storage_js
  # above) -- a named list, canonical field -> the raw header the user picked
  # for it last time. NULL if there's nothing saved yet, or it's unreadable.
  saved_column_mapping <- reactive({
    tryCatch(jsonlite::fromJSON(input$saved_column_mapping), error = function(e) NULL)
  })

  # Waterbody name from the file itself, when it has a waterbody name column
  # (always true for the template or a file already in the webtool's format):
  # the most common non-blank value in that column. list(name, column), both
  # NA if there's no such column or it only holds numbers (e.g. site IDs).
  detected_wb_name <- reactive({
    raw <- raw_upload()
    col <- match_column(names(raw), "waterbody_name", df = raw)
    none <- list(name = NA_character_, column = NA_character_)
    if (is.na(col)) return(none)
    vals <- trimws(as.character(raw[[col]]))
    vals <- vals[!is.na(vals) & vals != "" & is.na(suppressWarnings(as.numeric(vals)))]
    if (length(vals) == 0) return(none)
    list(name = names(sort(table(vals), decreasing = TRUE))[1], column = col)
  })

  # Whole-file settings: waterbody name/type and method are the same for
  # every row in a single upload (one waterbody, one method, one habitat
  # type), so they're picked once here instead of mapped to a repeating
  # column -- less work, and nothing to get out of sync row to row.
  output$whole_file_settings_ui <- renderUI({
    req(input$upload, !is_formatted())

    # These settings describe the survey, not any one file, so a new upload
    # (e.g. next year's data from the same lake/method) shouldn't silently
    # blank them out -- read back whatever was already selected, if anything,
    # rather than always resetting to the placeholder.
    kept <- function(id) if (isTruthy(input[[id]])) input[[id]] else ""

    # State/Ecoregion: show the auto-detected value pre-filled right in the
    # dropdown -- same as every other guess on this tab -- rather than a
    # separate status message elsewhere. The user can correct it like any
    # other pre-filled guess.
    detected_state <- NA_character_
    detected_eco   <- NA_character_
    coords <- detected_coords()
    if (!is.null(coords$lat) && !is.null(coords$lon)) {
      geo <- lookup_state_ecoregion(coords$lat, coords$lon, states_lookup, ecoregions_trans, ecoregion_code_lookup)
      detected_state <- first(na.omit(geo$state))
      detected_eco   <- first(na.omit(geo$ecoregion))
    }
    state_default <- if (kept("state_override") != "") kept("state_override") else coalesce(detected_state, "")
    eco_default   <- if (kept("ecoregion_override") != "") kept("ecoregion_override") else coalesce(detected_eco, "")

    # Waterbody name: the file's own waterbody name column wins when there is
    # one (a new file may be a different waterbody); otherwise keep whatever
    # was typed before. isolate() so typing in the box doesn't rebuild this
    # whole panel on every keystroke.
    wb_found   <- detected_wb_name()
    wb_default <- if (!is.na(wb_found$name)) wb_found$name else isolate(kept("waterbody_label"))

    method_choices <- c("-- choose one --" = "",
                        setNames(fixed_methods, readable_choice(fixed_methods)),
                        "Gill net — auto-detect spring/fall from Collection date" = "gill_net_auto")

    tagList(
      # On a narrow screen, let a long choice wrap instead of being cut off.
      tags$style(HTML("#watertype_choice + .selectize-control .item,
                       #method_choice + .selectize-control .item,
                       #ecoregion_override + .selectize-control .item,
                       [id^=colmap_] + .selectize-control .item { white-space: normal; }")),
      h5("Step A: settings for this whole file"),
      p("Each upload is one waterbody sampled with one method (it can include many years, one survey per year), so these apply to every row.",
        style = "font-size:13px; color:#555;"),
      fluidRow(
        column(12, textInput("waterbody_label", "Waterbody name (lake, reservoir, or stream)",
                             value = wb_default, placeholder = "e.g., Alamo Lake", width = "100%"),
               if (!is.na(wb_found$name))
                 p(style = "font-size:13px; margin-top:-8px; color:#555;",
                   "Filled in from the ", strong(wb_found$column), " column in your file. Check that it's correct; this name is used in all your results tables and plots.")
               else
                 p(style = "font-size:13px; margin-top:-8px; color:#555;",
                   "Your file doesn't seem to have a waterbody name column, so please type the name here. It is used in all your results tables and plots."))
      ),
      fluidRow(
        # Each on its own full-width row so long choices (e.g. the gill net
        # auto-detect option) are readable in full.
        column(12, selectInput("watertype_choice", "Waterbody type",
                               choices = c("-- choose one --" = "", setNames(fixed_watertypes, readable_choice(fixed_watertypes))),
                               selected = kept("watertype_choice"), width = "100%"))
      ),
      fluidRow(
        column(12, selectInput("method_choice", "AFS Standard Collection Method",
                               choices = method_choices, selected = kept("method_choice"),
                               width = "100%"))
      ),
      hr(),
      h5("State/Province and Ecoregion"),
      p("Pre-filled here from coordinates found automatically in your file -- check it's correct, or pick a value yourself if none was found.",
        style = "font-size:13px; color:#555;"),
      uiOutput("geo_detect_status"),
      # Stacked full-width, same as Waterbody type / Collection method above.
      fluidRow(
        column(12, selectInput("state_override", "State/Province (for entire file)",
                               choices = c("-- none --" = "", state_choices), selected = state_default,
                               width = "100%"),
               p(style = "font-size:13px; margin-top:-8px; color:#555;",
                 "If your state/province has no state comparison data, you can still compare with your ecoregion and with North America."))
      ),
      fluidRow(
        column(12, selectInput("ecoregion_override", "Ecoregion (for entire file)",
                               choices = c("-- none --" = "", valid_ecoregions), selected = eco_default,
                               width = "100%"),
               p(style = "font-size:13px; margin-top:-8px;",
                 icon("map-location-dot"), " Not sure which ecoregion you're in? ",
                 actionLink("open_ecoregion_map", "Open the ecoregion map"),
                 " and click on your waterbody's location to fill in this box and the State/Province box. (More about ecoregions on the ",
                 a("EPA website", href = "https://www.epa.gov/eco-research/ecoregions-north-america", target = "_blank"),
                 ".)"))
      )
    )
  })

  # Ecoregion picker: a pop-up map of the EPA Level I ecoregions. Clicking a
  # region fills in the Ecoregion dropdown with it.
  observeEvent(input$open_ecoregion_map, {
    showModal(modalDialog(
      title = "Find your waterbody",
      p("Zoom and drag the map to your waterbody, then click on it. The state/province and ecoregion at the spot you click will be filled in for you."),
      leafletOutput("ecoregion_picker_map", height = "480px"),
      # The map is first laid out while the pop-up is still animating open,
      # so Leaflet only draws regions inside that too-small area. A resize
      # once it's fully open makes it redraw at its real size.
      tags$script(HTML("$('.modal').one('shown.bs.modal', function(){ window.dispatchEvent(new Event('resize')); });")),
      uiOutput("ecoregion_picker_status"),
      size = "l", easyClose = TRUE,
      footer = modalButton("Done")
    ))
  })

  output$ecoregion_picker_map <- renderLeaflet({
    factpal <- colorFactor(rainbow(length(unique(ecoregions_trans$NA_L1CODE))),
                           ecoregions_trans$NA_L1CODE)
    leaflet() %>%
      addTiles(options = tileOptions(minZoom = 2, maxZoom = 10)) %>%
      # Each ecoregion is many separate polygons, and Leaflet keeps only the
      # last shape drawn per layerId -- so every polygon gets its own ID
      # ("<code>_<row>"), and the click handler reads the code back off it.
      addPolygons(data = ecoregions_trans, layerId = ~paste0(NA_L1CODE, "_", seq_along(NA_L1CODE)),
                  color = ~factpal(NA_L1CODE), fillOpacity = 0.45, weight = 1,
                  label = ~paste0(NA_L1CODE, " ", str_to_title(NA_L1NAME)),
                  highlightOptions = highlightOptions(weight = 3, color = "black", fillOpacity = 0.7)) %>%
      setView(lng = -100, lat = 45, zoom = 3) %>%
      # When the pop-up is closed and opened again, Leaflet redraws the map
      # without its id, so clicks were sent as "undefined_shape_click" and a
      # second pick never reached the observer below. Set it on every draw.
      htmlwidgets::onRender("function(el, x) { this.id = el.id; }")
  })

  # A click fills in the ecoregion (from the polygon clicked) and the
  # state/province (looked up from the clicked point's lat/long), whichever
  # of the two has comparison data.
  observeEvent(input$ecoregion_picker_map_shape_click, {
    click <- input$ecoregion_picker_map_shape_click
    code <- sub("_.*$", "", as.character(click$id))
    picked_eco <- unname(ecoregion_code_lookup[code])
    picked_state <- lookup_state_ecoregion(click$lat, click$lng, states_lookup,
                                           ecoregions_trans, ecoregion_code_lookup)$state[1]
    if (length(picked_eco) != 1) picked_eco <- NA_character_
    # Any state/province is kept, with or without state comparison data.
    if (!isTRUE(picked_state %in% all_states)) picked_state <- NA_character_

    if (!is.na(picked_eco))   updateSelectInput(session, "ecoregion_override", selected = picked_eco)
    if (!is.na(picked_state)) updateSelectInput(session, "state_override", selected = picked_state)

    line <- function(ok, what, value, missing_msg) {
      if (ok) div(icon("circle-check"), paste0(" ", what, " set to "), strong(value, .noWS = "after"), ".",
                  style = "color:#1e7b34;")
      else div(icon("circle-exclamation"), paste0(" ", missing_msg), style = "color:#c0392b;")
    }
    output$ecoregion_picker_status <- renderUI(
      div(style = "margin-top:10px; font-size:15px;",
          line(!is.na(picked_state), "State/Province", picked_state,
               "No state/province found at that spot. You can pick one from the list, or leave it blank; you can still compare with the ecoregion and North America."),
          if (!is.na(picked_state) && !picked_state %in% valid_states)
            div(icon("circle-info"), " ", picked_state, " has no state comparison data, so compare with the ecoregion or North America.",
                style = "color:#1f4e8c;"),
          line(!is.na(picked_eco), "Ecoregion", picked_eco,
               "That area has no ecoregion in the comparison data. Please pick one from the list instead."),
          p("Click Done, or click somewhere else to change it.", style = "color:#555; margin:4px 0 0;")))
  })

  # Explains an empty/unfilled dropdown below -- the success case doesn't
  # need a message of its own since the detected value is pre-filled right
  # into the State/Province and Ecoregion dropdowns themselves.
  output$geo_detect_status <- renderUI({
    req(input$upload)

    coords <- detected_coords()
    if (is.null(coords$lat) || is.null(coords$lon)) {
      return(div(style = "font-size:13px; color:#c0392b; margin-bottom:10px;",
                icon("circle-exclamation"), " No coordinates were found automatically in your file. Pick a state/ecoregion below instead."))
    }

    geo <- lookup_state_ecoregion(coords$lat, coords$lon, states_lookup, ecoregions_trans, ecoregion_code_lookup)
    if (all(is.na(geo$state)) && all(is.na(geo$ecoregion))) {
      div(style = "font-size:13px; color:#c0392b; margin-bottom:10px;",
          icon("circle-exclamation"), " Found coordinates in your file, but they didn't fall inside any state or ecoregion boundary (check they're valid North America decimal-degree coordinates). Pick a state/ecoregion below instead.")
    } else {
      NULL
    }
  })

  output$uu_column_mapping_ui <- renderUI({
    req(input$upload, !is_formatted())
    raw <- raw_upload()
    headers <- names(raw)
    saved <- saved_column_mapping()

    rows <- lapply(canonical_fields, function(field) {
      saved_choice <- saved[[field]]
      # Last time's choice came from a different file, so it's reused only if
      # this file's column of that name still holds the right kind of values.
      default <- if (!is.null(saved_choice) && saved_choice %in% headers &&
                     column_fits_field(raw[[saved_choice]], field)) {
        saved_choice
      } else {
        match_column(headers, field, df = raw)
      }
      # Fields the analysis can do without get a real, selectable "no column"
      # choice (column_mapping() treats NO_COLUMN like any non-header value,
      # i.e. NA). It can't be the empty string: the dropdown widget shows an
      # empty-valued choice only as placeholder text, so once a guess was
      # filled in there'd be no way to pick "none" again.
      choices <- if (field %in% no_column_ok) {
        c("-- No column for this in my file --" = NO_COLUMN, setNames(headers, headers))
      } else {
        c("-- choose a column --" = "", setNames(headers, headers))
      }
      selected <- if (!is.na(default)) default else if (field %in% no_column_ok) NO_COLUMN else ""
      # One full-width box per row, stacked like the Step A boxes.
      box <- fluidRow(column(12, selectInput(paste0("colmap_", field), HTML(field_labels[[field]]),
                                             choices = choices, selected = selected,
                                             width = "100%")))
      # Length and weight each get a units box right under them; the values
      # are converted to mm / g in uu_clean(). Default: the unit the user
      # confirmed last time for this same column, else one named in the
      # column header (e.g. "TL_cm"), else mm / g.
      if (field %in% c("total_length", "weight")) {
        type    <- if (field == "total_length") "length" else "weight"
        unit_id <- paste0(type, "_unit")
        units   <- if (type == "length") length_units else weight_units
        saved_unit <- saved[[unit_id]]
        unit_default <- if (!is.null(saved_unit) && saved_unit %in% units && identical(saved_choice, default)) {
          saved_unit
        } else {
          coalesce(guess_unit_from_header(default, type), unname(units[1]))
        }
        box <- tagList(box, fluidRow(column(12,
          div(style = "margin-left: 20px; margin-top: -8px;",
              selectInput(unit_id, paste0("Units of ", if (type == "length") "total length" else "weight",
                                          " in your file (converted to ", unname(units[1]), " for the analysis)"),
                          choices = units, selected = unit_default, width = "100%"),
              uiOutput(paste0(type, "_unit_warning"))))))
      }
      box
    })

    tagList(
      h5("Step B: match your remaining columns"),
      p(strong("Please check every box below."),
        " The webtool has filled in its best guess of which of your columns matches each item, based on your column headers (or the choices you made last time on this computer). You are the one who knows your data, so make sure each guess is correct and change any that are wrong. Any of your columns not used here are simply ignored.",
        style = "font-size:13px; color:#555;"),
      tagList(rows),
      uiOutput("transect_auto_status")
    )
  })

  # ── Unit sanity checks ─────────────────────────────────────────────────
  # A wrong units box silently skews every length/weight result, so flag
  # values that don't look like fish for the chosen unit, and suggest the
  # unit that would make them look right.
  mapped_numeric <- function(field) {
    col <- column_mapping()[[field]]
    if (is.na(col)) return(NULL)
    v <- suppressWarnings(parse_number(as.character(raw_upload()[[col]])))
    v[!is.na(v) & v > 0]
  }

  # Median total length in mm, or NULL if there's no usable length column.
  median_length_mm <- reactive({
    L <- mapped_numeric("total_length")
    req(input$length_unit)
    if (length(L) == 0) return(NULL)
    median(length_to_mm(L, input$length_unit))
  })
  length_looks_wrong <- reactive({
    med <- median_length_mm()
    !is.null(med) && (med < 50 || med > 1500)
  })

  unit_warning <- function(...) {
    div(style = "font-size:13px; color:#c0392b; margin:-6px 0 10px 0;", icon("triangle-exclamation"), " ", ...)
  }
  unit_name <- function(unit, units) names(units)[units == unit]

  output$length_unit_warning <- renderUI({
    med <- median_length_mm()
    req(!is.null(med), length_looks_wrong())
    raw_med <- median(mapped_numeric("total_length"))
    # The unit that puts the typical fish closest to ~300 mm.
    best <- length_units[which.min(abs(log(length_to_mm(raw_med, length_units) / 300)))]
    unit_warning(
      "With the units picked above, your typical fish is ", strong(round(med), " mm"),
      " long, which is ", if (med < 50) "very small" else "very large", " for these species. ",
      if (best != input$length_unit) tagList("Your lengths look more like ", strong(unit_name(best, length_units)), ". "),
      "Please check the units box.")
  })

  output$weight_unit_warning <- renderUI({
    req(input$weight_unit, input$length_unit, !length_looks_wrong())
    cm <- column_mapping()
    req(!is.na(cm[["weight"]]), !is.na(cm[["total_length"]]))
    raw <- raw_upload()
    L <- suppressWarnings(parse_number(as.character(raw[[cm[["total_length"]]]])))
    W <- suppressWarnings(parse_number(as.character(raw[[cm[["weight"]]]])))
    ok <- !is.na(L) & !is.na(W) & L > 0 & W > 0
    req(sum(ok) >= 3)
    L_mm <- length_to_mm(L[ok], input$length_unit)
    # Weight relative to a typical fish of the same length (condition factor
    # ~1: W[g] = L[mm]^3 / 1e5). Real fish sit well within 0.1x-10x of that.
    ratio_for <- function(unit) median(weight_to_g(W[ok], unit) / (L_mm^3 / 1e5))
    r <- ratio_for(input$weight_unit)
    req(r < 0.1 || r > 10)
    best <- weight_units[which.min(abs(log(vapply(weight_units, ratio_for, numeric(1)))))]
    unit_warning(
      "With the units picked, your fish weigh about ",
      strong(if (r < 1) paste0(signif(1 / r, 2), " times lighter") else paste0(signif(r, 2), " times heavier")),
      " than typical fish of the same length. ",
      if (best != input$weight_unit) tagList("Your weights look more like ", strong(unit_name(best, weight_units)), ". "),
      "Please check the units box.")
  })

  # Tells the user when transects were numbered automatically from effort,
  # and how many were found per year, so they can sanity-check the count.
  output$transect_auto_status <- renderUI({
    cm <- column_mapping()
    req(is.na(cm[["transect_or_net_set"]]))
    if (is.na(cm[["effort"]])) {
      return(div(style = "font-size:13px; color:#c0392b; margin-bottom:10px;",
                 icon("circle-exclamation"),
                 " No transect/net set column chosen. Choose your Effort column (e.g. seconds per transect) and transects will be numbered automatically wherever the effort value changes."))
    }
    uu <- tryCatch(uu_clean(), error = function(e) NULL)
    req(uu)
    counts <- uu %>% distinct(year, transect_or_net_set) %>% count(year)
    div(style = "font-size:13px; color:#1e7b34; margin-bottom:10px;",
        icon("circle-check"),
        paste0(" No transect/net set column, so transects were numbered 1, 2, 3... from your Effort column: a new transect starts wherever the effort value changes from the row above. Found ",
               paste0(counts$n, " transect", ifelse(counts$n == 1, "", "s"), " in ", counts$year, collapse = ", "),
               ". (Two back-to-back transects with exactly the same effort would be counted as one.)"))
  })

  # Named vector: canonical field -> user's chosen raw column name (or NA if
  # left unmapped).
  # Guards against a stale colmap_* selection left over from a previously
  # uploaded file: right after a new file is chosen, the dropdowns still hold
  # their old values for one reactive tick, until uu_column_mapping_ui
  # re-renders with fresh choices/defaults for the new headers. Without this
  # check, a leftover selection naming a column that doesn't exist in the new
  # file would crash the first downstream pull().
  column_mapping <- reactive({
    req(input$upload)
    headers <- names(raw_upload())
    vals <- vapply(canonical_fields, function(f) {
      v <- input[[paste0("colmap_", f)]]
      if (is.null(v) || v == "" || !(v %in% headers)) NA_character_ else v
    }, character(1))
    vals
  })

  # Coordinates found anywhere in the raw upload -- by column name, by
  # scanning values, or from a degrees/minutes/seconds split (see
  # detect_coordinates() in R/upload_mapping.R). Computed once per upload
  # since it's used both to gate on and to actually do the state/ecoregion
  # lookup.
  detected_coords <- reactive({
    req(input$upload)
    detect_coordinates(raw_upload())
  })

  # Whether each required field can actually be filled in, accounting for the
  # fields with more than one possible source: `state` from a column, a
  # lat/long pair, or the manual override; `year` from a column or a
  # collection date. Everything else is satisfied only by a direct column
  # mapping, same as before those alternatives existed.
  field_satisfied <- reactive({
    cm <- column_mapping()
    sat <- setNames(!is.na(cm), names(cm))

    # state isn't a column at all -- state_override is pre-filled from a
    # coordinate lookup whenever one succeeds (see whole_file_settings_ui).
    # It's optional: without a state (or in a state with no state data) the
    # waterbody is still compared with its ecoregion and North America.
    sat["state"] <- TRUE
    if (is.na(cm[["year"]])) {
      sat["year"] <- !is.na(cm[["collection_date"]])
    }
    # No transect/net-set column: numbered automatically from changes in the
    # effort column instead (see transects_from_effort()). Neither is needed
    # without effort -- CPUE is then skipped, and length frequency and
    # relative weight are still calculated.
    sat["transect_or_net_set"] <- TRUE
    # waterbody_name/waterbody_type/method aren't columns -- they're the
    # whole-file settings above. waterbody_name always has a fallback (see
    # uu_clean()), so it never blocks; the other two need an explicit pick,
    # and "auto-detect" gill net season additionally needs a collection date.
    sat["waterbody_name"] <- TRUE
    sat["waterbody_type"] <- isTruthy(input$watertype_choice)
    sat["method"] <- isTruthy(input$method_choice) &&
      (input$method_choice != "gill_net_auto" || !is.na(cm[["collection_date"]]))
    sat
  })

  # required_fields (column-mappable) plus the whole-file settings that also
  # have to be filled in before results can be calculated.
  all_required_fields <- c(required_fields, "waterbody_name", "waterbody_type", "method", "state")

  # Distinct raw species values, once the user has a common_name column chosen.
  uu_species_values <- reactive({
    cm <- column_mapping()
    req(!is.na(cm[["common_name"]]))
    raw_upload() %>% pull(cm[["common_name"]]) %>% as.character() %>%
      na.omit() %>% unique() %>% sort()
  })

  output$uu_species_mapping_ui <- renderUI({
    req(!is_formatted())
    vals <- tryCatch(uu_species_values(), error = function(e) NULL)
    req(length(vals) > 0)

    choices <- c("-- exclude these rows --" = "", setNames(uni.spp, uni.spp))

    rows <- lapply(seq_along(vals), function(i) {
      v <- vals[i]
      m <- match_species(v, uni.spp)
      note <- if (m$confidence == "fuzzy") {
        span("best guess — please check", style = "color:#e67e22; font-size:11px;")
      } else if (m$confidence == "none") {
        span("no match found — pick manually", style = "color:#c0392b; font-size:11px;")
      } else NULL

      fluidRow(
        column(4, tags$code(v)),
        column(5, selectInput(paste0("sppmap_", i), NULL, choices = choices,
                              selected = if (!is.na(m$match)) m$match else "")),
        column(3, note)
      )
    })

    tagList(
      h5("Step C: match your species names to the standard species list"),
      p("Each distinct species value in your file is listed below with its best guess. Rows whose species you leave as \"exclude\" are dropped from the analysis. Check that each guess is the right common name for your scientific name or species code; if there is any doubt, use the latest edition of the American Fisheries Society's ",
        em("Common and Scientific Names of Fishes from the United States, Canada, and Mexico"), " (Names of Fishes).",
        style = "font-size:13px; color:#555;"),
      tagList(rows)
    )
  })

  # Named character vector: raw species value -> canonical species name (or
  # "" for "exclude these rows").
  species_mapping <- reactive({
    vals <- uu_species_values()
    req(length(vals) > 0)
    mapped <- vapply(seq_along(vals), function(i) {
      v <- input[[paste0("sppmap_", i)]]
      if (is.null(v)) "" else v
    }, character(1))
    setNames(mapped, vals)
  })

  # The single source of truth downstream: raw_upload() + column_mapping() +
  # species_mapping() combined into exactly the required schema (same column
  # names/order as user_upload_data_template.csv). Everything after this
  # point is unchanged from before the smart-upload feature.
  uu_clean <- reactive({
    req(input$upload)
    if (is_formatted()) {
      f <- formatted_upload()
      req(f$data)
      return(f$data)
    }
    cm <- column_mapping()
    req(all(field_satisfied()[all_required_fields]))

    raw <- raw_upload()
    sm  <- species_mapping()
    n   <- nrow(raw)

    get_col <- function(field) {
      col <- cm[[field]]
      if (is.na(col)) rep(NA, n) else raw[[col]]
    }

    # state/ecoregion come from the Step A dropdowns, not a column -- those
    # are pre-filled from a lat/long lookup automatically whenever
    # coordinates were found in the file (see whole_file_settings_ui), so
    # this already reflects that detection unless the user has overridden it.
    state_vals     <- if (isTruthy(input$state_override)) rep(input$state_override, n) else rep(NA_character_, n)
    ecoregion_vals <- if (isTruthy(input$ecoregion_override)) rep(input$ecoregion_override, n) else rep(NA_character_, n)

    # year precedence: a mapped column, then derived from the collection date
    # -- row by row, so a year column that's blank or not a year on some rows
    # still gets those years from the date.
    year_vals <- if (!is.na(cm[["year"]])) {
      suppressWarnings(parse_number(as.character(get_col("year"))))
    } else {
      rep(NA_real_, n)
    }
    year_vals[!is.na(year_vals) & (year_vals < 1900 | year_vals > 2100)] <- NA
    if (!is.na(cm[["collection_date"]])) {
      year_vals <- coalesce(year_vals, year_from_date(get_col("collection_date")))
    }

    # method: a fixed choice for the whole file, except "gill_net_auto" which
    # resolves to gill_net_spring/gill_net_fall per row from the collection
    # date (Jan-Jun -> spring, Jul-Dec -> fall) -- field_satisfied() already
    # guarantees a collection date is mapped whenever this option is chosen.
    method_vals <- if (identical(input$method_choice, "gill_net_auto")) {
      gillnet_season_from_date(get_col("collection_date"))
    } else {
      rep(input$method_choice, n)
    }

    effort_vals <- parse_number(as.character(get_col("effort")))
    transect_vals <- if (!is.na(cm[["transect_or_net_set"]])) {
      as.character(get_col("transect_or_net_set"))
    } else {
      transects_from_effort(effort_vals, year = year_vals,
                            date = if (!is.na(cm[["collection_date"]])) get_col("collection_date") else NULL)
    }

    out <- tibble(
      state               = str_squish(state_vals),
      ecoregion           = str_squish(ecoregion_vals),
      waterbody_name      = rep(wb_label(), n),
      common_name_raw     = as.character(get_col("common_name")),
      method              = method_vals,
      waterbody_type      = rep(input$watertype_choice, n),
      year                = year_vals,
      transect_or_net_set = transect_vals,
      effort              = effort_vals,
      total_length        = length_to_mm(parse_number(as.character(get_col("total_length"))), input$length_unit),
      weight              = weight_to_g(parse_number(as.character(get_col("weight"))), input$weight_unit)
    ) %>%
      mutate(common_name = unname(sm[common_name_raw])) %>%
      filter(!is.na(common_name), common_name != "") %>%
      select(-common_name_raw) %>%
      relocate(common_name, .after = waterbody_name)

    if (all(is.na(out$ecoregion))) out <- out %>% select(-ecoregion)

    out
  })

  # Push the confirmed column mapping to the browser's localStorage (see
  # column_mapping_storage_js) so the next upload on this device can reuse it.
  # Only the fields actually pointing at a real column are saved -- there's
  # nothing useful to remember about a field set to "No column for this in my file".
  observe({
    req(!is_formatted(), uu_clean())
    cm <- column_mapping()
    mapped <- as.list(cm[!is.na(cm)])
    if (!is.na(cm[["total_length"]]) && isTruthy(input$length_unit)) mapped$length_unit <- input$length_unit
    if (!is.na(cm[["weight"]]) && isTruthy(input$weight_unit)) mapped$weight_unit <- input$weight_unit
    if (length(mapped) > 0) {
      # Sent as a plain list -- Shiny serializes it to a JS object on the
      # wire, and the client stringifies it itself before writing to
      # localStorage (see column_mapping_storage_js). Pre-encoding it here
      # with jsonlite::toJSON would double-encode: Shiny's own outer message
      # serialization recognizes the jsonlite "json" class and embeds it raw,
      # so the handler would receive an already-parsed object, not a string.
      session$sendCustomMessage("save_column_mapping", mapped)
    }
  })

  output$uu_clean_preview <- renderDT({
    req(uu_clean())
    datatable(uu_clean() %>% head(15), options = list(dom = "t", scrollX = TRUE))
  })

  # Same file from two buttons (Map & Review Data tab and Results Table tab).
  # Each button needs its own ID -- two outputs sharing one ID makes Shiny
  # bind neither, leaving both buttons permanently disabled.
  cleaned_download <- downloadHandler(
    filename = function() "cleaned_upload_data.csv",
    content = function(file) {
      write.csv(uu_clean(), file, row.names = FALSE)
    }
  )
  output$download_cleaned <- cleaned_download
  output$download_cleaned_results <- cleaned_download

  # uu_raw: used only for populating sidebar dropdowns — pivots the cleaned
  # upload once. Separate reactive from uu_metrics_cache so dropdown
  # population doesn't depend on metric calculations.
  uu_raw <- reactive({
    req(input$upload)
    uu <- uu_clean()

    # State comparisons only for a state with standardized data.
    uu_state <- uu %>% filter(state %in% valid_states) %>% select(-any_of("ecoregion")) %>% mutate(type = "state") %>% rename(area = state)
    uu_all   <- uu %>% select(-state, -any_of("ecoregion")) %>% mutate(type = "all", area = "North America")
    uu_all   <- bind_rows(uu_all, uu_state)

    if ("ecoregion" %in% names(uu)) {
      uu_all <- bind_rows(uu_all, uu %>% select(-state) %>% mutate(type = "ecoregion") %>% rename(area = ecoregion))
    }
    uu_all
  })

  # Catch ecoregion/state values that don't exactly match a standardized
  # area name -- rows with these values silently show no standardized
  # comparison data otherwise, indistinguishable from a genuine database gap.
  # Also surfaces unmapped required fields and species names that need a
  # manual check, from the smart-upload mapping step above.
  uu_validation <- reactive({
    req(input$upload)
    # A state/province that isn't recognized at all is flagged (likely a
    # misspelling); a real one without state data isn't an error -- the
    # waterbody is still compared with its ecoregion and North America (see
    # state_comparison_note).
    area_checks <- function() {
      raw <- tryCatch(uu_raw(), error = function(e) NULL)
      states <- tryCatch(unique(na.omit(uu_clean()$state)), error = function(e) character(0))
      list(bad_ecoregion = if (is.null(raw)) character(0) else
             raw %>% filter(type == "ecoregion") %>% distinct(area) %>% pull() %>% setdiff(valid_ecoregions),
           bad_state = setdiff(states, all_states))
    }

    # Formatted files: no mapping to check -- just missing columns and
    # values that don't match the standardized database's names.
    if (is_formatted()) {
      f <- formatted_upload()
      areas <- if (length(f$missing) == 0) area_checks() else list(bad_ecoregion = character(0), bad_state = character(0))
      return(c(list(unmapped = character(0), species_review = character(0),
                    missing_cols = f$missing, bad_species = f$bad_species,
                    bad_method = f$bad_method, bad_watertype = f$bad_watertype),
               areas))
    }

    cm <- column_mapping()
    unmapped <- all_required_fields[!field_satisfied()[all_required_fields]]

    species_review <- character(0)
    if (!is.na(cm[["common_name"]])) {
      vals <- tryCatch(uu_species_values(), error = function(e) character(0))
      if (length(vals) > 0) {
        confidences <- vapply(vals, function(v) match_species(v, uni.spp)$confidence, character(1))
        species_review <- vals[confidences %in% c("fuzzy", "none")]
      }
    }

    areas <- if (length(unmapped) == 0) area_checks() else list(bad_ecoregion = character(0), bad_state = character(0))

    c(list(unmapped = unmapped, species_review = species_review), areas)
  })

  output$uu_validation_warnings <- renderUI({
    v <- uu_validation()

    warning_block <- function(bad_values, valid_values, label) {
      if (length(bad_values) == 0) return(NULL)
      suggestions <- suggest_closest(bad_values, valid_values)
      items <- map2(bad_values, suggestions, function(bad, s) {
        if (!is.na(s)) tags$li(tags$code(bad), " — did you mean ", tags$code(s), "?")
        else tags$li(tags$code(bad), " — not a recognized ", label)
      })
      tagList(
        tags$p(strong(length(bad_values)), " ", label, " value(s) in your file don't match the standardized database. Rows with these values will show no standardized comparison data until fixed:"),
        tags$ul(items)
      )
    }

    unmapped_block <- if (length(v$unmapped) == 0) NULL else tagList(
      tags$p(strong("Missing required column(s):"), " go to the ", strong("Map & Review Data"), " tab and choose a column for: ",
            paste(gsub("<[^>]+>", "", field_labels[v$unmapped]), collapse = ", "), ". Results can't be calculated until every required field is mapped.")
    )

    species_block <- if (length(v$species_review) == 0) NULL else tagList(
      tags$p(strong(length(v$species_review)), " species value(s) need a manual check on the ", strong("Map & Review Data"), " tab:"),
      tags$ul(lapply(v$species_review, function(s) tags$li(tags$code(s))))
    )

    # Formatted-file checks (these fields are NULL for Option 1 uploads).
    missing_cols_block <- if (length(v$missing_cols) == 0) NULL else tagList(
      tags$p(strong("Your file is missing these webtool column(s): "), paste(v$missing_cols, collapse = ", "),
             ". Add them to your file, or choose ", strong("No, it's my own data sheet"),
             " above to match your columns on the ", strong("Map & Review Data"), " tab instead.")
    )

    msgs <- tagList(
      missing_cols_block,
      unmapped_block,
      species_block,
      warning_block(v$bad_species, uni.spp, "species"),
      warning_block(v$bad_method, fixed_methods, "method"),
      warning_block(v$bad_watertype, fixed_watertypes, "waterbody type"),
      warning_block(v$bad_ecoregion, valid_ecoregions, "ecoregion"),
      warning_block(v$bad_state, all_states, "state/province")
    )

    if (length(v$unmapped) == 0 && length(v$species_review) == 0 &&
        length(v$missing_cols) == 0 && length(v$bad_species) == 0 &&
        length(v$bad_method) == 0 && length(v$bad_watertype) == 0 &&
        length(v$bad_ecoregion) == 0 && length(v$bad_state) == 0) return(NULL)

    div(style = "background-color:#fdf2e9; border-left:4px solid #e67e22; padding:10px 14px; border-radius:4px; margin-bottom:15px; font-size:13px;",
        icon("triangle-exclamation"), strong(" Data check:"),
        msgs)
  })

  # Render a UI for selecting area
  output$dyn_type <- renderUI({
    req(input$upload)
    
    present <- unique(uu_raw()$type)
    temp <- c("North America", "Ecoregion", "State/Province")[c("all", "ecoregion", "state") %in% present]

    tagList(
      radioGroupButtons(inputId = "typechoice3",
                        label = "Show data by:",
                        choices = temp,
                        direction = "vertical",
                        selected = temp[1]),
      state_comparison_note()
    )
  })

  # Shown under "Show data by" when the waterbody's state/province has no
  # standardized state data (or none was given), so the missing State/Province
  # choice isn't a mystery.
  state_comparison_note <- function() {
    states <- tryCatch(unique(na.omit(uu_clean()$state)), error = function(e) character(0))
    if (length(states) > 0 && all(states %in% valid_states)) return(NULL)
    msg <- if (length(states) == 0) {
      "no state/province was given for this waterbody."
    } else {
      paste0("there are no standardized state data for ", paste(setdiff(states, valid_states), collapse = ", "), ".")
    }
    div(style = "background-color:#eaf2fb; border-left:4px solid #1f4e8c; padding:8px 12px; border-radius:4px; margin:-4px 0 12px; font-size:13px;",
        icon("circle-info"), strong(" State comparison not possible:"), " ", msg,
        " Compare with the ecoregion or with North America.")
  }
  
  # Render a UI for selecting area depending on North America, Ecoregions, or State/Province for tab 3
  output$dyn_area3 <- renderUI({
    req(input$upload)
    req(input$typechoice3)

    if(input$typechoice3 == "North America") {
      temp <- "North America"
    } else if(input$typechoice3 == "Ecoregion") {
      temp <- uu_raw() %>%
        filter(type == "ecoregion") %>%
        distinct(area) %>% pull()
    } else if(input$typechoice3 == "State/Province") {
      temp <- uu_raw() %>%
        filter(type == "state") %>%
        distinct(area) %>% pull()
    }
    
    selectInput(inputId = "areachoice3",
                label = "Select area:", 
                choices = temp,
                selected = temp[1])
  })  
  
  # Render a UI for selecting species depending on user upload data in tab 3
  output$dyn_spp3 <- renderUI({
    req(input$upload)
    req(input$areachoice3)
    
    temp <- uu_raw() %>%
      filter(area == input$areachoice3) %>%
      distinct(common_name) %>% pull()
    
    selectInput(inputId = "sppchoice3",
                label = "Select species:",
                choices = temp,
                selected = temp[1])
  })  
  
  # Render a UI for selecting species depending on user upload data in tab 3
  output$dyn_method3 <- renderUI({
    req(input$upload)
    req(input$areachoice3)
    req(input$sppchoice3)
    
    temp <- uu_raw() %>%
      filter(area == input$areachoice3,
             common_name == input$sppchoice3) %>%
      distinct(method) %>% pull()
    
    selectInput(inputId = "methodchoice3",
                label = "Select method:",
                choices = temp,
                selected = temp[1])
  })  
  
  # Render a UI for selecting species depending on user upload data in tab 3
  output$dyn_watertype3 <- renderUI({
    req(input$upload) 
    req(input$areachoice3)
    req(input$sppchoice3)
    req(input$methodchoice3)
    
    temp <- uu_raw() %>%
      filter(area == input$areachoice3,
             common_name == input$sppchoice3,
             method == input$methodchoice3) %>%
      distinct(waterbody_type) %>% pull()
    
    selectInput(inputId = "watertypechoice3",
                label = "Select waterbody type:",
                choices = temp,
                selected = temp[1])
  })

  # Render a UI for selecting year(s) depending on user upload data in tab 3.
  # "All years" reproduces the original pooled-across-years result; picking
  # one or more individual years shows that year's own (unaveraged) values
  # instead, so a multi-year upload can be viewed as a trend.
  output$dyn_year3 <- renderUI({
    req(input$upload)
    req(input$areachoice3)
    req(input$sppchoice3)
    req(input$methodchoice3)
    req(input$watertypechoice3)

    temp <- user_years()

    # With only one year, "all years combined" is just that year -- offer a
    # single choice named for the year instead of two identical ones.
    year_choices <- if (length(temp) <= 1) setNames("All years", if (length(temp) == 1) temp else "All data")
                    else c("All years combined" = "All years", temp)

    tagList(
      selectInput(inputId = "yearchoice3",
                  label = "Select year(s):",
                  choices = year_choices,
                  selected = "All years",
                  multiple = TRUE),
      if (length(temp) > 1) p("Selecting several years shows them side by side in both the Results Table and the Results Plots.",
                              style = "font-size: 12px; color:#555; margin-top:-8px;")
    )
  })


  # make filtered, a reactive data object for tab 1 explore, multiple combos okay
  filtered <- reactive({
    f_df <-  metrics %>%
      filter(metric %in% input$metricchoice,
             area %in% input$areachoice,
             case_when("All" %in% input$sppchoice ~ common_name %in% uni.spp,
                       "All" %nin% input$sppchoice ~ common_name %in% input$sppchoice),
             case_when("All" %in% input$methodchoice ~ method %in% uni.method,
                       "All" %nin% input$methodchoice ~ method %in% input$methodchoice),
             case_when("All" %in% input$watertypechoice ~ waterbody_type %in% uni.watertype,
                       "All" %nin% input$watertypechoice ~ waterbody_type %in% input$watertypechoice)) %>%
      arrange(common_name) %>%
      select(area, common_name, method, waterbody_type, gcat, metric, N, mean,
             se, `5%`, `25%`, `50%`, `75%`, `95%`)
    
  })
  
  output$alldownload <- downloadHandler(
    filename = function() { paste0("AFS-all-", Sys.Date(), ".csv") },
    content  = function(file) {
      write.csv(metrics %>% arrange(common_name) %>%
                  select(area, common_name, method, waterbody_type, gcat, metric, N, mean,
                         se, `5%`, `25%`, `50%`, `75%`, `95%`), file)
    }
  )
  
  # make locations, a reactive data object for tab 1, only locations that are selected
  locate <- reactive({
    if(input$typechoice == "North America") {
      l_df <-  locs %>%
        filter(case_when("All" %in% input$sppchoice ~ common_name %in% uni.spp,
                         "All" %nin% input$sppchoice ~ common_name %in% input$sppchoice),
               case_when("All" %in% input$methodchoice ~ method %in% uni.method,
                         "All" %nin% input$methodchoice ~ method %in% input$methodchoice),
               case_when("All" %in% input$watertypechoice ~ waterbody_type %in% uni.watertype,
                         "All" %nin% input$watertypechoice ~ waterbody_type %in% input$watertypechoice)) %>%
        mutate(coords = paste(lat, long)) %>%
        group_by(state, ecoregion, waterbody_name, common_name, method, 
                 waterbody_type, coords) %>%
        summarize(Nsurveys = n(),
                  lat = unique(lat),
                  long = unique(long)) %>%
        ungroup()%>%
        select(-coords)
      
    } else if(input$typechoice == "Ecoregion") {
      l_df <-  locs %>%
        filter(ecoregion %in% input$areachoice,
               case_when("All" %in% input$sppchoice ~ common_name %in% uni.spp,
                         "All" %nin% input$sppchoice ~ common_name %in% input$sppchoice),
               case_when("All" %in% input$methodchoice ~ method %in% uni.method,
                         "All" %nin% input$methodchoice ~ method %in% input$methodchoice),
               case_when("All" %in% input$watertypechoice ~ waterbody_type %in% uni.watertype,
                         "All" %nin% input$watertypechoice ~ waterbody_type %in% input$watertypechoice)) %>%
        mutate(coords = paste(lat, long)) %>%
        group_by(state, ecoregion, waterbody_name, common_name, method, 
                 waterbody_type, coords) %>%
        summarize(Nsurveys = n(),
                  lat = unique(lat),
                  long = unique(long)) %>%
        ungroup()%>%
        select(-coords)
      
    } else if(input$typechoice == "State/Province") {
      l_df <-  locs %>%
        filter(state %in% input$areachoice,
               case_when("All" %in% input$sppchoice ~ common_name %in% uni.spp,
                         "All" %nin% input$sppchoice ~ common_name %in% input$sppchoice),
               case_when("All" %in% input$methodchoice ~ method %in% uni.method,
                         "All" %nin% input$methodchoice ~ method %in% input$methodchoice),
               case_when("All" %in% input$watertypechoice ~ waterbody_type %in% uni.watertype,
                         "All" %nin% input$watertypechoice ~ waterbody_type %in% input$watertypechoice)) %>%
        mutate(coords = paste(lat, long)) %>%
        group_by(state, ecoregion, waterbody_name, common_name, method, 
                 waterbody_type, coords) %>%
        summarize(Nsurveys = n(),
                  lat = unique(lat),
                  long = unique(long)) %>%
        ungroup() %>%
        select(-coords)
      
    }
  })

  # Create excluded dataframe for combinations with only one site and one year
  exclude <- reactive({
    if(input$typechoice == "North America") {
      e_df <- locate() %>%
        group_by(common_name, method, waterbody_type) %>%
        summarize(Nlocs = length(waterbody_name),
                  Nsurveys_tot = sum(Nsurveys))%>%
        filter(Nlocs == 1,
               Nsurveys_tot == 1) %>%
        select(-Nlocs, -Nsurveys_tot)
      
    } else if(input$typechoice == "Ecoregion") {
      e_df <- locate() %>%
        group_by(ecoregion, common_name, method, waterbody_type) %>%
        summarize(Nlocs = length(waterbody_name),
                  Nsurveys_tot = sum(Nsurveys))%>%
        filter(Nlocs == 1,
               Nsurveys_tot == 1) %>%
        select(-Nlocs, -Nsurveys_tot)
      
    } else if(input$typechoice == "State/Province") {
      e_df <- locate() %>%
        group_by(state, common_name, method, waterbody_type) %>%
        summarize(Nlocs = length(waterbody_name),
                  Nsurveys_tot = sum(Nsurveys)) %>%
        filter(Nlocs == 1,
               Nsurveys_tot == 1) %>%
        select(-Nlocs, -Nsurveys_tot)
      
    }
  })
  
  # make filtered, a reactive data object for tab 2, single combos only
  filtered2 <- reactive({
    
    f_df <-  metrics %>%
      filter(metric %in% uni.metric,
             area %in% input$areachoice2,
             common_name %in% input$sppchoice2,
             method %in% input$methodchoice2,
             waterbody_type %in% input$watertypechoice2) %>%
      arrange(common_name) %>%
      select(area, common_name, method, waterbody_type, gcat, metric, N, mean,
             se, `5%`, `25%`, `50%`, `75%`, `95%`)
    
  })
  
  output$filterdownload <- downloadHandler(
    filename = function() {
      paste0("AFS-filtered-", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      write_csv(filtered(), file)
    }
  )
  
  # Making the table look nice
  output$filtertable <- renderDT({
    filtered() %>%
      readable_values() %>%
      mutate(gcat = as.character(gcat)) %>%
      apa_datatable(apa_result_cols, digits = 2)
  })
  
  filtered3 <- reactive({
    
    f_df <-  metrics %>%
      filter(metric %in% uni.metric,
             area %in% input$areachoice3,
             common_name %in% input$sppchoice3,
             method %in% input$methodchoice3,
             waterbody_type %in% input$watertypechoice3) %>%
      arrange(common_name) %>%
      select(area, common_name, method, waterbody_type, gcat, metric, N, mean,
             se, `5%`, `25%`, `50%`, `75%`, `95%`)
    
  })

  # ── Build uu_all and compute all metrics once on upload ──────────────────
  # This is the expensive step (psdAdd + 3 metric calculations).
  # It is cached here and only re-runs when the cleaned/mapped upload changes.
  uu_metrics_cache <- reactive({
    req(input$upload)
    uu <- uu_clean()
    
    uu_state <- uu %>% filter(state %in% valid_states) %>% select(-any_of("ecoregion")) %>% mutate(type = "state")  %>% rename(area = state)
    uu_all    <- uu %>% select(-state, -any_of("ecoregion")) %>% mutate(type = "all", area = "North America")
    
    uu_all <- bind_rows(uu_all, uu_state)
    
    if ("ecoregion" %in% names(uu)) {
      uu_all <- bind_rows(uu_all, uu %>% select(-state) %>% mutate(type = "ecoregion") %>% rename(area = ecoregion))
    }
    
    # FSA sub-group assignment (collapsed from 20 conditions to 4)
    trout_spp   <- c("Brown Trout", "Brook Trout", "Cutthroat Trout", "Rainbow Trout")
    # Every waterbody type offered in the upload dropdown must appear in one
    # of these two lists: a trout record with a type in neither gets no
    # FSA_group, which crashes psdAdd/wrAdd for Brown/Brook Trout (they need
    # a group to pick among multiple Gabelhouse/Ws entries -- see
    # .psd_add_safe for the fallback).
    lentic_wbs  <- c("small_standing_waters", "large_standing_waters")
    lotic_wbs   <- c("wadeable_streams", "rivers")
    
    uu_all <- uu_all %>%
      mutate(FSA_group = case_when(
        common_name %in% trout_spp & waterbody_type %in% lentic_wbs ~ "lentic",
        common_name %in% trout_spp & waterbody_type %in% lotic_wbs  ~ "lotic",
        common_name == "Brook Trout" ~ "overall",
        TRUE ~ NA_character_
      ))
    
    # Calculate all 3 metrics and N — expensive steps, cached here
    uu_counts     <- calculate_total_n(uu_all)
    uu_cpue       <- calculate_cpue(uu_all)
    uu_cpue_total <- calculate_cpue_total(uu_all)
    uu_lf         <- calculate_lf(uu_all)
    uu_rw         <- calculate_rw(uu_all)

    bind_rows(uu_cpue, uu_cpue_total, uu_lf, uu_rw) %>%
      left_join(uu_counts, by = c("type", "area", "common_name", "method", "waterbody_type", "year"),
                suffix = c("", "_total")) %>%
      mutate(N = coalesce(N, N_total)) %>%
      select(-N_total) %>%
      mutate(gcat = case_when(gcat == "stock"     ~ "S-Q",
                              gcat == "quality"   ~ "Q-P",
                              gcat == "preferred" ~ "P-M",
                              gcat == "memorable" ~ "M-T",
                              gcat == "trophy"    ~ "T") %>%
               factor(levels = c("S-Q", "Q-P", "P-M", "M-T", "T")))
  })

  # uu_processed: fast filter-only step, re-runs when dropdowns change.
  # Year filter: "All years" keeps the pooled (year = NA) rows -- the
  # original behavior -- and/or specific years keep that year's own rows;
  # selecting several years at once is how a multi-year upload is compared
  # year by year.
  uu_processed <- reactive({
    req(input$areachoice3, input$sppchoice3, input$methodchoice3, input$watertypechoice3, input$yearchoice3)

    uu_metrics_cache() %>%
      filter(metric         %in% uni.metric,
             area           %in% input$areachoice3,
             common_name    %in% input$sppchoice3,
             method         %in% input$methodchoice3,
             waterbody_type %in% input$watertypechoice3,
             (("All years" %in% input$yearchoice3) & is.na(year)) |
               (year %in% suppressWarnings(as.integer(input$yearchoice3)))) %>%
      arrange(common_name, year) %>%
      select(area, common_name, method, waterbody_type, year, gcat, metric, cpue_calc, cpue_method, N, mean,
             se, `5%`, `25%`, `50%`, `75%`, `95%`)
  })

  # APA-style column headings: statistical symbols italic (N, M, SE),
  # percentiles as ordinals, everything else in sentence case.
  apa_result_cols <- c(metric = "Metric", data_source = "Data source", area = "Area", common_name = "Species",
                       method = "Method", waterbody_type = "Waterbody type", year = "Year",
                       gcat = "PSD category", cpue_method = "CPUE calculation", N = "<i>N</i>",
                       mean = "<i>M</i>", se = "<i>SE</i>", `5%` = "5th", `25%` = "25th", `50%` = "50th",
                       `75%` = "75th", `95%` = "95th")
  readable_values <- function(df) {
    df %>% mutate(across(any_of(c("method", "waterbody_type", "area")), ~ str_replace_all(.x, "_", " ")))
  }

  # ── Results tables (APA) ────────────────────────────────────────────────
  # Instead of one very wide table, one narrow table per metric. Species,
  # method, waterbody type and area are single choices in the sidebar, so
  # they're the same on every row -- they go in the table title instead of
  # repeating as columns -- and each table keeps only the columns its metric
  # uses. Rows: the standardized average first (the reference), then each
  # year of your data, then all years combined. Length frequency and
  # relative weight are grouped by size category so the values being
  # compared sit next to each other.
  #
  # Notes and column headings are written with <i>...</i> for italics so the
  # same text serves the screen (HTML) and the Word download (see
  # html_italic_runs()).
  results_tables <- reactive({
    user  <- uu_processed()
    stand <- filtered3()
    req(nrow(user) + nrow(stand) > 0)

    stand_area <- first(stand$area)
    if (is.null(stand_area) || is.na(stand_area)) stand_area <- input$areachoice3
    stand_area <- str_replace_all(stand_area, "_", " ")
    wb <- wb_label()

    user_years <- sort(unique(na.omit(user$year)))
    user_lvls  <- c(as.character(user_years), if (any(is.na(user$year))) ALL_YEARS_LABEL)
    rows <- bind_rows(
      stand %>% mutate(source = paste(stand_area, "average"), order = 0, cpue_calc = "standardized"),
      user  %>% mutate(lvl = ifelse(is.na(year), ALL_YEARS_LABEL, as.character(year)),
                       source = vapply(lvl, user_series_label, character(1), sep = ", "),
                       order = match(lvl, user_lvls)) %>% select(-lvl)
    ) %>% mutate(gcat = as.character(gcat))

    pretty <- function(x) tools::toTitleCase(str_replace_all(x, "_", " "))
    subject <- paste0(input$sppchoice3, " Sampled by ", pretty(input$methodchoice3), " in ",
                      pretty(input$watertypechoice3))
    unit <- cpue_unit_label(input$methodchoice3, input$watertypechoice3)

    # Drops statistic columns with no values at all for this metric (e.g.
    # percentiles for length frequency).
    keep_filled <- function(df) df %>% select(where(~ !all(is.na(.x))))
    stat_cols <- c("N", "mean", "se", "5%", "25%", "50%", "75%", "95%")
    i <- function(x) paste0("<i>", x, "</i>")
    esc <- function(x) as.character(htmltools::htmlEscape(x))
    pct_note <- function(df) if (any(c("5%", "95%") %in% names(df))) "; 5th–95th = percentiles" else ""
    psd_note <- "PSD = proportional size distribution (S-Q = stock-quality, Q-P = quality-preferred, P-M = preferred-memorable, M-T = memorable-trophy, T = trophy)"

    out <- list()

    cpue <- rows %>% filter(metric == "CPUE")
    if (nrow(cpue) > 0) {
      cpue <- cpue %>%
        mutate(calc = case_when(cpue_calc == "standardized" ~ "Mean across surveys",
                                cpue_calc == "total" ~ "Total catch ÷ total effort",
                                TRUE ~ "Per-transect mean"),
               calc_order = match(calc, c("Mean across surveys", "Per-transect mean", "Total catch ÷ total effort"))) %>%
        arrange(order, calc_order) %>%
        select(source, calc, any_of(stat_cols)) %>% keep_filled()
      out$cpue <- list(
        title = paste0("Catch per Unit Effort (", unit, ") of ", subject),
        data = cpue,
        note = paste0(i("M"), " = mean; ", i("SE"), " = standard error", pct_note(cpue), ". ",
                      "For ", esc(paste(stand_area, "average")), ", ", i("N"), " is the number of standardized surveys and the percentiles describe CPUE among those surveys. ",
                      "For ", esc(wb), ", ", i("N"), " is the number of transects or net sets in the per-transect mean and the number of fish in the total catch ÷ total effort, which is a single value and so has no standard error or percentiles."))
    }

    for (m in c("Length Frequency", "Relative Weight")) {
      d <- rows %>% filter(metric == m)
      if (nrow(d) == 0) next
      d <- d %>%
        mutate(gcat = factor(gcat, levels = c("S-Q", "Q-P", "P-M", "M-T", "T"))) %>%
        arrange(gcat, order) %>% mutate(gcat = as.character(gcat)) %>%
        select(gcat, source, any_of(stat_cols)) %>% keep_filled()
      key <- if (m == "Length Frequency") "lf" else "rw"
      out[[key]] <- list(
        title = paste0(if (key == "lf") "Length Frequency (%) of " else "Relative Weight of ", subject),
        data = d,
        note = paste0(psd_note, "; ", i("M"), " = mean", if (key == "lf") " percentage of stock-length fish in the category" else " relative weight",
                      "; ", i("SE"), " = standard error", pct_note(d), ". ",
                      "For ", esc(paste(stand_area, "average")), ", ", i("N"), " is the number of standardized surveys",
                      if (key == "rw") " with fish in that category" else "",
                      "; for ", esc(wb), ", ", i("N"), " is the number of fish",
                      if (key == "lf") " (stock length and above)" else " in that category", "."))
    }
    # APA grouping: a value repeated down the first column (PSD category, or
    # the data source in the CPUE table) is shown only on its group's first row.
    lapply(out, function(t) {
      first_col <- t$data[[1]]
      t$data[[1]] <- ifelse(c(FALSE, head(first_col, -1) == tail(first_col, -1)), "", first_col)
      t
    })
  })

  # Column headings shared by the screen and Word versions.
  results_col_html <- c(source = "Data source", calc = "Calculation", gcat = "PSD category",
                        N = "<i>N</i>", mean = "<i>M</i>", se = "<i>SE</i>", `5%` = "5th",
                        `25%` = "25th", `50%` = "50th", `75%` = "75th", `95%` = "95th")

  output$uu_results_tables <- renderUI({
    tabs <- results_tables()
    blocks <- lapply(seq_along(tabs), function(k) {
      key <- names(tabs)[k]
      tagList(apa_heading("Table", k, tabs[[key]]$title),
              DTOutput(paste0("uu_table_", key)),
              apa_note(HTML(tabs[[key]]$note)),
              br())
    })
    tagList(blocks)
  })

  # Short tables: show every row, no paging or sideways scrolling.
  for (key in c("cpue", "lf", "rw")) local({
    k <- key
    output[[paste0("uu_table_", k)]] <- renderDT({
      t <- results_tables()[[k]]
      req(t)
      apa_datatable(t$data, results_col_html, page_length = 100, scroll_x = FALSE)
    })
  })

  # The same tables as a Word document, in APA layout (see build_apa_docx()).
  output$download_tables_word <- downloadHandler(
    filename = function() paste0("AFS_comparison_tables_", Sys.Date(), ".docx"),
    content = function(file) {
      # Downloaded tables are titled with exactly what was compared (see
      # comparison_title()), for use in a report or journal.
      tabs <- results_tables()
      metric_names <- c(cpue = paste0("catch per unit effort (", cpue_unit_label(input$methodchoice3, input$watertypechoice3), ")"),
                        lf = "length frequency (%)", rw = "relative weight")
      for (k in names(tabs)) tabs[[k]]$title <- htmltools::htmlEscape(comparison_title(metric_names[[k]]))
      build_apa_docx(tabs, results_col_html, file)
    }
  )

  # ── One-page "lab report" ───────────────────────────────────────────────
  # Laid out like a medical lab printout: one row per index with the
  # waterbody's result, the typical range (25th–75th percentile of the
  # standardized surveys), the reference range (5th–95th), the result's
  # approximate percentile rank, and a flag. Length frequency has only an
  # average in the standardized data, so PSD and PSD-P are shown against
  # that average without a flag.
  lab_flag <- function(v, ref) {
    if (is.null(ref) || is.na(v) || any(is.na(c(ref$`5%`, ref$`25%`, ref$`75%`, ref$`95%`)))) return(NA_character_)
    if (v < ref$`5%`) "LOW" else if (v < ref$`25%`) "Below typical" else if (v <= ref$`75%`) "Typical"
    else if (v <= ref$`95%`) "Above typical" else "HIGH"
  }

  lab_report_data <- reactive({
    user  <- uu_processed() %>%
      mutate(lvl = ifelse(is.na(year), ALL_YEARS_LABEL, as.character(year)), gcat = as.character(gcat))
    stand <- filtered3() %>% mutate(gcat = as.character(gcat))
    req(nrow(user) > 0)
    lvls <- c(as.character(sort(unique(na.omit(user$year)))), if (any(is.na(user$year))) ALL_YEARS_LABEL)
    first_row <- function(df) if (nrow(df) == 0) NULL else df[1, ]
    result <- function(v, n_text) list(value = v, n = n_text)

    rows <- list()
    # CPUE: the survey's mean across transects/net sets, ranked among the
    # standardized surveys' CPUE.
    cpue_ref <- first_row(stand %>% filter(metric == "CPUE"))
    rows[[length(rows) + 1]] <- list(
      index = "Catch per unit effort (CPUE)", units = cpue_unit_label(input$methodchoice3, input$watertypechoice3),
      digits = 1, ref = cpue_ref, ref_avg = if (!is.null(cpue_ref)) cpue_ref$mean else NA,
      results = setNames(lapply(lvls, function(l) {
        u <- first_row(user %>% filter(metric == "CPUE", cpue_calc == "transect", lvl == l))
        if (is.null(u)) result(NA, "") else result(u$mean, paste0(u$N, if (u$N == 1) " transect" else " transects"))
      }), lvls))

    # PSD and PSD-P from the incremental length-frequency percentages,
    # ranked against the standardized surveys' own PSD values
    # (psd_percentiles, from make_length_percentiles.R).
    psd_cats <- list("PSD" = c("Q-P", "P-M", "M-T", "T"), "PSD-P" = c("P-M", "M-T", "T"))
    psd_labels <- c("PSD" = "PSD (quality and larger)", "PSD-P" = "PSD-P (preferred and larger)")
    stand_lf <- stand %>% filter(metric == "Length Frequency")
    lf_n <- function(u) paste0(first(na.omit(u$N)), " stock-length fish")
    for (nm in names(psd_cats)) {
      cats <- psd_cats[[nm]]
      ref <- first_row(psd_percentiles %>% filter(area == input$areachoice3, common_name == input$sppchoice3,
                                                  method == input$methodchoice3, waterbody_type == input$watertypechoice3,
                                                  gcat == nm))
      rows[[length(rows) + 1]] <- list(
        index = psd_labels[[nm]], units = "% of stock-length fish", digits = 0, ref = ref,
        ref_avg = if (!is.null(ref)) ref$mean else if (nrow(stand_lf) > 0) sum(stand_lf$mean[stand_lf$gcat %in% cats], na.rm = TRUE) else NA,
        results = setNames(lapply(lvls, function(l) {
          u <- user %>% filter(metric == "Length Frequency", lvl == l)
          if (nrow(u) == 0) result(NA, "") else result(sum(u$mean[u$gcat %in% cats], na.rm = TRUE), lf_n(u))
        }), lvls))
    }

    # Each size category's share of stock-length fish (the length-frequency
    # bar graph), ranked against the standardized surveys' shares.
    lf_cat_names <- c("S-Q" = "stock to quality", "Q-P" = "quality to preferred", "P-M" = "preferred to memorable",
                      "M-T" = "memorable to trophy", "T" = "trophy")
    for (g in names(lf_cat_names)) {
      ref <- first_row(stand_lf %>% filter(gcat == g))
      if (is.null(ref)) next
      rows[[length(rows) + 1]] <- list(
        index = paste0("Size structure, ", g, " (", lf_cat_names[[g]], ")"), units = "% of stock-length fish", digits = 0,
        ref = if (!is.na(ref$`5%`)) ref else NULL, ref_avg = ref$mean,
        results = setNames(lapply(lvls, function(l) {
          u <- user %>% filter(metric == "Length Frequency", lvl == l)
          if (nrow(u) == 0) result(NA, "") else result(coalesce(sum(u$mean[u$gcat == g], na.rm = TRUE), 0), lf_n(u))
        }), lvls))
    }

    # Relative weight, one row per size category that has data.
    cat_names <- c("S-Q" = "stock to quality", "Q-P" = "quality to preferred", "P-M" = "preferred to memorable",
                   "M-T" = "memorable to trophy", "T" = "trophy")
    for (g in names(cat_names)) {
      ref <- first_row(stand %>% filter(metric == "Relative Weight", gcat == g))
      u_g <- user %>% filter(metric == "Relative Weight", gcat == g)
      if (is.null(ref) && nrow(u_g) == 0) next
      rows[[length(rows) + 1]] <- list(
        index = paste0("Relative weight, ", g, " (", cat_names[[g]], ")"), units = "Wr", digits = 0,
        ref = ref, ref_avg = if (!is.null(ref)) ref$mean else NA,
        results = setNames(lapply(lvls, function(l) {
          u <- first_row(u_g %>% filter(lvl == l))
          if (is.null(u) || is.na(u$mean)) result(NA, "") else result(u$mean, paste0(u$N, " fish"))
        }), lvls))
    }

    list(rows = rows, lvls = lvls, n_surveys = if (!is.null(cpue_ref)) cpue_ref$N else NA)
  })

  output$lab_report_ui <- renderUI({
    # req() signals "shiny.silent.error", which isn't an "error" condition.
    d <- tryCatch(lab_report_data(), shiny.silent.error = function(e) NULL, error = function(e) NULL)
    if (is.null(d)) {
      return(div(style = "padding: 20px; font-size: 15px;",
                 icon("circle-info"), " Upload your file and finish Steps 3 and 4 on the left first; the report is built from those choices."))
    }
    fmt <- function(x, digits) if (is.na(x)) "—" else formatC(x, format = "f", digits = digits, big.mark = ",")
    # A different color for each flag, red (low) through green (typical) to
    # blue (high), so below and above typical are told apart at a glance.
    flag_style <- function(f) switch(f,
      "LOW"           = "background:#a93226; color:white;",
      "Below typical" = "background:#f5b041; color:#3d2400;",
      "Typical"       = "background:#cfe8c4; color:#1e4d12;",
      "Above typical" = "background:#aed6f1; color:#0b3a5e;",
      "HIGH"          = "background:#1f4e8c; color:white;", "")
    flag_badge <- function(f) tags$span(class = "lr-flag", style = flag_style(f), f)
    multi <- length(d$lvls) > 1

    result_cell <- function(r, row) {
      if (is.na(r$value)) return(tags$td("—", class = "lr-num"))
      f <- lab_flag(r$value, row$ref)
      pct <- if (!is.null(row$ref) && !is.na(f)) estimate_pct_rank(r$value, row$ref) else NA
      tags$td(class = "lr-num",
              tags$div(class = "lr-value", fmt(r$value, row$digits)),
              if (!is.na(f)) flag_badge(f),
              tags$div(class = "lr-small",
                       paste(c(if (!is.na(pct)) paste0("~", pct, ordinal_suffix(pct), " percentile"),
                               if (nzchar(r$n)) r$n), collapse = "; ")))
    }
    range_cell <- function(row, lo, hi) {
      if (is.null(row$ref) || is.na(row$ref[[lo]])) return(tags$td("—", class = "lr-num"))
      tags$td(class = "lr-num", paste0(fmt(row$ref[[lo]], row$digits), "–", fmt(row$ref[[hi]], row$digits)))
    }

    body_rows <- lapply(d$rows, function(row) {
      tags$tr(
        tags$td(class = "lr-index", row$index, tags$div(class = "lr-small", row$units)),
        lapply(d$lvls, function(l) result_cell(row$results[[l]], row)),
        range_cell(row, "25%", "75%"),
        range_cell(row, "5%", "95%"),
        tags$td(class = "lr-num", fmt(row$ref_avg, row$digits),
                if (!is.null(row$ref) && !is.na(row$ref$N)) tags$div(class = "lr-small", paste0("N = ", format(row$ref$N, big.mark = ","), " surveys")))
      )
    })

    st <- upload_state()
    info <- list(
      "Waterbody" = wb_label(),
      "State/Province" = if (!is.na(st)) st else "—",
      "Species" = input$sppchoice3,
      "Collection method" = method_phrase(input$methodchoice3),
      "Waterbody type" = sentence_case(watertype_phrase(input$watertypechoice3)),
      # The survey years in the file, even when only "All years" is shown.
      "Year(s)" = paste(tryCatch(sort(unique(na.omit(uu_clean()$year))), error = function(e) character(0)), collapse = ", "),
      "Compared with" = sentence_case(standards_phrase(input$areachoice3)),
      "Report date" = format(Sys.Date(), "%B %d, %Y"))

    div(class = "lab-report",
        # Styles live inside the report so the print window (which copies only
        # this element) looks the same.
        tags$style(HTML("
          .lab-report { font-family: Arial, Helvetica, sans-serif; color: #222; max-width: 900px; margin: 0 auto; }
          .lab-report h2 { margin: 0; font-size: 22px; } .lab-report .lr-sub { color: #555; font-size: 13px; margin-bottom: 10px; }
          .lab-report .lr-head { border-bottom: 3px solid #325d88; padding-bottom: 6px; margin-bottom: 10px; }
          .lab-report table { border-collapse: collapse; width: 100%; font-size: 13px; }
          .lab-report .lr-info td { padding: 2px 10px 2px 0; vertical-align: top; }
          .lab-report .lr-info td:nth-child(odd) { color: #555; white-space: nowrap; }
          .lab-report .lr-results { margin-top: 12px; }
          .lab-report .lr-results th { background: #325d88; color: white; padding: 6px; text-align: center; font-weight: bold; }
          .lab-report .lr-results td { border-bottom: 1px solid #ccc; padding: 6px; vertical-align: top; }
          .lab-report .lr-num { text-align: center; } .lab-report .lr-index { font-weight: bold; }
          .lab-report .lr-value { font-size: 15px; font-weight: bold; }
          .lab-report .lr-flag { display: inline-block; padding: 1px 7px; border-radius: 10px; font-size: 11px; font-weight: bold; margin-top: 2px; -webkit-print-color-adjust: exact; print-color-adjust: exact; }
          .lab-report .lr-small { font-size: 11px; color: #666; font-weight: normal; }
          .lab-report .lr-notes { font-size: 11px; color: #444; margin-top: 10px; line-height: 1.4; }
          .lab-report .lr-warn { border: 1px solid #d98b00; background: #fdf3e1; padding: 6px 8px; margin-top: 8px; font-size: 11px; -webkit-print-color-adjust: exact; print-color-adjust: exact; }
          @page { size: letter; margin: 0.45in; }
          @media print {
            body { margin: 0 !important; }
            .lab-report { max-width: none; }
            .lab-report h2 { font-size: 18px; } .lab-report .lr-sub { margin-bottom: 4px; }
            .lab-report table { font-size: 11px; }
            .lab-report .lr-results { margin-top: 6px; }
            .lab-report .lr-results td { padding: 3px 5px; } .lab-report .lr-results th { padding: 4px; }
            .lab-report .lr-value { font-size: 12px; display: inline; margin-right: 6px; }
            .lab-report .lr-flag { font-size: 9px; margin-top: 0; }
            .lab-report .lr-small { font-size: 9px; }
            .lab-report .lr-notes { font-size: 9px; margin-top: 4px; line-height: 1.3; }
            .lab-report .lr-notes p { margin: 0 0 3px; }
            .lab-report .lr-warn { font-size: 9px; padding: 4px 6px; margin-top: 4px; }
          }
        ")),
        div(class = "lr-head",
            h2("Fish Population Report"),
            div(class = "lr-sub", "AFS Standard Sampling Webtool: comparison with standardized fish data")),
        tags$table(class = "lr-info",
                   tags$tbody(lapply(seq(1, length(info), by = 2), function(i) {
                     tags$tr(tags$td(names(info)[i]), tags$td(tags$b(info[[i]])),
                             if (i + 1 <= length(info)) tagList(tags$td(names(info)[i + 1]), tags$td(tags$b(info[[i + 1]]))))
                   }))),
        tags$table(class = "lr-results",
                   tags$thead(tags$tr(
                     tags$th("Index"),
                     lapply(d$lvls, function(l) tags$th(if (multi) paste("Result,", l) else "Result")),
                     tags$th("Typical range", tags$div(class = "lr-small", style = "color:#dde;", "25th–75th percentile")),
                     tags$th("Reference range", tags$div(class = "lr-small", style = "color:#dde;", "5th–95th percentile")),
                     tags$th("Reference average"))),
                   tags$tbody(body_rows)),
        div(class = "lr-notes",
            p(strong("How to read this report. "),
              "Ranges come from the standardized surveys for the same species, collection method, and waterbody type in the area compared. ",
              "The typical range holds the middle 50% of those surveys and the reference range the middle 90%. ",
              "Flags: ", flag_badge("LOW"), " below the reference range; ", flag_badge("Below typical"),
              " within the reference range but below the typical range; ", flag_badge("Typical"), " within the typical range; ",
              flag_badge("Above typical"), " within the reference range but above the typical range; ",
              flag_badge("HIGH"), " above the reference range. ",
              "Percentile = approximate rank of this result among the standardized surveys, estimated from their 5th, 25th, 50th, 75th, and 95th percentiles."),
            p("All reference values are waterbody-level: each standardized survey's own value is calculated first, and the ranges describe those survey values. ",
              "CPUE is the mean across this waterbody's transects or net sets. PSD and PSD-P are the percentages of stock-length fish that are quality length and larger, and preferred length and larger; ",
              "the size-structure rows are the percentage of stock-length fish in each length category (the length-frequency bar graph). Relative weight (Wr) is the mean for fish in each length category. ",
              "A dash means no reference range is available for that combination.")),
        div(class = "lr-warn",
            strong("Valid only for data collected with AFS standard methods"),
            " (Standard Methods for Sampling North American Freshwater Fishes, 2nd edition, 2024). Results for data collected any other way are not a fair comparison."))
  })

  # Opens the report in a pop-up; "Print / Save as PDF" copies just the
  # report into a clean window and prints it, so the app page isn't printed.
  show_lab_report <- function() {
    showModal(modalDialog(
      uiOutput("lab_report_ui"),
      size = "l", easyClose = TRUE,
      footer = tagList(
        tags$button(type = "button", class = "btn btn-primary",
                    onclick = "var r = document.querySelector('.lab-report'); if (!r) return; var w = window.open('', '_blank'); w.document.write('<!doctype html><html><head><meta charset=\"utf-8\"><title>Fish Population Report</title></head><body style=\"margin:24px\">' + r.outerHTML + '</body></html>'); w.document.close(); w.focus(); setTimeout(function(){ w.print(); }, 300);",
                    icon("print"), " Print / Save as PDF"),
        modalButton("Close"))
    ))
  }
  observeEvent(input$open_lab_report, show_lab_report())
  observeEvent(input$open_lab_report2, show_lab_report())
  
  # For downloading the filtered dataset for tab 3 comparison
  
  both <- reactive({
    
    both_df <- uu_processed() %>%
      bind_rows(filtered3(), .id = "id") %>%
      rename(data_source = id) %>%
      mutate(data_source = case_when(data_source == 1 ~ wb_label(),
                                     data_source == 2 ~ area),
             cpue_method = coalesce(cpue_method, "")) %>%
      arrange(metric, common_name, cpue_calc, year) %>%
      select(metric, data_source, common_name, method, waterbody_type, year, gcat, cpue_method, N, mean,
             se, `5%`, `25%`, `50%`, `75%`, `95%`)
  })
  
  output$filterdownloaduu <- downloadHandler(
    filename = function() {
      paste0("AFS-UU-filtered-", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      write.csv(both(), file)
    }
  )
  
  # Map of ecoregions and sites
  output$plotSites <- renderLeaflet({
    # Remove 'exclude()' from plot_data if only 1 site or 1 year is represented
    plot_data <- locate() %>%
      anti_join(exclude())
    
    factpal <- colorFactor(rainbow(length(unique(ecoregions_trans$NA_L1CODE))), 
                           ecoregions_trans$NA_L1CODE)
    
    map <- leaflet() %>% 
      addTiles(options = tileOptions(minZoom = 2, maxZoom = 6)) %>%
      addPolygons(data = ecoregions_trans, color = ~factpal(NA_L1CODE),
                  fillOpacity = 0.5, popup = ~htmltools::htmlEscape(NA_L1NAME),
                  stroke = FALSE)
    if(nrow(plot_data) > 0) {
      map <- map %>%
        addCircleMarkers(data = plot_data, lng = ~long, lat = ~lat, stroke = FALSE, 
                         radius = 3, fillOpacity = 0.75, fillColor = "black",
                         popup = popupTable(plot_data,
                                            zcol = 4:7,
                                            row.numbers = FALSE,
                                            feature.id = FALSE))
    }
    print(map)
    
  })
  
  # Text to describe insufficient sample size for anonymity
  output$report_absent1 <- renderText({
    paste0(nrow(exclude()), " site(s) not shown to maintain data anonymity. ")
  })
  
  # Text to describe sites missing lat/lon
  output$report_absent2 <- renderText({
    temp <- locate() %>% filter(is.na(lat)) 
    if(nrow(temp) == 0) {
      paste0(nrow(temp), 
             " site(s) not shown due to unreported coordinates. ")
    } else {
      paste0(nrow(temp), 
             " site(s) not shown due to unreported coordinates in ",
             paste(unique(temp$state), collapse = ", "), 
             ". ") 
    }
    
  })
  
  
  plotLengthFrequency <- reactive({
    temp <- filtered2() %>%
      filter(metric == "Length Frequency")
    
    N <- temp %>% 
      distinct(N) %>% 
      pull(N)
    
    if(nrow(temp) == 0){
      fig <- ggplot() +
        annotate("text", x = 1, y = 1, size = 8,
                 label = "No length frequency data for selected options") +
        theme_void()
    } else {
      fig <- ggplot(temp, aes(x = gcat, y = mean)) +
        geom_bar(stat = "identity", fill = "white", color = "black") +
        # Capped and heavier so a small SE is still visible.
        geom_errorbar(aes(ymin = mean - se,
                          ymax = mean + se),
                      width = 0.3, linewidth = 1) +
        scale_y_continuous("Frequency (%)",
                           limits = c(0, 100),
                           expand = c(0, 0)) +
        theme_apa(base_size = 20) +
        xlab("Proportional size distribution categories") +
        theme(legend.position = "none") +
        geom_richtext(x = 5, y = 90, label = paste0("<i>N</i> = ", N), size = 7, fill = NA, label.color = NA)
    }
    
    print(fig)
    
  })
  
  callModule(plotDownload, "LF_plot", plotLengthFrequency,
             apa = function() list(number = 1, title = view_title("length frequency"), note = fig_notes$view_lf))
  
  plotRelativeWeight <- reactive({
    
    temp <- filtered2() %>%
      filter(metric == "Relative Weight")
    
    if(nrow(temp) == 0){
      fig <- ggplot() +
        annotate("text", x = 1, y = 1, size = 8,
                 label = "No relative weight data for selected options") +
        theme_void()
    } else {
      fig <- ggplot(temp, aes(x = gcat)) +
        # Caps wider than the point, so an SE smaller than the point still
        # shows as a line sticking out on each side of it.
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                      width = 0.25, linewidth = 1, color = "black", na.rm = TRUE) +
        geom_point(aes(y = mean),
                   size = 4,
                   color = "black") +
        geom_hline(yintercept = 100, linetype = "dashed") +
        scale_x_discrete(drop = FALSE) +
        ylim(50, 160) +
        labs(y = "Relative weight (<i>W<sub>r</sub></i>)") +
        theme_apa(base_size = 20) +
        xlab("Proportional size distribution categories") +
        theme(legend.position = "bottom",
              legend.title = element_blank(), 
              axis.title.y = ggtext::element_markdown())
    }    
    
    print(fig)
    
  })
  
  callModule(plotDownload, "RW_plot", plotRelativeWeight,
             apa = function() list(number = 2, title = fig_titles$view_rw,
                                   note = paste(fig_notes$view_rw, rw_n_caption_view_md())))
  
  rw_n_caption_view_md <- reactive({
    temp <- filtered2() %>% 
      filter(metric == "Relative Weight") %>% 
      filter(!is.na(N)) %>% 
      arrange(gcat)
    
    if (nrow(temp) == 0) return("")
    
    stand_text <- temp %>% 
      mutate(txt = paste0(gcat, " (*N* = ", N, ")")) %>% 
      pull(txt) %>% 
      paste(collapse = ", ")
    
    (paste0(
      "Sample sizes by proportional size distribution category: ", stand_text, "."
    ))
  })
  output$rw_n_caption_view <- renderUI(md_inline(rw_n_caption_view_md()))

  
  plotCPUE <- reactive({
    
    temp <- filtered2() %>%
      filter(metric == "CPUE") 
    
    N <- unique(temp$N)
    
    if(nrow(temp) == 0){
      fig <- ggplot() +
        annotate("text", x = 1, y = 1, size = 8,
                 label = "No CPUE data for selected options") +
        theme_void()
    } else {
      # Same layout as the Compare Your Data CPUE plot (draw_cpue_plot()),
      # with just the standardized block: Mean ± SE and Median (IQR), each
      # labeled with its value, N, and CPUE units.
      unit_label <- cpue_unit_label(temp$method[1], temp$waterbody_type[1])
      rows <- bind_rows(lapply(split(temp, temp$area), function(a) {
        a <- a %>% slice(1)
        se_safe <- coalesce(a$se, 0)
        bind_rows(
          a %>% transmute(row_label = "Mean ± SE",
                          center = mean, lo = mean - se_safe, hi = mean + se_safe,
                          value_label = mean_label_text(mean, se, N, unit_label), pct_label = NA_character_),
          a %>% transmute(row_label = "Median (IQR)",
                          center = `50%`, lo = `25%`, hi = `75%`,
                          value_label = median_label_text(`50%`, `25%`, `75%`, N, unit_label),
                          pct_label = NA_character_)
        ) %>% mutate(block = paste0(a$area, " — standardized data",
                                    if (!is.na(a$N)) paste0(" (N = ", a$N, " surveys)") else ""))
      })) %>%
        mutate(block = factor(block, levels = unique(block)),
               color = "black", fill = STANDARDIZED_FILL, shape = "point")
      x_limits <- range(c(rows$lo, rows$hi, rows$center), na.rm = TRUE)
      fig <- draw_cpue_plot(rows, unit_label, x_limits, view_cpue_label_size(), view_cpue_width_in())
    }

    print(fig)

  })

  # Font scaled to the plot's on-screen width, as on the Compare tab.
  view_cpue_width_in <- reactive({
    width_px <- session$clientData[["output_CPUE_plot-plot_width"]]
    if (!is.null(width_px) && width_px > 0) width_px / 72 else 10
  })
  view_cpue_label_size <- reactive(pmin(pmax(5.5 * view_cpue_width_in() / 12.5, 4), 6))

  callModule(plotDownload, "CPUE_plot", plotCPUE, width = 10, height = 4.5,
             apa = function() list(number = 3, title = view_title("catch per unit effort"),
                                   note = paste(fig_notes$view_cpue, cpue_units_note(input$methodchoice2, input$watertypechoice2))))

  output$view_cpue_note <- renderUI(md_inline(paste(fig_notes$view_cpue,
                                                    cpue_units_note(input$methodchoice2, input$watertypechoice2))))
  
  mtext <- "
<br>

<i>Before formatting your input data, ensure that there is corresponding data in the standardized dataset for your species, method, and waterbody type of interest by going to the View tab and selecting your values</i>

<center><b>How to Format Input Data</b></center>

You can upload your own data to compare to the standardized data, as a <b>CSV or Excel (.xlsx/.xls) file</b>. Each row of your file should be <b>one fish</b>, with its length and/or weight. (The webtool counts every row as one fish when calculating catch per unit effort, so don't combine several fish into one row.)

There are three ways to get your data in:

- <b>Option 1: Upload your own data sheet as it is (easiest)</b>, in whatever format it's in. The webtool guesses which of your column headings and species names match the fields described below (columns it doesn't need are ignored). <b>You check the guesses on the Map & Review Data tab and correct any that are wrong</b>, since the webtool can't tell when a guess is wrong. The webtool then puts your data into the correct format and loads it for you.
- <b>Option 2: Use our template.</b> Download the template with the DOWNLOAD button at the left, copy and paste your data into the matching columns, save it as a <b>CSV (.csv) or Excel (.xlsx) file</b>, and upload it. In Step 2, choose <b>Yes, it's already formatted</b> so the webtool reads your columns directly and skips the column checking.
- <b>Option 3: Your file is already in the webtool's format.</b> For example, a file you downloaded from the webtool's Map & Review Data tab last time, as a CSV or Excel file. In Step 2, choose <b>Yes, it's already formatted</b> and upload it; the column checking is skipped.
- <b>Excel files: only the first sheet is read.</b> For any option, put your data on the first (leftmost) sheet of the workbook, or save that sheet as a CSV file.

The webtool calculates three metrics for each combination of area, species, collection method, type of water body, and year:

1. Catch per unit effort (CPUE)
2. Length frequency
3. Relative weight

<center><b>Column Details</b></center>

Below are the details the app needs, expressed using the required column names/values -- these are what you'll match your own columns and species names to on the <b>Map & Review Data</b> tab, so your file doesn't need to use these names or spellings itself. Each upload must be from <b>one waterbody sampled with one collection method</b> (one waterbody name, state, method, and waterbody type per upload). Within that, you can include as many years as you want, with <b>one survey per year</b> (a survey spread over several days in the same year counts as one survey). Results are shown for each year and for all years combined. There is an example dataset at the bottom of the page with simulated data that may be a helpful guide.

1. CPUE requires `effort` and `transect_or_net_set` columns (if you don't have a transect or net set column, the app can number transects for you from the `effort` column -- see <b>Transect or net set</b> below)
2. Length frequency requires `total_length` column 
3. Relative weight requires `weight` and `total_length` columns

Required columns in input dataframe: 

- **Location**:
  - `state` is the name of the state/province where fish were collected. Every row is the same state (as it's for the same water body), so it's picked once on the **Map & Review Data** tab (Step A) rather than being a column -- if your file has coordinates anywhere in it, the app finds them automatically and pre-fills the state for you there; otherwise pick it yourself
  - `ecoregion` is optionally included, to compare your data to standardized data in that ecoregion -- like state, it's picked once on the same tab, pre-filled automatically from coordinates when possible
    - You can also find your ecoregion by clicking on your location the map on the Explore tab. Ecoregions can also be determined from [EPA ecoregions](https://www.epa.gov/eco-research/ecoregions-north-america) level 1.  Correctly formatted ecoregion names are listed below.
  - `waterbody_name` is the name of the water body. If your file has a waterbody name column (the template does), the name is filled in for you automatically; otherwise type it once in the box on the **Map & Review Data** tab (Step A). This name is used to label your data in all results tables and plots

<style>
.basic-styling td,
.basic-styling th {
  border: 1px solid #999;
  padding: 0.5rem;
}
</style>

<div class='ox-hugo-table basic-styling'>
<div></div>
<div class='table-caption'>
  <span class='table-number'></span>
</div>

| **Ecoregions**                    |
|-----------------------------------|
| 0 Water                           |
| 1 Arctic Cordillera               |
| 2 Tundra                          |
| 3 Taiga                           |
| 4 Hudson Plain                    |
| 5 Northern Forests                |
| 6 Northwestern Forested Mountains |
| 7 Marine West Coast Forest        |
| 8 Eastern Temperate Forests       |
| 9 Great Plains                    |
| 10 North American Deserts         |
| 11 Mediterranean California       |
| 12 Southern Semiarid Highlands    |
| 13 Temperate Sierras              |
| 14 Tropical Dry Forests           |
| 15 Tropical Wet Forests           |

<br>

- **Date**:
  - `year` is a four-digit numeric. **If your file has a collection date instead of a year column**, map that date on the Map & Review Data tab (Collection date field) and the year will be filled in automatically
- **Measurements**: 
  - `total_length` is fish total length (TL). It can be in millimeters, centimeters, or inches: pick the unit in the box under the Total length column on the **Map & Review Data** tab, and the webtool converts it to mm
  - `weight` is fish weight. It can be in grams, kilograms, ounces, or pounds: pick the unit in the box under the Weight column on the **Map & Review Data** tab, and the webtool converts it to grams. Weight is *optional*; leave it blank or choose `-- No column for this in my file --` in the Weight box if you did not weigh fish
  - `effort` is specified in **Collection method**. A column named `effort` is required, but the data is is *optional* and the rest of the column can be left blank
- **Transect or net set (sampling unit)**:
  - `transect_or_net_set` tells the app which individual sample each fish came from. A <b>transect</b> is one electrofishing run (for example, one 600-second shocking run along the shoreline); a <b>net set</b> is one net fished once (for example, one gill net set overnight, one trammel net drift, or one seine haul). A survey is usually made up of several transects or net sets
      - Use a simple number or text ID (e.g., 1, 2, 3... or T1, T2, T3...). Every fish caught in the same transect or net set gets the same ID, and each transect or net set gets a different ID. IDs can start over at 1 each year
      - Why it matters: CPUE is calculated separately for each transect or net set and then averaged, so the app can report a mean CPUE with a standard error (and a median), not just one pooled number
      - <b>No transect or net set column in your file?</b> That's OK. Choose `-- No column for this in my file --` in the Transect box on the <b>Map & Review Data</b> tab and map your Effort column instead. The app will number the transects for you, starting a new transect each time the effort value changes from the row above (for example, a run of rows with 600 seconds followed by rows with 540 seconds become transects 1 and 2). One limitation: two back-to-back transects with exactly the same effort are counted as one, so add a transect column if that happens in your data
- **Collection method**: see the table below for details
  - `method` is picked once for the whole file from a dropdown (Step A on the **Map & Review Data** tab), not a column, since one upload is always one method
  - `effort` is the effort for that **individual transect or net set** (not summed across the whole survey), report number of effort units
      - Examples of effort: 600 seconds for a single electrofishing transect, 1 net night for a single gill net set, 1 100-m drift for a single drifting trammel net haul
  - The 'gill_net_spring' method is for gill netting done between January and June (months 1 - 6), and 'gill_net_fall' is for between July and December (months 7 - 12)
    - If you're not sure which season your gill net survey falls in, or your data spans both, choose 'Gill net -- auto-detect spring/fall from Collection date' in the method dropdown and map a Collection date column instead
  - Data for additional collection methods will be added in the future

<center>

<style>
.basic-styling td,
.basic-styling th {
  border: 1px solid #999;
  padding: 0.5rem;
}
</style>

<div class='ox-hugo-table basic-styling'>
<div></div>
<div class='table-caption'>
  <span class='table-number'></span>
</div>

| **Method**          | **Effort Units** |
|--------------------------|-----------------|
| boat_electrofishing      | Seconds         |
| raft_electrofishing      | Seconds         |
| gill_net_fall            | Net nights      |
| gill_net_spring          | Net nights      |
| drifting_trammel_net     | 100-m drift     |
| large_catfish_hoopnet    | 24 hour set     |
| bag_seine                | 0.25 arc (small_standing_waters); 0.5 arc (rivers) |
| stream_seine             | 10-15 m haul    |
| backpack_electrofishing  | 100 m<sup>2</sup>            |
| tow_barge_electrofishing | 100 m<sup>2</sup>            |

</div>

</center>

- **Type of water body**:
  - `waterbody_type` is picked once for the whole file from a dropdown (Step A on the **Map & Review Data** tab): *large_standing_waters*, *small_standing_waters*, *wadeable_streams*, or *rivers*
- **Species**:
  - `common_name` must match one of the following species. On the **Map & Review Data** tab the webtool suggests a match for each species name in your file (common names, scientific names, or abbreviations), which you check and correct before continuing. You should know which common name each of your scientific names or species codes stands for; if there is any doubt, use the latest edition of the American Fisheries Society's *Common and Scientific Names of Fishes from the United States, Canada, and Mexico* (Names of Fishes):

<center>

<style>
.basic-styling td,
.basic-styling th {
  border: 1px solid #999;
  padding: 0.5rem;
}
</style>

<div class='ox-hugo-table basic-styling'>
<div></div>
<div class='table-caption'>
  <span class='table-number'></span>
</div>

| **Species**        | **Species**      | **Species**               |
|--------------------|------------------|---------------------------|
| Arctic Grayling    | Gizzard Shad     | Shorthead Redhorse        |
| Bigmouth Buffalo   | Grass Carp       | Silver Carp               |
| Black Bullhead     | Green Sunfish    | Smallmouth Bass           |
| Black Crappie      | Lake Trout       | Spotted Bass              |
| Blue Catfish       | Largemouth Bass  | Spotted Gar               |
| Bluegill           | Longnose Gar     | Striped Bass (landlocked) |
| Brook Trout        | Muskellunge      | Utah Chub                 |
| Brown Bullhead     | Northern Pike    | Walleye                   |
| Brown Trout        | Paddlefish       | Warmouth                  |
| Bull Trout         | Palmetto Bass    | White Bass                |
| Burbot             | Pumpkinseed      | White Crappie             |
| Chain Pickerel     | Rainbow Trout    | White Perch               |
| Channel Catfish    | Redear Sunfish   | White Sucker              |
| Common Carp        | River Carpsucker | Yellow Bass               |
| Cutthroat Trout    | Rock Bass        | Yellow Bullhead           |
| Flathead Catfish   | Sauger           | Yellow Perch              |
| Freshwater Drum    | Saugeye          |                           |

</div>

</center>  

<br>

<center><b>Example Dataset</b></center>

<br>  
"

output$instructions <- renderUI({
  tf <- tempfile()
  knit(text = mtext, output = tf)
  div(style = "background-color: #ffffff; border: 1px solid #e0dcd5; border-radius: 6px; padding: 18px 22px; margin-bottom: 15px;",
      # fragment.only: without it markdownToHTML emits a full HTML page whose
      # built-in stylesheet leaks into the whole app once this tab renders --
      # it caps the page body at 800px wide (squeezing every plot) and makes
      # pop-up windows semi-transparent via a global .fade rule.
      HTML(markdown::markdownToHTML(file = tf, fragment.only = TRUE)))
})

output$example <- renderDT(datatable(ex, options = list(lengthChange = FALSE, 
                                                        pageLength = 25)) %>%
                             formatRound(c(8:10), 0, mark ="") %>%
                             formatString(7))

plotLengthFrequencyuser <- reactive({
  # uu_unfiltered() has a req() for uploaded data, so no more error messages
  temp <- uu_processed() %>%
    bind_rows(filtered3(), .id = "id") %>%
    rename(data_source = id) %>%
    label_data_source() %>%
    filter(metric == "Length Frequency")

  stand_area <- temp %>%
    filter(data_source == "Standardized") %>%
    distinct(area) %>%
    pull(area) %>%
    first()

  stand_only <- temp %>%
    filter(data_source == "Standardized")

  if(nrow(stand_only) == 0){
    fig <- ggplot() +
      annotate("text", x = 1, y = 1, size = 8,
               label = "No standardized data \n corresponding to uploaded data") +
      theme_void()

  } else {
    # One color per selected year (plus "All years"), Standardized always
    # white -- so a multi-year selection shows every year side by side
    # instead of being limited to one bar per category.
    user_levels <- user_source_levels(temp)
    level_order <- c(user_levels, "Standardized")
    palette <- c(user_series_fills(user_levels), Standardized = STANDARDIZED_FILL)
    # Sample sizes in the legend, as in the original webtool: number of
    # standardized surveys, and number of the user's fish (stock length and up).
    first_n <- function(src) first(na.omit(temp$N[temp$data_source == src]))
    stand_n <- first_n("Standardized")
    legend_labels <- setNames(
      c(vapply(user_levels, function(lvl) {
          n <- first_n(lvl)
          paste0(user_series_label(lvl), if (!is.na(n)) paste0(" (*n* = ", n, " fish)") else "")
        }, character(1)),
        paste0(wrap_br(stand_area), " average",
               if (!is.na(stand_n)) paste0("<br>(*N* = ", stand_n, " surveys)") else "")),
      level_order)

    # Legend sits inside the plot (freeing the width it used to take on the
    # right for the bars), in whichever top corner has shorter bars under it.
    # If both corners have tall bars, the y-axis gets extra headroom above
    # 100% so the legend has empty space to sit in.
    cats <- levels(droplevels(factor(temp$gcat)))
    tops <- temp %>% mutate(top = mean + coalesce(se, 0)) %>% group_by(gcat) %>%
      summarise(top = max(top, na.rm = TRUE), .groups = "drop")
    side_max <- function(g) max(c(0, tops$top[tops$gcat %in% g]), na.rm = TRUE)
    left_max  <- side_max(head(cats, 2))
    right_max <- side_max(tail(cats, 2))
    legend_right <- right_max <= left_max
    y_top <- if (min(left_max, right_max) > 55) 100 + 12 * (length(level_order) + 1) else 100

    # A category with no fish has no row for that data source, and dodging
    # then slides the other source's bar into the empty slot (e.g. the North
    # America M-T bar drawn where the user's M-T bar belongs). No fish in a
    # category is 0% of them, so fill those in as 0 to keep every slot.
    temp <- temp %>%
      mutate(data_source = factor(data_source, levels = level_order)) %>%
      tidyr::complete(gcat, data_source, fill = list(mean = 0))

    fig <- ggplot(temp, aes(x = gcat, y = mean, fill = data_source)) +
      geom_bar(stat = "identity", width = 0.85,
               position = position_dodge(width = 0.9, preserve = "single"),
               color = "black")  +
      # Capped error bars drawn twice -- a wide white line under a black one
      # -- so they stand out on both the black standardized bars and the
      # user's white/gray bars, even when the SE is small.
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                    position = position_dodge(width = 0.9, preserve = "single"),
                    width = 0.35, linewidth = 2.6, color = "white", na.rm = TRUE) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                    position = position_dodge(width = 0.9, preserve = "single"),
                    width = 0.35, linewidth = 1, color = "black", na.rm = TRUE) +
      scale_y_continuous("Frequency (%)",
                         limits = c(0, y_top),
                         breaks = seq(0, 100, 20),
                         expand = c(0, 0)) +
      scale_fill_manual(values = palette, labels = legend_labels, name = "Data source") +
      theme_apa(base_size = 20) +
      xlab("Proportional size distribution categories") +
      theme(legend.position = "inside",
            legend.position.inside = if (legend_right) c(0.99, 0.99) else c(0.01, 0.99),
            legend.justification = if (legend_right) c(1, 1) else c(0, 1),
            legend.background = element_rect(fill = scales::alpha("white", 0.9), color = NA),
            legend.title = element_text(size = 15),
            legend.text = ggtext::element_markdown(size = 14))
  }

  print(fig)

})

callModule(plotDownload, "LF_plot_UU", plotLengthFrequencyuser, width = 13, height = 7,
           apa = function() list(number = 1, title = comparison_title("length frequency"),
                                 note = sub("{wb}", wb_label(), fig_notes$uu_lf, fixed = TRUE)))

plotRelativeWeightuser <- reactive({

  temp <- uu_processed() %>%
    bind_rows(filtered3(), .id = "id") %>%
    rename(data_source = id) %>%
    label_data_source() %>%
    filter(metric == "Relative Weight")

  stand_only <- temp %>%
    filter(data_source == "Standardized")

  user_only <- temp %>%
    filter(data_source != "Standardized")

  stand_area <- stand_only %>%
    distinct(area) %>%
    pull(area) %>%
    first()

  if (nrow(user_only) == 0){
    
    fig <- ggplot() +
      annotate("text", x = 1, y = 1, size = 8,
               label = "The uploaded data included no weights. \n Please include weights to generate this plot") +
      theme_void()
    
  } else if (all(is.na(user_only$mean))){

    fig <- ggplot() +
      annotate("text", x = 1, y = 1, size = 8,
               label = "No relative weights for uploaded data \n because species does not have standard \n weight equation, there is not standard \n weight equation for the 75th percentile, \n or the individual is shorter or longer \n than usable lengths for standard weight \n equation") +
      theme_void()

  } else if (nrow(stand_only) == 0){
    
    fig <- ggplot() +
      annotate("text", x = 1, y = 1, size = 8,
               label = "No standardized data \n corresponding to uploaded data") +
      theme_void()
    
  } else if (max(temp$mean) > 160){

    fig <- ggplot() +
      annotate("text", x = 1, y = 1, size = 8,
               label = "Relative weight exceeds max of 160") +
      theme_void()

  } else {

    user_levels <- user_source_levels(temp)
    level_order <- c(user_levels, "Standardized")
    fills  <- c(user_series_fills(user_levels), Standardized = STANDARDIZED_FILL)
    shapes <- c(user_series_shapes(user_levels), Standardized = STANDARDIZED_SHAPE)
    # Sample sizes in the legend: the user's fish with a relative weight
    # (summed over size categories), and the standardized surveys -- which
    # differ by size category, so shown as a range (exact numbers per
    # category are in the caption below the plot).
    user_n <- function(lvl) sum(temp$N[temp$data_source == lvl & !is.na(temp$mean)], na.rm = TRUE)
    stand_ns <- na.omit(stand_only$N)
    stand_n_text <- if (length(stand_ns) == 0) "" else if (min(stand_ns) == max(stand_ns)) {
      paste0("<br>(*N* = ", min(stand_ns), " surveys)")
    } else {
      paste0("<br>(*N* = ", min(stand_ns), "–", max(stand_ns), " surveys)")
    }
    legend_labels <- setNames(
      c(vapply(user_levels, function(lvl) paste0(user_series_label(lvl), " (*n* = ", user_n(lvl), " fish)"),
               character(1)),
        paste0(wrap_br(stand_area), stand_n_text)),
      level_order)

    temp <- temp %>% mutate(src = factor(data_source, levels = level_order))
    # Error bars drawn first so the markers sit on top of them, with caps
    # wider than a marker so an SE smaller than the marker still shows as a
    # line sticking out on each side. Caps shrink when many series share a
    # category so neighbors' caps don't touch.
    cap_w <- min(0.25, 0.55 / max(1, n_distinct(temp$src)))
    fig <- ggplot(temp, aes(x = gcat, group = src)) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                    width = cap_w, linewidth = 1, color = "black", na.rm = TRUE,
                    position = position_dodge(width = 0.6)) +
      geom_point(aes(y = mean, fill = src, shape = src),
                 size = 4.5, color = "black", stroke = 0.9,
                 position = position_dodge(width = 0.6)) +
      scale_x_discrete(drop = FALSE) +
      ylim(50, 160) +
      geom_hline(yintercept = 100, linetype = "dashed") +
      labs(y = "Relative weight (<i>W<sub>r</sub></i>)") +
      scale_fill_manual(name = "Data source", values = fills, labels = legend_labels) +
      scale_shape_manual(name = "Data source", values = shapes, labels = legend_labels) +
      theme_apa(base_size = 20) +
      xlab("Proportional size distribution categories") +
      theme(legend.position = "inside",
            legend.position.inside = c(0.99, 0.99),
            legend.justification = c(1, 1),
            legend.background = element_rect(fill = scales::alpha("white", 0.9), color = NA),
            legend.title = element_text(size = 15),
            legend.text = ggtext::element_markdown(size = 14),
            axis.title.y = ggtext::element_markdown())

  }
  
  print(fig)
  
})

callModule(plotDownload, "RW_plot_UU", plotRelativeWeightuser, width = 13, height = 7,
           apa = function() list(number = 2, title = comparison_title("relative weight"), note = paste(rw_caption_md(), rw_n_caption_md())))

rw_caption_md <- reactive({
  temp <- uu_processed() %>%
    bind_rows(filtered3(), .id = "id") %>%
    rename(data_source = id) %>%
    label_data_source() %>%
    filter(metric == "Relative Weight")

  stand_area <- temp %>%
    filter(data_source == "Standardized") %>%
    distinct(area) %>%
    pull(area) %>%
    first()

  if (is.null(stand_area) || length(stand_area) == 0) stand_area <- "the selected area"

  (paste0(
    "Black circles are the standardized average; white and gray markers are ", wb_label(), " data. Markers indicate means and vertical lines indicate ± 1 standard error; the dashed line marks a relative weight of 100. The two means are calculated differently: the ", stand_area,
    " value is the average of each contributing survey's own average relative weight in that category, while the ", wb_label(),
    " value is the average across the individual fish uploaded in that category."
  ))
})

rw_n_caption_md <- reactive({
  temp <- uu_processed() %>%
    bind_rows(filtered3(), .id = "id") %>%
    rename(data_source = id) %>%
    label_data_source() %>%
    filter(metric == "Relative Weight") %>%
    filter(!is.na(N)) %>%
    arrange(gcat)

  if (nrow(temp) == 0) return("")

  stand_area <- temp %>%
    filter(data_source == "Standardized") %>%
    distinct(area) %>%
    pull(area) %>%
    first()

  stand_text <- temp %>%
    filter(data_source == "Standardized") %>%
    mutate(txt = paste0(gcat, " (*N* = ", N, ")")) %>%
    pull(txt) %>%
    paste(collapse = ", ")

  # One clause per selected year/level (plus "All years"), so this stays
  # correct however many years are currently selected instead of assuming a
  # single "User upload" series.
  user_levels <- user_source_levels(temp)
  user_text <- paste(vapply(user_levels, function(lvl) {
    label_prefix <- user_series_label(lvl)
    txt <- temp %>% filter(data_source == lvl) %>%
      mutate(t = paste0(gcat, " (*n* = ", N, ")")) %>% pull(t) %>% paste(collapse = ", ")
    paste0(label_prefix, ": ", txt)
  }, character(1)), collapse = "; ")

  (paste0(
    "Sample sizes by proportional size distribution category: ", stand_area, ": ", stand_text,
    ". ", user_text, "."
  ))
})

# Measures the actual rendered width of text via grid font metrics (a
# grobWidth of a wrapped, multi-line string already returns the width of its
# widest line, so this works for wrapped axis labels too). fontface must
# match whatever the text actually renders as -- bold and italic glyphs are
# measurably wider than plain at the same point size, and the value/median
# labels this feeds into are rendered bold, so measuring them as plain text
# would systematically underestimate their true width.
text_width_in <- function(labels, font_size, fontface = "plain") {
  pt_const <- 72.27 / 25.4
  fontsize_pt <- font_size * pt_const
  vapply(labels, function(lab) {
    grid::convertWidth(grid::grobWidth(grid::textGrob(lab, gp = grid::gpar(fontsize = fontsize_pt, fontface = fontface))),
                       "inches", valueOnly = TRUE)
  }, numeric(1))
}

# A centered label whose left edge would cross the left boundary gets
# left-aligned instead (text extends rightward from its true value, away
# from the axis) -- needed for very low CPUE values (e.g. a rare species)
# that sit close to the y-axis. Symmetrically, a label whose right edge
# would cross the right boundary gets right-aligned instead (text extends
# leftward) -- needed for high CPUE values/long labels (e.g. "(N=2)" tacked
# onto an already-long IQR label) that sit close to the plot's right edge.
# The 20% pad is deliberate slack on top of the measured width:
# text-measurement via grid can still differ slightly from one rendering
# backend to the next (font hinting, subpixel rounding) even after matching
# device and font weight, so this errs toward flipping alignment a little
# early rather than clipping.
safe_hjust <- function(x, labels, label_size, left_boundary, right_boundary, data_units_per_inch, fontface = "plain") {
  half_width_units <- (text_width_in(labels, label_size, fontface) / 2) * data_units_per_inch * 1.2
  case_when(
    x - half_width_units < left_boundary  ~ 0,
    x + half_width_units > right_boundary ~ 1,
    TRUE ~ 0.5
  )
}

# SE is undefined (NA) with only one transect/net-set -- sd() of a single
# value has no meaning -- so the mean is shown alone, flagged with its N
# instead of a broken "X ± NA" label. With only two, SE is technically
# defined but rests on a single degree of freedom, so it's flagged the same
# way as a caution rather than presented as if it were a solid estimate.
mean_label_text <- function(mean, se, N, unit_label) {
  case_when(
    is.na(se) ~ paste0(round(mean, 1), " ", unit_label, " (N=", N, ")"),
    N <= 2    ~ paste0(round(mean, 1), " ± ", round(se, 1), " ", unit_label, " (N=", N, ")"),
    TRUE      ~ paste0(round(mean, 1), " ± ", round(se, 1), " ", unit_label)
  )
}

# IQR (25th-75th percentile) is the spread measure that pairs with a median,
# the same way SE pairs with a mean -- both are already computed for every
# CPUE row, so this adds no new statistics, just displays ones that existed
# but weren't shown. Below 3 transects/datasets the median is mathematically
# identical to the mean (the middle of one point is itself; the middle of
# two is their average) -- still shown, but flagged with its N so it reads
# as "this is the same as the mean, and here's why" rather than looking like
# an independent second estimate.
median_label_text <- function(median, p25, p75, N, unit_label) {
  base <- paste0(round(median, 1), " ", unit_label, " (IQR: ", round(p25, 1), "–", round(p75, 1), ")")
  ifelse(N <= 2, paste0(base, " (N=", N, ")"), base)
}

ordinal_suffix <- function(n) {
  if (n %% 100 %in% 11:13) return("th")
  switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
}

# Estimates the waterbody's approximate percentile rank by linear
# interpolation between the 5 known standardized percentile points (5th,
# 25th, 50th, 75th, 95th). An approximation, not an exact percentile, since
# only 5 reference points are known rather than the full underlying
# distribution.
estimate_pct_rank <- function(value, ref) {
  known_p <- c(5, 25, 50, 75, 95)
  known_x <- c(ref$`5%`, ref$`25%`, ref$`50%`, ref$`75%`, ref$`95%`)
  if (value <= known_x[1]) {
    max(1, round(5 * value / known_x[1]))
  } else if (value >= known_x[5]) {
    min(99, round(95 + (value - known_x[5]) / (known_x[5] - known_x[4]) * 4))
  } else {
    round(approx(x = known_x, y = known_p, xout = value)$y)
  }
}

  # ---- CPUE plot -------------------------------------------------------------
  #
  # Rows are grouped into blocks -- the standardized area first, then each
  # selected year in order, then "All years combined" last -- and every
  # statistic for a block (mean, median, pooled total) sits together under
  # one header, on a shaded band, so a reader never has to hunt for
  # which year a line belongs to. Row labels on the axis are short
  # ("Median (IQR)") because the block header already says whose data it is.
  #
  # Every row gets the same fixed physical height (cpue_row_height_in()),
  # enough for the bold value label above the point and the italic
  # percentile note below it, so labels can't overlap no matter how many
  # years are selected -- the plot just grows taller instead.

  # Plot data for the current selection: NULL plus a message when there's
  # nothing to draw, otherwise one row per statistic with its block.
  cpue_plot_data <- reactive({
    temp <- uu_processed() %>%
      bind_rows(filtered3(), .id = "id") %>%
      rename(data_source = id) %>%
      label_data_source() %>%
      filter(metric == "CPUE")

    stand_only <- temp %>% filter(data_source == "Standardized")
    user_only  <- temp %>% filter(data_source != "Standardized")

    if (nrow(user_only) == 0) {
      return(list(message = "CPUE cannot be shown because \n the uploaded data have no effort data."))
    }
    if (nrow(stand_only) == 0) {
      return(list(message = "No standardized data \n corresponding to uploaded data"))
    }

    unit_label <- cpue_unit_label(temp$method[1], temp$waterbody_type[1])
    stand_band <- stand_only %>% slice(1)
    # SE is undefined (NA) with only one transect/net-set, so treat it as
    # zero (no visible error bar) rather than blanking the x-range.
    stand_se_safe <- coalesce(stand_band$se, 0)

    # Years ascending, then All years combined last, so blocks read top to
    # bottom in time order. Shades match the other plots.
    user_levels <- user_source_levels(temp)
    user_fills  <- user_series_fills(user_levels)
    block_order <- c(setdiff(user_levels, ALL_YEARS_LABEL), intersect(ALL_YEARS_LABEL, user_levels))

    stand_rows <- bind_rows(
      stand_band %>% transmute(row_label = "Mean ± SE",
                               center = mean, lo = mean - stand_se_safe, hi = mean + stand_se_safe,
                               value_label = mean_label_text(mean, se, N, unit_label), pct_label = NA_character_),
      stand_band %>% transmute(row_label = "Median (IQR)",
                               center = `50%`, lo = `25%`, hi = `75%`,
                               value_label = median_label_text(`50%`, `25%`, `75%`, N, unit_label),
                               pct_label = "50th percentile")
    ) %>% mutate(block = paste0(stand_band$area, " — standardized data",
                                if (!is.na(stand_band$N)) paste0(" (N = ", stand_band$N, " surveys)") else ""),
                 color = "black", fill = STANDARDIZED_FILL, shape = "point")

    # Each year's Per-Transect Mean (± SE), Median (± IQR, with its
    # estimated percentile rank within the area's reference distribution),
    # and pooled Total catch / total effort (a single ratio: no error bar).
    build_block <- function(lvl) {
      ut  <- user_only %>% filter(data_source == lvl, cpue_calc == "transect")
      tot <- user_only %>% filter(data_source == lvl, cpue_calc == "total")
      out <- NULL
      if (nrow(ut) > 0) {
        se_safe  <- coalesce(ut$se, 0)
        pct_rank <- estimate_pct_rank(ut$`50%`, stand_band)
        out <- bind_rows(
          ut %>% transmute(row_label = "Per-transect mean ± SE",
                           center = mean, lo = mean - se_safe, hi = mean + se_safe,
                           value_label = mean_label_text(mean, se, N, unit_label), pct_label = NA_character_,
                           shape = "point"),
          ut %>% transmute(row_label = "Median (IQR)",
                           center = `50%`, lo = `25%`, hi = `75%`,
                           value_label = median_label_text(`50%`, `25%`, `75%`, N, unit_label),
                           pct_label = paste0("~", pct_rank, ordinal_suffix(pct_rank), " percentile"),
                           shape = "point"))
      }
      if (nrow(tot) > 0) {
        out <- bind_rows(out, tot %>% transmute(row_label = "Total catch ÷ total effort",
                                                center = mean, lo = NA_real_, hi = NA_real_,
                                                value_label = paste0(round(mean, 1), " ", unit_label),
                                                pct_label = NA_character_, shape = "diamond"))
      }
      if (is.null(out)) return(NULL)
      # Sample sizes in the header: transects/net sets averaged, and total
      # fish caught (the pooled total row's N).
      sizes <- c(if (nrow(ut) > 0 && !is.na(ut$N[1])) paste0(ut$N[1], if (ut$N[1] == 1) " transect" else " transects"),
                 if (nrow(tot) > 0 && !is.na(tot$N[1])) paste0(tot$N[1], " fish"))
      header <- user_series_label(lvl)
      if (length(sizes) > 0) header <- paste0(header, " (n = ", paste(sizes, collapse = ", "), ")")
      out %>% mutate(block = header, color = "black", fill = user_fills[[lvl]])
    }

    rows <- bind_rows(stand_rows, bind_rows(lapply(block_order, build_block)))
    rows$block <- factor(rows$block, levels = unique(rows$block))

    list(rows = rows, unit_label = unit_label,
         x_limits = range(c(rows$lo, rows$hi, rows$center), na.rm = TRUE))
  })

  # Vertical position of every row and block header, top to bottom, in "row
  # units" (1 unit = one statistic's row). Headers take 0.8 of a row and
  # there's a small gap between blocks.
  cpue_layout <- function(rows) {
    cursor <- 0
    rows$y <- NA_real_
    headers <- list()
    for (b in levels(droplevels(rows$block))) {
      idx <- which(rows$block == b)
      header_y <- cursor
      cursor <- cursor - 0.8
      for (i in idx) {
        rows$y[i] <- cursor
        cursor <- cursor - 1
      }
      headers[[b]] <- tibble(block = b, header_y = header_y,
                             top = header_y + 0.45, bottom = rows$y[max(idx)] - 0.55,
                             color = rows$color[idx[1]],
                             standardized = rows$fill[idx[1]] == STANDARDIZED_FILL)
      cursor <- cursor - 0.3
    }
    headers <- bind_rows(headers)
    # The standardized block always gets a darker band so it stands out;
    # the user's blocks alternate light gray / white.
    headers$band_fill <- ifelse(headers$standardized, "grey85",
                                ifelse(seq_len(nrow(headers)) %% 2 == 1, "grey95", NA))
    headers$band <- !is.na(headers$band_fill)
    list(rows = rows, headers = headers,
         y_limits = c(min(headers$bottom) - 0.05, max(headers$top) + 0.05))
  }

  # Physical height of one row: room for the value label above the point
  # and the percentile note below it, with clear space in between.
  cpue_row_height_in <- function(label_size) 0.155 * label_size

  # Whole plot height for a given set of rows: the panel (row units x row
  # height) plus the x-axis title/ticks and margins.
  cpue_plot_height <- function(rows, label_size) {
    lay <- cpue_layout(rows)
    diff(lay$y_limits) * cpue_row_height_in(label_size) + 0.25 * label_size + 0.3
  }

  # Draws one CPUE plot for the given rows (all of them on screen, or one
  # page's worth for a multi-page download). x_limits is passed in so every
  # page of a multi-page download shares the same x scale.
  draw_cpue_plot <- function(rows, unit_label, x_limits, label_size, plot_width_in, page_note = NULL) {
    # Text-width measurements need a graphics device open to report font
    # metrics; ggsave() evaluates the plot before opening its own device, so
    # open a fixed one here to get the same answer regardless of caller.
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)

    lay <- cpue_layout(rows)
    rows <- lay$rows
    headers <- lay$headers

    x_range <- diff(x_limits)
    if (x_range == 0) x_range <- max(abs(x_limits[1]), 1)
    x_expand <- c(0.06, 0.08)
    left_boundary  <- x_limits[1] - x_expand[1] * x_range
    right_boundary <- x_limits[2] + x_expand[2] * x_range

    # The y-axis row labels claim part of the canvas width, so subtract them
    # to get the panel's true width before deciding label alignment.
    axis_label_width_in <- max(text_width_in(rows$row_label, label_size))
    available_panel_in <- plot_width_in - axis_label_width_in - 0.5
    data_units_per_inch <- (right_boundary - left_boundary) / available_panel_in

    # Value labels stay centered over their point unless that would run past
    # the left or right edge, in which case they're aligned to extend away
    # from that edge instead.
    rows <- rows %>%
      mutate(hjust = safe_hjust(center, value_label, label_size, left_boundary, right_boundary, data_units_per_inch, fontface = "bold"))
    pct_rows <- rows %>%
      filter(!is.na(pct_label)) %>%
      mutate(pct_hjust = safe_hjust(center, pct_label, label_size * 0.85, left_boundary, right_boundary, data_units_per_inch, fontface = "italic"))
    bar_rows <- rows %>% filter(!is.na(lo))

    fig <- ggplot() +
      # Alternating light bands mark each block (area, year, or all years).
      geom_rect(data = headers %>% filter(band),
                aes(xmin = -Inf, xmax = Inf, ymin = bottom, ymax = top, fill = I(band_fill)),
                color = NA) +
      geom_text(data = headers, aes(x = left_boundary + 0.08 * data_units_per_inch, y = header_y, label = block, color = I(color)),
                hjust = 0, size = label_size * 1.05, fontface = "bold") +
      geom_errorbar(data = bar_rows, aes(xmin = lo, xmax = hi, y = y, color = I(color)),
                    width = 0.18, linewidth = 0.7, orientation = "y") +
      # Black-outlined markers filled with the series' shade (standardized
      # solid black; the user's white or gray), matching the other plots.
      geom_point(data = rows %>% filter(shape == "point"), aes(x = center, y = y, fill = I(fill)),
                 shape = 21, color = "black", stroke = 0.9, size = 3.5) +
      # Pooled total: a diamond with no error bar -- one ratio, not a distribution.
      geom_point(data = rows %>% filter(shape == "diamond"), aes(x = center, y = y, fill = I(fill)),
                 shape = 23, color = "black", stroke = 0.9, size = 4) +
      geom_text(data = rows, aes(x = center, y = y + 0.3, label = value_label, hjust = hjust, color = I(color)),
                size = label_size, fontface = "bold") +
      geom_text(data = pct_rows, aes(x = center, y = y - 0.3, label = pct_label, hjust = pct_hjust, color = I(color)),
                size = label_size * 0.85, fontface = "italic") +
      scale_x_continuous(paste0("CPUE (", unit_label, ")"), expand = expansion(mult = x_expand)) +
      scale_y_continuous(breaks = rows$y, labels = rows$row_label, expand = expansion(0)) +
      coord_cartesian(xlim = x_limits, ylim = lay$y_limits, clip = "off") +
      theme_apa(base_size = label_size * 3.6) +
      theme(axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.margin = margin(10, 20, 10, 10))

    if (!is.null(page_note)) {
      fig <- fig + labs(caption = page_note) + theme(plot.caption = element_text(size = label_size * 2.8, color = "grey40"))
    }
    fig
  }

  cpue_message_plot <- function(message) {
    ggplot() +
      annotate("text", x = 1, y = 1, size = 8, label = message) +
      theme_void()
  }

  # On screen: full width of the tab, font scaled to that width, and the
  # height grows with the number of rows so spacing stays fixed.
  cpue_screen_width_in <- reactive({
    width_px <- session$clientData[["output_CPUE_plot_UU-plot_width"]]
    if (!is.null(width_px) && width_px > 0) width_px / 72 else 12.5
  })
  cpue_screen_label_size <- reactive(pmin(pmax(5.5 * cpue_screen_width_in() / 12.5, 4), 6))

  cpue_screen_height_in <- reactive({
    d <- cpue_plot_data()
    if (is.null(d$rows)) return(5)
    cpue_plot_height(d$rows, cpue_screen_label_size())
  })

  output$cpue_plot_container <- renderUI({
    tagList(
      plot_download_buttons("CPUE_plot_UU-download_plot", "CPUE_plot_UU-download_word"),
      fluidRow(
        plotOutput("CPUE_plot_UU-plot", height = paste0(round(cpue_screen_height_in() * 72), "px"), width = "100%")
      )
    )
  })

  output[["CPUE_plot_UU-plot"]] <- renderPlot({
    d <- cpue_plot_data()
    if (is.null(d$rows)) return(print(cpue_message_plot(d$message)))
    print(draw_cpue_plot(d$rows, d$unit_label, d$x_limits, cpue_screen_label_size(), cpue_screen_width_in()))
  }, height = function() round(cpue_screen_height_in() * 72))

  # Download: portrait, letter-size width (8.5in). If everything fits on one
  # page it's a single PNG; otherwise the blocks are split across as many
  # 8.5 x 11in pages as needed (never splitting a block) in one PDF, all
  # pages sharing the same x scale so they can be compared directly.
  cpue_download_label_size <- 4.2
  cpue_page_w <- 8.5
  cpue_page_h <- 11

  cpue_download_pages <- reactive({
    d <- cpue_plot_data()
    if (is.null(d$rows)) return(NULL)
    blocks <- levels(d$rows$block)
    pages <- list()
    current <- character(0)
    for (b in blocks) {
      trial <- c(current, b)
      fits <- cpue_plot_height(d$rows %>% filter(block %in% trial), cpue_download_label_size) + 0.4 <= cpue_page_h
      if (!fits && length(current) > 0) {
        pages[[length(pages) + 1]] <- current
        current <- b
      } else {
        current <- trial
      }
    }
    pages[[length(pages) + 1]] <- current
    pages
  })

  output[["CPUE_plot_UU-download_plot"]] <- downloadHandler(
    filename = function() {
      pages <- cpue_download_pages()
      if (!is.null(pages) && length(pages) > 1) "cpue_plot.pdf" else "cpue_plot.png"
    },
    content = function(file) {
      d <- cpue_plot_data()
      pages <- cpue_download_pages()
      if (is.null(pages)) {
        ggsave(file, cpue_message_plot(d$message), width = cpue_page_w, height = 5, device = "png")
      } else if (length(pages) == 1) {
        h <- cpue_plot_height(d$rows, cpue_download_label_size)
        ggsave(file, draw_cpue_plot(d$rows, d$unit_label, d$x_limits, cpue_download_label_size, cpue_page_w),
               width = cpue_page_w, height = h, device = "png", dpi = 300)
      } else {
        # Each page is drawn at its natural height at the top of the sheet,
        # so a short last page keeps the same row spacing as the others.
        grDevices::pdf(file, width = cpue_page_w, height = cpue_page_h, onefile = TRUE)
        for (i in seq_along(pages)) {
          page_rows <- d$rows %>% filter(block %in% pages[[i]]) %>% mutate(block = droplevels(block))
          fig <- draw_cpue_plot(page_rows, d$unit_label, d$x_limits, cpue_download_label_size, cpue_page_w,
                                page_note = paste0("Page ", i, " of ", length(pages)))
          h <- min(cpue_plot_height(page_rows, cpue_download_label_size) + 0.4, cpue_page_h)
          grid::grid.newpage()
          print(fig, vp = grid::viewport(y = 1, height = grid::unit(h, "in"), just = "top"))
        }
        grDevices::dev.off()
      }
    }
  )

  # Word figure: the same page images as the PNG/PDF download (a long
  # multi-year plot continues over several images), with the AFS-style
  # caption below (see build_figure_docx()).
  output[["CPUE_plot_UU-download_word"]] <- downloadHandler(
    filename = function() "figure_3.docx",
    content = function(file) {
      d <- cpue_plot_data()
      pages <- cpue_download_pages()
      images <- if (is.null(pages)) {
        png <- tempfile(fileext = ".png")
        ggsave(png, cpue_message_plot(d$message), width = cpue_page_w, height = 5, dpi = 300)
        list(list(path = png, width = cpue_page_w, height = 5))
      } else {
        lapply(pages, function(p) {
          page_rows <- d$rows %>% filter(block %in% p) %>% mutate(block = droplevels(block))
          h <- cpue_plot_height(page_rows, cpue_download_label_size)
          png <- tempfile(fileext = ".png")
          ggsave(png, draw_cpue_plot(page_rows, d$unit_label, d$x_limits, cpue_download_label_size, cpue_page_w),
                 width = cpue_page_w, height = h, dpi = 300)
          list(path = png, width = cpue_page_w, height = h)
        })
      }
      build_figure_docx(file, 3, comparison_title("catch per unit effort"), cpue_caption_md(), images)
    }
  )

cpue_caption_md <- reactive({
  temp <- uu_processed() %>%
    bind_rows(filtered3(), .id = "id") %>%
    rename(data_source = id) %>%
    label_data_source() %>%
    filter(metric == "CPUE")

  stand_area <- temp %>%
    filter(data_source == "Standardized") %>% 
    distinct(area) %>% 
    pull(area) %>% 
    first()
  
  if (is.null(stand_area) || length(stand_area) == 0) stand_area <- "the selected area"
  
  (paste0(
    "The standardized average for ", stand_area, " (black markers, dark gray band) is shown first, followed by ", wb_label(),
    " (white or gray markers). *Mean ± SE* is the mean catch per unit effort (CPUE) ± 1 standard error, across standardized surveys or across the ", wb_label(), " transects/net sets. ",
    "*Median (IQR)* is the median, with the interquartile range (25th–75th percentile) as the horizontal line; the percentile beneath each ", wb_label(), " median is its approximate rank among the standardized surveys. ",
    "*Total catch ÷ total effort* (diamond) is the total number of fish divided by the total effort across all transects/net sets; as a single ratio it has no error bar. ",
    "With only one or two transects/net sets, the median equals the mean and is labeled with its *N*. ",
    "*N* = number of standardized surveys; *n* = number of ", wb_label(), " transects/net sets and fish. ",
    cpue_units_note(input$methodchoice3, input$watertypechoice3)
  ))
})

# The data-dependent figure notes, shown on screen inline after "Note."
output$rw_caption   <- renderUI(md_inline(rw_caption_md()))
output$rw_n_caption <- renderUI(md_inline(rw_n_caption_md()))
output$cpue_caption <- renderUI(md_inline(cpue_caption_md()))

}

shinyApp(ui, server)
