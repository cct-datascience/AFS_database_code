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
# library(periscope)

# Module code
plotDownloadUI <- function(id, height = 400, width = "100%") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        2, offset = 10,
        downloadButton(ns("download_plot"), "Download plot")
      )
    ),
    fluidRow(
      plotOutput(ns('plot'), height = height, width = width)
    )

  )
}

plotDownload <- function(input, output, session, plotFun, width = 9, height = 6, file_ext = "png") {
  output$plot <- renderPlot({
    plotFun()
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("plot.", file_ext)
    },
    content = function(file) {
      ggsave(file, plotFun(), width = width, height = height)
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

# # Read in lat/longs
# locs <- read_csv("sites.csv") %>%
locs <- read_csv("toy_locs.csv") %>%
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

# For each value in `bad`, find the closest fuzzy match in `valid` (if any)
# to suggest as a likely intended spelling in the validation warning.
suggest_closest <- function(bad, valid) {
  vapply(bad, function(x) {
    hit <- agrep(x, valid, max.distance = 0.3, ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) hit[1] else NA_character_
  }, character(1))
}

# Read a user's uploaded CSV and normalize whitespace in the state/ecoregion
# columns (trim + collapse doubled/irregular internal spaces). These columns
# are matched against standardized area names by exact string equality
# elsewhere, so a stray double space (invisible in normal text -- e.g. "8
# Eastern Temperate Forests" typed with two spaces after "8") would otherwise
# silently break that match with no visible difference in the UI.
read_user_upload <- function(path) {
  uu <- read_csv(path)
  if ("state" %in% names(uu))     uu$state     <- str_squish(uu$state)
  if ("ecoregion" %in% names(uu)) uu$ecoregion <- str_squish(uu$ecoregion)
  uu
}


#Shiny App
ui <- navbarPage(title = "AFS Standard Sampling App",
                 header = tags$a(style='background-color:white;position:absolute;right:10px;top:15px;',tags$img(src='AFS_logo.png'), href = "https://fisheries.org/"),
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
                                         DTOutput("filtertable"))
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
                            mainPanel(plotDownloadUI("LF_plot"),
                                      br(), 
                                      p("Proportional size distribution length frequency. Black lines indicate standard error.", style = "font-size:25px;"),
                                      hr(),
                                      plotDownloadUI("RW_plot"),
                                      br(), 
                                      p("Relative weight by proportional size distribution categories. Points indicate means and lines indicate standard error.", style = "font-size:25px;"),
                                      uiOutput("rw_n_caption_view"),
                                      hr(), 
                                      plotDownloadUI("CPUE_plot", height = "200px"), 
                                      br(), 
                                      p("Catch per unit effort. The vertical line marks the mean CPUE, with the shaded band showing +/- 1 standard error. CPUE units depend on collection method: boat and raft electrofishing are fish per hour, gill net is fish per net nights, drifting trammel net is fish per 100-m drift, large catfish hoopnet is fish per 24 hour set, bag seine is fish per 0.25 arc (small standing waters) or 0.5 arc (rivers), stream seine is fish per 10-15m haul, and backpack and tow barge electrofishing are fish per 100m².", style = "font-size:25px;")
                            )
                          )
                 ),
                 tabPanel(title = tagList(icon("arrow-right-arrow-left"), "Compare Your Data"),
                          sidebarLayout(
                            sidebarPanel(
                              div(style = "background-color: #f7f3ee; border-left: 4px solid #325d88; padding: 12px 15px; border-radius: 4px; margin-bottom: 15px;",
                                  h5(icon("fish"), " Welcome!", style = "margin-top: 0;"),
                                  p("Compare your own fish survey data to standardized values across North America, your ecoregion, or your state.", 
                                    style = "margin-bottom: 0; font-size: 14px;")
                              ),
                              div(style = "margin-bottom: 18px;",
                                  h5(span(style = "display:inline-block; background-color:#325d88; color:white; border-radius:50%; width:24px; height:24px; text-align:center; line-height:24px; margin-right:8px;", "1"), 
                                     "Get the template"),
                                  downloadButton("download", "Download CSV template"),
                                  p("Use this template if you need it. If your data is already in a properly formatted CSV file, you can skip ahead to Step 2.", 
                                    style = "font-size: 13px; margin: 6px 0 0 32px; color: #555;")
                              ),
                              div(style = "margin-bottom: 18px;",
                                  h5(span(style = "display:inline-block; background-color:#325d88; color:white; border-radius:50%; width:24px; height:24px; text-align:center; line-height:24px; margin-right:8px;", "2"), 
                                     "Upload your data"),
                                  fileInput("upload", NULL,
                                            buttonLabel =  "Choose file...",
                                            multiple = FALSE, 
                                            accept = (".csv"), 
                                            placeholder = "No file selected"),
                                  textInput("waterbody_label", "Waterbody name (optional)",
                                            placeholder = "e.g., Alamo Lake")
                              ),
                              uiOutput("uu_validation_warnings"),
                              div(style = "margin-bottom: 8px;",
                                  h5(span(style = "display:inline-block; background-color:#325d88; color:white; border-radius:50%; width:24px; height:24px; text-align:center; line-height:24px; margin-right:8px;", "3"),
                                     "Choose what to compare")
                              ),
                              uiOutput("dyn_type"),
                              uiOutput("dyn_area3"),
                              uiOutput("dyn_spp3"),
                              uiOutput("dyn_method3"),
                              uiOutput("dyn_watertype3"),
                              hr(),
                              div(style = "margin-bottom: 8px;",
                                  h5(span(style = "display:inline-block; background-color:#325d88; color:white; border-radius:50%; width:24px; height:24px; text-align:center; line-height:24px; margin-right:8px;", "4"), 
                                     "See your results"),
                                  p("Your results appear in the ", strong("Results Table"), " and ", strong("Results Plots"), " tabs to the right.", 
                                    style = "font-size: 13px; margin-left: 32px;")
                              ),
                              downloadButton("filterdownloaduu", "Download plot data")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel(tagList(icon("circle-info"), "Instructions"), 
                                         uiOutput("instructions"), 
                                         DTOutput("example")),
                                tabPanel(tagList(icon("table"), "Results Table"), 
                                         br(),
                                         h6("If no table is displayed, upload a dataset and choose your filters in the sidebar."),
                                         br(),
                                         DTOutput("uu_results_table"),
                                         uiOutput("n_definitions")),
                                tabPanel(tagList(icon("chart-column"), "Results Plots"), 
                                         br(), 
                                         h6("If no plots are displayed, upload a dataset to generate comparison plots."), 
                                         br(), 
                                         plotDownloadUI("LF_plot_UU"),
                                         br(), 
                                         p("Proportional size distribution length frequency. Black lines at middle top of standardized data bars indicate standard error.", style = "font-size:25px;"),
                                         hr(),
                                         plotDownloadUI("RW_plot_UU"),
                                         br(), 
                                         div(style = "font-size:25px;", uiOutput("rw_caption")),
                                         uiOutput("rw_n_caption"),
                                         hr(),
                                         plotDownloadUI("CPUE_plot_UU", height = "400px", width = "100%"),
                                         br(), 
                                         div(style = "font-size:25px;", uiOutput("cpue_caption")))
                              )
                            )
                            
                          ), 
                          
                 )
                 
)




server <- function(input, output, session) {
  
  wrap_br <- function(x, width = 22) {
    paste(strwrap(x, width = width), collapse = "<br>")
  }
  
  wb_label <- reactive({
    if (is.null(input$waterbody_label) || trimws(input$waterbody_label) == "") {
      "Waterbody"
    } else {
      trimws(input$waterbody_label)
    }
  })
  
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
  
  # uu_raw: used only for populating sidebar dropdowns — reads the original
  # uploaded CSV once. Uses the same file path as uu_metrics_cache but is a 
  # separate reactive so dropdown population doesn't depend on metric calculations.
  uu_raw <- reactive({
    req(input$upload)
    uu <- read_user_upload(input$upload$datapath)
    
    uu_state <- uu %>% select(-any_of("ecoregion")) %>% mutate(type = "state") %>% rename(area = state)
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
  uu_validation <- reactive({
    req(input$upload)
    raw <- uu_raw()

    bad_ecoregion <- raw %>% filter(type == "ecoregion") %>% distinct(area) %>% pull() %>%
      setdiff(valid_ecoregions)

    bad_state <- raw %>% filter(type == "state") %>% distinct(area) %>% pull() %>%
      setdiff(valid_states)

    list(bad_ecoregion = bad_ecoregion, bad_state = bad_state)
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

    msgs <- tagList(
      warning_block(v$bad_ecoregion, valid_ecoregions, "ecoregion"),
      warning_block(v$bad_state, valid_states, "state/province")
    )

    if (length(v$bad_ecoregion) == 0 && length(v$bad_state) == 0) return(NULL)

    div(style = "background-color:#fdf2e9; border-left:4px solid #e67e22; padding:10px 14px; border-radius:4px; margin-bottom:15px; font-size:13px;",
        icon("triangle-exclamation"), strong(" Data check:"),
        msgs)
  })

  # Render a UI for selecting area
  output$dyn_type <- renderUI({
    req(input$upload)
    
    temp <- uu_raw() %>%
      select(type) %>%
      unique() %>%
      mutate(types = case_when(type == "all" ~ "North America",
                               type == "ecoregion" ~ "Ecoregion",
                               type == "state" ~ "State/Province")) %>%
      slice(match(c("North America", "Ecoregion", "State/Province"), types)) %>% 
      pull(types)
    
    radioGroupButtons(inputId = "typechoice3",
                      label = "Show data by:",
                      choices = temp,
                      direction = "vertical",
                      selected = temp[1])
  })
  
  # Render a UI for selecting area depending on North America, Ecoregions, or State/Province for tab 3
  output$dyn_area3 <- renderUI({
    req(input$upload)
    
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
      mutate(method = str_replace_all(method, "_", " "),
             area = str_replace_all(area, "_", " "),
             waterbody_type = str_replace_all(waterbody_type, "_", " ")) %>%
      dplyr::mutate(N = as.integer(N),
                    mean = round(mean, 2),
                    se = round(se, 2),
                    `5%` = round(`5%`, 2), 
                    `25%` = round(`25%`, 2),
                    `50%` = round(`50%`, 2),
                    `75%` = round(`75%`, 2),
                    `95%` = round(`95%`, 2)) %>%
      dplyr::rename("Area" = area,
                    "Common Name" = common_name,
                    "Method" = method,
                    "Waterbody Type" = waterbody_type,
                    "Proportional Size Distribution" = gcat,
                    "Metric" = metric, 
                    "N" = N,
                    "Mean" = mean,
                    "SE" = se,
                    "5%" = `5%`,
                    "25%" = `25%`,
                    "50%" = `50%`,
                    "75%" = `75%`,
                    "95%" = `95%`)
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
  # This is the expensive step (CSV read + psdAdd + 3 metric calculations).
  # It is cached here and only re-runs when a new file is uploaded.
  uu_metrics_cache <- reactive({
    req(input$upload)
    uu <- read_user_upload(input$upload$datapath)
    
    uu_state <- uu %>% select(-any_of("ecoregion")) %>% mutate(type = "state")  %>% rename(area = state)
    uu_all    <- uu %>% select(-state, -any_of("ecoregion")) %>% mutate(type = "all", area = "North America")
    
    uu_all <- bind_rows(uu_all, uu_state)
    
    if ("ecoregion" %in% names(uu)) {
      uu_all <- bind_rows(uu_all, uu %>% select(-state) %>% mutate(type = "ecoregion") %>% rename(area = ecoregion))
    }
    
    # FSA sub-group assignment (collapsed from 20 conditions to 4)
    trout_spp   <- c("Brown Trout", "Brook Trout", "Cutthroat Trout", "Rainbow Trout")
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
      left_join(uu_counts, by = c("type", "area", "common_name", "method", "waterbody_type"),
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

  # uu_processed: fast filter-only step, re-runs when dropdowns change
  uu_processed <- reactive({
    req(input$areachoice3, input$sppchoice3, input$methodchoice3, input$watertypechoice3)
    
    uu_metrics_cache() %>%
      filter(metric         %in% uni.metric,
             area           %in% input$areachoice3,
             common_name    %in% input$sppchoice3,
             method         %in% input$methodchoice3,
             waterbody_type %in% input$watertypechoice3) %>%
      arrange(common_name) %>%
      select(area, common_name, method, waterbody_type, gcat, metric, cpue_calc, cpue_method, N, mean,
             se, `5%`, `25%`, `50%`, `75%`, `95%`)
  })
  
  output$uu_results_table <- renderDT({
    temp <- both() %>% 
      mutate(across(c(mean, se, `5%`, `25%`, `50%`, `75%`, `95%`), ~round(., 1)))
    
    datatable(temp, options = list(lengthChange = FALSE, pageLength = 25))
  })
  
  output$n_definitions <- renderUI({
    temp <- both()
    if (nrow(temp) == 0) return(NULL)
    
    metrics_present <- unique(temp$metric)
    
    # Get actual region name from the standardized rows
    stand_area <- temp %>% 
      filter(!(data_source %in% c(wb_label(), "Waterbody"))) %>%
      distinct(data_source) %>% 
      pull(data_source) %>% 
      first()
    if (is.null(stand_area) || is.na(stand_area)) stand_area <- "the selected area"
    
    wb <- wb_label()
    
    lines <- c()
    if ("CPUE" %in% metrics_present) {
      lines <- c(lines, paste0(
        "<li><strong>CPUE rows</strong> &mdash; for <em>", stand_area,
        "</em>, <em>N</em> is the number of contributing surveys/datasets averaged together. For <em>", wb,
        "</em>, two CPUE values are shown: the <strong>Per-Transect Mean (&plusmn; SE)</strong> row's <em>n</em> is the number of transects or net sets averaged together; the <strong>Total (Pooled)</strong> row's <em>n</em> is the total number of fish (of that species) used in the pooled total-fish/total-effort ratio, and has no standard error since it is a single combined value rather than an average across samples.</li>"))
    }
    if ("Length Frequency" %in% metrics_present) {
      lines <- c(lines, paste0(
        "<li><strong>Length Frequency rows</strong> &mdash; for <em>", stand_area,
        "</em>, <em>N</em> is the number of contributing surveys/datasets. For <em>", wb,
        "</em>, <em>n</em> is the total number of fish (stock length and above) in the survey.</li>"))
    }
    if ("Relative Weight" %in% metrics_present) {
      lines <- c(lines, paste0(
        "<li><strong>Relative Weight rows</strong> &mdash; for <em>", stand_area,
        "</em>, <em>N</em> is the number of contributing surveys/datasets that included fish in that size category. For <em>", wb,
        "</em>, <em>n</em> is the number of individual fish in that size category.</li>"))
    }
    
    HTML(paste0(
      "<div style='font-size:14px; color:#555; margin-top:10px;'>",
      "<strong>What does N/n mean in this table?</strong><ul>",
      paste(lines, collapse = ""),
      "</ul></div>"
    ))
  })
  
  # For downloading the filtered dataset for tab 3 comparison
  
  both <- reactive({
    
    both_df <- uu_processed() %>%
      bind_rows(filtered3(), .id = "id") %>%
      rename(data_source = id) %>%
      mutate(data_source = case_when(data_source == 1 ~ wb_label(),
                                     data_source == 2 ~ area),
             cpue_method = coalesce(cpue_method, "")) %>%
      arrange(metric, common_name, cpue_calc) %>%
      select(metric, data_source, common_name, method, waterbody_type, gcat, cpue_method, N, mean,
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
        geom_errorbar(aes(ymin = mean - se,
                          ymax = mean + se),
                      width = 0) +
        scale_y_continuous("Frequency (%)",
                           limits = c(0, 100),
                           expand = c(0, 0)) +
        theme_classic(base_size = 20) +
        xlab("Proportional size distribution categories") +
        theme(legend.position = "none") +
        geom_richtext(x = 5, y = 90, label = paste0("<i>N</i> = ", N), size = 7, fill = NA, label.color = NA)
    }
    
    print(fig)
    
  })
  
  callModule(plotDownload, "LF_plot", plotLengthFrequency)
  
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
        geom_point(aes(y = mean),
                   size = 4, 
                   color = "black") +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                      width = 0, 
                      color = "black") +
        geom_hline(yintercept = 100, linetype = "dashed") +
        scale_x_discrete(drop = FALSE) +
        ylim(50, 160) +
        labs(y = "Relative weight (<i>W<sub>r</sub></i>)") +
        theme_classic(base_size = 20) +
        xlab("Proportional size distribution categories") +
        theme(legend.position = "bottom",
              legend.title = element_blank(), 
              axis.title.y = ggtext::element_markdown())
    }    
    
    print(fig)
    
  })
  
  callModule(plotDownload, "RW_plot", plotRelativeWeight)
  
  output$rw_n_caption_view <- renderUI({
    temp <- filtered2() %>% 
      filter(metric == "Relative Weight") %>% 
      filter(!is.na(N)) %>% 
      arrange(gcat)
    
    if (nrow(temp) == 0) return(NULL)
    
    stand_text <- temp %>% 
      mutate(txt = paste0(gcat, " (*N* = ", N, ")")) %>% 
      pull(txt) %>% 
      paste(collapse = ", ")
    
    HTML(markdown::markdownToHTML(text = paste0(
      "Sample sizes by proportional size distribution category &mdash; ", stand_text, "."
    ), fragment.only = TRUE))
  })
  
  
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
      temp <- temp %>% 
        mutate(area_num = as.numeric(factor(area)))
      
      fig <- ggplot(temp) +
        geom_rect(aes(xmin = mean - se, xmax = mean + se, 
                     ymin = area_num - 0.12, ymax = area_num + 0.12), 
                 fill = "grey60", alpha = 0.4) +
        geom_segment(aes(x = mean, xend = mean, 
                         y = area_num - 0.12, yend = area_num + 0.12), 
                     color = "grey30", linewidth = 1.5) +
        scale_x_continuous("CPUE", expand = expansion(mult = c(0.05, 0.1))) +
        scale_y_continuous(breaks = NULL) +
        theme_classic(base_size = 20) +
        labs(title = paste0("*N* = ", N)) +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(), 
              plot.title = ggtext::element_markdown(hjust = 1, face = "bold"))
    }
    
    print(fig)
    
  })
  
  callModule(plotDownload, "CPUE_plot", plotCPUE, width = 10, height = 3.5)
  
  mtext <- "
<br>

<i>Before formatting your input data, ensure that there is corresponding data in the standardized dataset for your species, method, and waterbody type of interest by going to the View tab and selecting your values</i>

<center><b>How to Format Input Data</b></center>

You can upload your own data to compare to the standardized data. This needs to be provided as a csv file that will have length and/or weight measurements with one row per observation (a single fish or multiple fish summed). You can download a template file with the correct column names with the DOWNLOAD button at the left. This app will calculate the three metrics of interest for each unique combination of area, species, collection method, type of water body, and year. The three metrics are: 

1. Catch per unit effort (CPUE)
2. Length frequency
3. Relative weight

<center><b>Column Details</b></center>

Below are the details of the required columns in the data. The order of the columns does not matter <b>BUT</b> the column names are <b>case sensitive and must be lower case</b>. Additionally, only upload data from <b>one</b> waterbody sampling effort at a time (under the column `waterbody_name`). There is an example dataset at the bottom of the page with simulated data that may be a helpful guide.

1. CPUE requires `effort` column
2. Length frequency requires `total_length` column 
3. Relative weight requires `weight` and `total_length` columns

Required columns in input dataframe: 

- **Location**: 
  - **Requires** `state` column
    - `state` is name of state where fish were collected, spelled out with first letter capitalized
    - Every row should be the same state (as it's for the same water body)
  - An `ecoregion` column can optionally be included, to compare your data to standardized data in that ecoregion
    - `ecoregion` is name of ecoregion where fish were collected, spelled out and capitalized correctly
    - You can also find your ecoregion by clicking on your location the map on the Explore tab. Ecoregions can also be determined from [EPA ecoregions](https://www.epa.gov/eco-research/ecoregions-north-america) level 1.  Correctly formatted ecoregion names are listed below. 
  - `waterbody_name` is the name of the water body

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
  - `year` is a four-digit numeric
- **Measurements**: 
  - `total_length` is fish total length (TL) in mm
  - `weight` is fish record weight (g). A column named `weight` is required, but the data is is *optional* and the rest of the column can be left blank
  - `effort` is specified in **Collection method**. A column named `effort` is required, but the data is is *optional* and the rest of the column can be left blank
- **Sampling unit**:
  - `transect_or_net_set` identifies which individual transect (for electrofishing) or net set (for netting methods) each fish record came from
      - Use a simple numeric or text ID (e.g., 1, 2, 3... or T1, T2, T3...) that is consistent for every fish caught within the same transect/net set
      - CPUE is now calculated separately for each transect/net set and then averaged, which lets the app report a mean CPUE with a standard error instead of a single pooled number
- **Collection method**: see the table below for details
  - `method` must exactly match one of the options in 'Method name'
  - `effort` is the effort for that **individual transect or net set** (not summed across the whole survey), report number of effort units
      - Examples of effort: 600 seconds for a single electrofishing transect, 1 net night for a single gill net set, 1 100-m drift for a single drifting trammel net haul
  - The 'gill_net_spring' method is for gill netting done between January and June (months 1 - 6), and 'gill_net_fall' is for between July and December (months 7 - 12)
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
  - `waterbody_type` must exactly match one of the following: *large_standing_waters*, *small_standing_waters*, *two_story_standing_waters*, *wadeable_streams*, *rivers*
- **Species**:
  - `common_name` must **exactly match** one of following species, as from [`FSA::PSDlit`](https://fishr-core-team.github.io/FSA/):

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
      HTML(markdown::markdownToHTML(file = tf)))
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
    mutate(data_source = case_when(data_source == 1 ~ "User upload", 
                                   data_source == 2 ~ "Standardized")) %>% 
    filter(metric == "Length Frequency")
  print(temp)
  
  stand_N <- temp %>% 
    filter(data_source == "Standardized") %>% 
    distinct(N) %>% 
    pull(N)
  
  stand_area <- temp %>% 
    filter(data_source == "Standardized") %>% 
    distinct(area) %>% 
    pull(area) %>% 
    first()
  
  stand_N_label <- paste0(wrap_br(stand_area), " average <br> length frequency <br> (*N* = ", stand_N, " datasets)")
  
  user_N <- temp %>% 
    filter(data_source == "User upload") %>% 
    distinct(N) %>% 
    pull(N)
  
  user_N_label <- paste0(wb_label(), " length <br> frequency <br> (*n* = ", user_N, " fish)")
  
  stand_only <- temp %>% 
    filter(data_source == "Standardized")
  
  if(nrow(stand_only) == 0){
    fig <- ggplot() +
      annotate("text", x = 1, y = 1, size = 8,
               label = "No standardized data \n corresponding to uploaded data") +
      theme_void()
    
  } else {
    fig <- ggplot(temp, aes(x = gcat, y = mean, fill = data_source)) +
      geom_bar(stat = "identity", position = position_dodge(preserve = "single"), 
               color = "black")  +
      geom_errorbar(aes(ymin = mean - se,
                        ymax = mean + se),
                    position = position_dodge(width = 0.9, preserve = "single"),
                    width = 0) +
      scale_y_continuous("Frequency (%)",
                         limits = c(0, 100),
                         expand = c(0, 0)) +
      scale_fill_manual("legend", values = c("white", "grey60"), 
                        name = "Data source", 
                        labels = c(stand_N_label, user_N_label)) +
      theme_classic(base_size = 20) +
      xlab("Proportional size distribution categories") +
      theme(legend.position = c(0.75, 0.8), 
            legend.text = ggtext::element_markdown(), 
            legend.key.spacing.y = unit(0.25, "cm"))
  }
  
  print(fig)
  
})

callModule(plotDownload, "LF_plot_UU", plotLengthFrequencyuser)

plotRelativeWeightuser <- reactive({
  
  temp <- uu_processed() %>% 
    bind_rows(filtered3(), .id = "id") %>% 
    rename(data_source = id) %>% 
    mutate(data_source = case_when(data_source == 1 ~ "User upload", 
                                   data_source == 2 ~ "Standardized")) %>% 
    filter(metric == "Relative Weight")
  
  stand_only <- temp %>% 
    filter(data_source == "Standardized")
  
  user_only <- temp %>% 
    filter(data_source == "User upload")
  
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
    
    fig <- ggplot(temp, aes(x = gcat)) +
      geom_point(aes(y = mean,
                     color = data_source),
                 size = 4, 
                 position = position_dodge(width = 0.05)) +
      geom_errorbar(aes(ymin = mean - se, 
                        ymax = mean + se, 
                        color = data_source),
                    width = 0, 
                    position = position_dodge(width = 0.05)) +
      scale_x_discrete(drop = FALSE) +
      ylim(50, 160) +
      geom_hline(yintercept = 100, linetype = "dashed") +
      labs(y = "Relative weight (<i>W<sub>r</sub></i>)") +
      scale_color_manual(name = "Data source", 
                         breaks = c("Standardized", "User upload"),
                         values = c("Standardized" = "black", "User upload" = "grey60"), 
                         labels = c("Standardized" = wrap_br(stand_area), "User upload" = wb_label())) +
      theme_classic(base_size = 20) +
      xlab("Proportional size distribution categories") +
      theme(legend.position = "inside", 
            legend.position.inside = c(0.8, 0.85), 
            legend.text = ggtext::element_markdown(),
            axis.title.y = ggtext::element_markdown()) 
    
  }
  
  print(fig)
  
})

callModule(plotDownload, "RW_plot_UU", plotRelativeWeightuser)

output$rw_caption <- renderUI({
  temp <- uu_processed() %>% 
    bind_rows(filtered3(), .id = "id") %>% 
    rename(data_source = id) %>% 
    mutate(data_source = case_when(data_source == 1 ~ "User upload", 
                                   data_source == 2 ~ "Standardized")) %>% 
    filter(metric == "Relative Weight")
  
  stand_area <- temp %>% 
    filter(data_source == "Standardized") %>% 
    distinct(area) %>% 
    pull(area) %>% 
    first()
  
  if (is.null(stand_area) || length(stand_area) == 0) stand_area <- "the selected area"
  
  HTML(markdown::markdownToHTML(text = paste0(
    "Relative weight by proportional size distribution categories. Points indicate means and lines indicate standard error. Note that these two means are calculated differently: the ", stand_area, 
    " value is the average of each contributing survey's own average relative weight in that category, while the ", wb_label(),
    " value is the average across the individual fish uploaded in that category."
  ), fragment.only = TRUE))
})

output$rw_n_caption <- renderUI({
  temp <- uu_processed() %>% 
    bind_rows(filtered3(), .id = "id") %>% 
    rename(data_source = id) %>% 
    mutate(data_source = case_when(data_source == 1 ~ "User upload", 
                                   data_source == 2 ~ "Standardized")) %>% 
    filter(metric == "Relative Weight") %>% 
    filter(!is.na(N)) %>% 
    arrange(gcat)
  
  if (nrow(temp) == 0) return(NULL)
  
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
  
  user_text <- temp %>% 
    filter(data_source == "User upload") %>% 
    mutate(txt = paste0(gcat, " (*n* = ", N, ")")) %>% 
    pull(txt) %>% 
    paste(collapse = ", ")
  
  HTML(markdown::markdownToHTML(text = paste0(
    "Sample sizes by proportional size distribution category &mdash; ", stand_area, ": ", stand_text, 
    ". ", wb_label(), ": ", user_text, "."
  ), fragment.only = TRUE))
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

build_cpue_user_plot <- function(label_size = 4, plot_width_in = 12.5) {
  # Text-width measurements below (text_width_in/safe_hjust) need a graphics
  # device open to report font metrics. For the on-screen render, Shiny's
  # own device is already active when this runs. But for the download,
  # ggsave() evaluates this whole function to get its plot argument BEFORE
  # it opens its own output device -- so without this, the measurements
  # would run against whatever device happens to be active on the server
  # (often none), which can report different font metrics than the actual
  # output device and throw off exactly the calculations meant to keep text
  # clear of the axis. Opening a fixed, consistent device here for the
  # measurements guarantees the same answer regardless of caller.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # All spacing below was hand-tuned for label_size = 4; scale it proportionally
  # so a bigger font (e.g. 5.5 on-screen) gets bigger gaps too, instead of the
  # same absolute spacing becoming too tight for larger text.
  s <- label_size / 4

  temp <- uu_processed() %>%
    bind_rows(filtered3(), .id = "id") %>%
    rename(data_source = id) %>%
    mutate(data_source = case_when(data_source == 1 ~ "User upload",
                                   data_source == 2 ~ "Standardized")) %>%
    filter(metric == "CPUE")

  stand_only <- temp %>%
    filter(data_source == "Standardized")

  user_only <- temp %>%
    filter(data_source == "User upload")

  if (nrow(user_only) == 0){

    fig <- ggplot() +
      annotate("text", x = 1, y = 1, size = 8,
               label = "The uploaded data included no efforts. \n Please include efforts to generate this plot") +
      theme_void()

  } else if(nrow(stand_only) == 0){

    fig <- ggplot() +
      annotate("text", x = 1, y = 1, size = 8,
               label = "No standardized data \n corresponding to uploaded data") +
      theme_void()

  } else {

    unit_label <- cpue_unit_label(temp$method[1], temp$waterbody_type[1])

    stand_band <- temp %>%
      filter(data_source == "Standardized") %>%
      slice(1)

    # Two user rows share data_source == "User upload", split explicitly by
    # cpue_calc rather than assuming one row per data_source.
    user_transect <- temp %>%
      filter(data_source == "User upload", cpue_calc == "transect")

    user_total <- temp %>%
      filter(data_source == "User upload", cpue_calc == "total")

    # SE is undefined (NA) with only one transect/net-set -- sd() of a
    # single value has no meaning -- so it's treated as zero (no visible
    # error bar) here rather than left NA, which would otherwise turn the
    # entire x-range NA and blank out every label position in the plot.
    stand_se_safe <- coalesce(stand_band$se, 0)
    transect_se_safe <- coalesce(user_transect$se, 0)

    # Four lines -- Mean (± SE) and Median (± IQR), once for the standardized
    # area and once for the user's own per-transect data -- plus the pooled
    # total on its own line below. Ranking a mean against a percentile-built
    # reference and a median against the same reference mixes two different
    # kinds of statistics, so each gets its own line with its own matching
    # error measure instead of being overlaid on one row: SE for a mean, IQR
    # (25th-75th percentile) for a median, since a median's spread isn't a
    # standard error. The median line is always shown, even with only 1-2
    # transects/datasets where it's mathematically identical to the mean --
    # median_label_text() flags that case with "(N=...)" rather than hiding
    # the line outright.
    # Each median's percentile rank is shown right below its value: exactly
    # "50th percentile" for the area (true by definition of a median, not a
    # calculation), and an estimated rank for the waterbody -- where its
    # median falls within the area's reference distribution, interpolated
    # from the same 5 known percentile points used elsewhere.
    transect_pct_rank <- estimate_pct_rank(user_transect$`50%`, stand_band)

    rows <- bind_rows(
      stand_band %>% transmute(row_label = paste0(area, " — Mean"),
                               center = mean, lo = mean - stand_se_safe, hi = mean + stand_se_safe,
                               value_label = mean_label_text(mean, se, N, unit_label), pct_label = NA_character_,
                               color = "steelblue4"),
      stand_band %>% transmute(row_label = paste0(area, " — Median"),
                               center = `50%`, lo = `25%`, hi = `75%`,
                               value_label = median_label_text(`50%`, `25%`, `75%`, N, unit_label),
                               pct_label = "50th percentile", color = "steelblue4"),
      user_transect %>% transmute(row_label = paste0(wb_label(), " — Per-Transect Mean"),
                                  center = mean, lo = mean - transect_se_safe, hi = mean + transect_se_safe,
                                  value_label = mean_label_text(mean, se, N, unit_label), pct_label = NA_character_,
                                  color = "grey30"),
      user_transect %>% transmute(row_label = paste0(wb_label(), " — Median"),
                                  center = `50%`, lo = `25%`, hi = `75%`,
                                  value_label = median_label_text(`50%`, `25%`, `75%`, N, unit_label),
                                  pct_label = paste0("~", transect_pct_rank, ordinal_suffix(transect_pct_rank), " percentile"),
                                  color = "grey30")
    )
    # Topmost row first, working down to y = 1; the pooled total always
    # anchors the bottom at y = 0, kept separate since it has no mean/median
    # split of its own -- a single ratio isn't a distribution.
    rows$y <- rev(seq_len(nrow(rows)))

    total_row <- user_total %>%
      transmute(row_label = paste0(wb_label(), " — ", cpue_method),
               center = mean, value_label = paste0(round(mean, 1), " ", unit_label), y = 0)

    axis_labels <- bind_rows(rows %>% select(y, row_label), total_row %>% select(y, row_label)) %>%
      mutate(row_label = str_wrap(row_label, width = 28))

    # Full x-range must include every error bar and the total's point, not
    # just the central estimates, or this would underestimate the true
    # data-units-per-inch and under-space the value labels.
    full_x_min <- min(rows$lo, total_row$center)
    full_x_max <- max(rows$hi, total_row$center)
    x_range <- full_x_max - full_x_min
    x_expand <- c(0.08, 0.1)
    panel_width_units <- x_range * (1 + sum(x_expand))

    # The y-axis category labels are now real axis text (not text drawn
    # inside the panel, as in the previous design), so they claim their own
    # share of the physical canvas -- a long area or waterbody name can take
    # a couple of inches. Without subtracting that out, data_units_per_inch
    # would assume more panel width than actually exists once axis text is
    # laid out, letting a value label overflow the left edge undetected.
    axis_label_width_in <- max(text_width_in(axis_labels$row_label, label_size))
    available_panel_in <- plot_width_in - axis_label_width_in - 0.4
    data_units_per_inch <- panel_width_units / available_panel_in
    left_boundary <- full_x_min - x_expand[1] * x_range
    right_boundary <- full_x_max + x_expand[2] * x_range

    # Value labels stay centered above their point unless that would cross
    # the y-axis (e.g. a very low CPUE for a rarely-caught species) or the
    # panel's right edge (e.g. a high CPUE, or a long "(N=...)"-flagged
    # label), in which case they're left- or right-aligned instead so the
    # text extends away from whichever edge it was about to cross. Each
    # row's label is independent now -- with one line per statistic instead
    # of several stacked on one row, there's no longer a matching label at
    # the same x that needs to share the decision.
    rows <- rows %>%
      mutate(hjust = safe_hjust(center, value_label, label_size, left_boundary, right_boundary, data_units_per_inch, fontface = "bold"))
    total_row <- total_row %>%
      mutate(hjust = safe_hjust(center, value_label, label_size, left_boundary, right_boundary, data_units_per_inch, fontface = "bold"))

    # Percentile-rank text sits just below each median's point (the value
    # label is above it), in its own italic style so it reads as a
    # secondary, approximate note rather than a measured value like the
    # median itself.
    pct_rows <- rows %>%
      filter(!is.na(pct_label)) %>%
      mutate(pct_hjust = safe_hjust(center, pct_label, label_size, left_boundary, right_boundary, data_units_per_inch, fontface = "italic"))

    fig <- ggplot() +
      geom_errorbar(data = rows, aes(xmin = lo, xmax = hi, y = y, color = I(color)),
                    width = 0.15, linewidth = 0.7, orientation = "y") +
      geom_point(data = rows, aes(x = center, y = y, color = I(color)), size = 3) +
      geom_text(data = rows, aes(x = center, y = y + 0.28, label = value_label, hjust = hjust, color = I(color)),
                size = label_size, fontface = "bold") +
      geom_text(data = pct_rows, aes(x = center, y = y - 0.22, label = pct_label, hjust = pct_hjust, color = I(color)),
                size = label_size * 0.85, fontface = "italic") +
      # Pooled total: no error bar (single value, not a distribution) --
      # diamond marker instead of the round point used for mean/median.
      geom_point(data = total_row, aes(x = center, y = y), shape = 18, size = 4, color = "black") +
      geom_text(data = total_row, aes(x = center, y = y + 0.28, label = value_label, hjust = hjust),
                size = label_size, fontface = "bold", color = "black") +
      scale_x_continuous(paste0("CPUE (", unit_label, ")"), expand = expansion(mult = x_expand)) +
      scale_y_continuous(breaks = axis_labels$y, labels = axis_labels$row_label,
                         expand = expansion(mult = c(0.18, 0.35))) +
      theme_classic(base_size = label_size * 4) +
      theme(axis.title.y = element_blank())

  }

  # Returned rather than printed here: this function opens its own pdf(NULL)
  # device above for text measurements, and printing while that device is
  # still current would draw into it instead of whatever device the caller
  # actually wants the plot on (Shiny's on-screen device, or ggsave's own
  # output device for the download) -- both callers already handle
  # printing/saving the returned object themselves.
  fig
}

# The on-screen render adapts its internal label math to the plot's ACTUAL
# rendered width (queried from the browser via clientData) instead of
# assuming a fixed 900px/12.5in canvas. When the real on-screen plot area
# ends up narrower than that -- a smaller browser window, more room taken by
# the sidebar, etc. -- labels sized for the wider assumed canvas would run
# past the true edges and get clipped by the y-axis, even though the same
# math is fine at the download's fixed, guaranteed-12.5in size.
output[["CPUE_plot_UU-plot"]] <- renderPlot({
  width_px <- session$clientData[["output_CPUE_plot_UU-plot_width"]]
  width_in <- if (!is.null(width_px) && width_px > 0) width_px / 72 else 12.5
  # Font size scales with the actual rendered width, holding the
  # text-to-canvas ratio roughly constant with the download's 12.5in/5.5pt
  # reference (which is known to render safely). Without this, a narrower
  # on-screen container keeps the same absolute font size while having less
  # physical room for it, so text claims a disproportionately larger share
  # of the canvas the narrower the window gets -- no amount of
  # alignment-flipping alone can fully compensate for that. Clamped so a
  # very narrow or very wide window doesn't make text illegibly small or
  # oddly huge.
  scaled_label_size <- pmin(pmax(5.5 * width_in / 12.5, 4), 6.5)
  build_cpue_user_plot(label_size = scaled_label_size, plot_width_in = width_in)
})

# Download always renders at a fixed 12.5 x 5.56in (900x400px @ 72dpi) so it
# looks the same regardless of the uploader's screen/window size.
output[["CPUE_plot_UU-download_plot"]] <- downloadHandler(
  filename = function() "plot.jpg",
  content = function(file) {
    ggsave(file, build_cpue_user_plot(label_size = 5.5, plot_width_in = 12.5), width = 12.5, height = 5.56)
  }
)

output$cpue_caption <- renderUI({
  temp <- uu_processed() %>% 
    bind_rows(filtered3(), .id = "id") %>% 
    rename(data_source = id) %>% 
    mutate(data_source = case_when(data_source == 1 ~ "User upload", 
                                   data_source == 2 ~ "Standardized")) %>% 
    filter(metric == "CPUE")
  
  stand_area <- temp %>% 
    filter(data_source == "Standardized") %>% 
    distinct(area) %>% 
    pull(area) %>% 
    first()
  
  if (is.null(stand_area) || length(stand_area) == 0) stand_area <- "the selected area"
  
  HTML(markdown::markdownToHTML(text = paste0(
    "Catch per unit effort, with units shown on the axis and next to each value (units depend on collection method — see below). Each line shows one statistic: **'", stand_area, " — Mean'** shows the mean CPUE across all standardized datasets in that area, +/- 1 standard error (SE). **'", stand_area,
    " — Median'** shows the median (50th percentile) CPUE across those same datasets, with its interquartile range (IQR, the 25th-75th percentile spread) shown as the error bar -- a median's natural measure of spread, not a standard error. **'", wb_label(),
    " — Per-Transect Mean'** shows your survey's mean CPUE calculated per transect or net set (the average of all the individual per-transect/net-set CPUEs in your survey), +/- 1 standard error. **'", wb_label(),
    " — Median'** shows the median of those same per-transect/net-set CPUEs, with its interquartile range shown as the error bar. With only 1 or 2 transects/net sets, the median is mathematically identical to the mean (the middle of one point is itself; the middle of two is their average) -- still shown, but labeled with '(N=1)' or '(N=2)' as a reminder that it isn't an independent second estimate. **'", wb_label(),
    " — Total Catch of [species]/Total Effort'** shows total fish caught divided by total effort, summed across all transects/net sets rather than averaged. This single ratio has no mean, median, or error bar, because it isn't a distribution of per-transect values -- it's one number. CPUE units depend on collection method: boat and raft electrofishing are fish per hour, gill net is fish per net nights, drifting trammel net is fish per 100-m drift, large catfish hoopnet is fish per 24 hour set, bag seine is fish per 0.25 arc (small standing waters) or 0.5 arc (rivers), stream seine is fish per 10-15m haul, and backpack and tow barge electrofishing are fish per 100m²."
  ), fragment.only = TRUE))
})

}

shinyApp(ui, server)
