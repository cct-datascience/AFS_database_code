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
plotDownloadUI <- function(id, height = 400) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        2, offset = 10,
        downloadButton(ns("download_plot"), "Download plot")
      )
    ),
    fluidRow(
      plotOutput(ns('plot'), height = height)
    )
    
  )
}

plotDownload <- function(input, output, session, plotFun, width = 9, height = 6) {
  output$plot <- renderPlot({
    plotFun()
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      "plot.png"
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


#Shiny App
ui <- navbarPage(title = "AFS Standard Sampling App", tags$a(style='background-color:white;position:absolute;right:10px;top:15px;',tags$img(src='AFS_logo.png'), href = "https://fisheries.org/"), 
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
                                         plotDownloadUI("CPUE_plot_UU", height = "200px"), 
                                         br(), 
                                         div(style = "font-size:25px;", uiOutput("cpue_caption")))
                              )
                            )
                            
                          ), 
                          
                 )
                 
)




server <- function(input, output) {
  
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
    uu <- read_csv(input$upload$datapath)
    
    uu_state <- uu %>% select(-any_of("ecoregion")) %>% mutate(type = "state") %>% rename(area = state)
    uu_all   <- uu %>% select(-state, -any_of("ecoregion")) %>% mutate(type = "all", area = "North America")
    uu_all   <- bind_rows(uu_all, uu_state)
    
    if ("ecoregion" %in% names(uu)) {
      uu_all <- bind_rows(uu_all, uu %>% select(-state) %>% mutate(type = "ecoregion") %>% rename(area = ecoregion))
    }
    uu_all
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
    uu <- read_csv(input$upload$datapath)
    
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
    uu_counts <- calculate_total_n(uu_all)
    uu_cpue   <- calculate_cpue(uu_all)
    uu_lf     <- calculate_lf(uu_all)
    uu_rw     <- calculate_rw(uu_all)
    
    bind_rows(uu_cpue, uu_lf, uu_rw) %>%
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
      select(area, common_name, method, waterbody_type, gcat, metric, N, mean,
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
        "</em>, <em>n</em> is the number of transects or net sets averaged together.</li>"))
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
                                     data_source == 2 ~ area)) %>%
      arrange(metric) %>%
      select(metric, data_source, common_name, method, waterbody_type, gcat, N, mean, 
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

plotCPUEuser <- reactive({
  temp <- uu_processed() %>% 
    bind_rows(filtered3(), .id = "id") %>% 
    rename(data_source = id) %>% 
    mutate(data_source = case_when(data_source == 1 ~ "User upload", 
                                   data_source == 2 ~ "Standardized")) %>% 
    filter(metric == "CPUE")
  print("Check")
  print(temp, n = Inf)
  
  stand_N <- temp %>% 
    filter(data_source == "Standardized") %>% 
    distinct(N) %>% 
    pull(N)
  
  stand_N_label <- paste0("Standardized data <br> (*N* = ", stand_N, " datasets)")
  
  user_N_label <- paste0(wb_label())
  
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
    
    temp <- temp %>% 
      mutate(area_num = as.numeric(factor(area)), 
             y_pos = case_when(data_source == "Standardized" ~ area_num - 0.3, 
                               data_source == "User upload" ~ area_num + 0.3))
    
    stand_ticks <- temp %>% 
      filter(data_source == "Standardized") %>% 
      group_by(y_pos) %>% 
      slice(1) %>% 
      ungroup() %>% 
      select(y_pos, area, `5%`, `25%`, `50%`, `75%`, `95%`) %>% 
      pivot_longer(cols = c(`5%`, `25%`, `50%`, `75%`, `95%`), 
                   names_to = "percentile", values_to = "x") %>% 
      mutate(is_median = percentile == "50%", 
             label_offset = case_when(percentile %in% c("5%", "50%", "95%") ~ 0.2, 
                                      percentile %in% c("25%", "75%") ~ 0.27), 
             label_hjust = case_when(percentile == "5%"  ~ 0, 
                                     percentile == "95%" ~ 1, 
                                     TRUE                ~ 0.5))
    
    stand_band <- temp %>% 
      filter(data_source == "Standardized") %>% 
      group_by(y_pos) %>% 
      slice(1) %>% 
      ungroup()
    
    user_band <- temp %>% 
      filter(data_source == "User upload") %>% 
      group_by(y_pos) %>% 
      slice(1) %>% 
      ungroup()
    
    # Estimate the user's approximate percentile rank by linear interpolation 
    # between the 5 known standardized percentile points (5/25/50/75/95th).
    # This is an approximation, not an exact percentile, since we only have 
    # 5 reference points rather than the full underlying distribution.
    known_p <- c(5, 25, 50, 75, 95)
    
    ordinal_suffix <- function(n) {
      if (n %% 100 %in% 11:13) return("th")
      switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
    }
    
    estimate_pct_rank <- function(user_mean, stand_row) {
      known_x <- c(stand_row$`5%`, stand_row$`25%`, stand_row$`50%`, stand_row$`75%`, stand_row$`95%`)
      if (user_mean <= known_x[1]) {
        max(1, round(5 * user_mean / known_x[1]))
      } else if (user_mean >= known_x[5]) {
        min(99, round(95 + (user_mean - known_x[5]) / (known_x[5] - known_x[4]) * 4))
      } else {
        round(approx(x = known_x, y = known_p, xout = user_mean)$y)
      }
    }
    
    user_band <- user_band %>% 
      left_join(stand_band %>% select(area, `5%`, `25%`, `50%`, `75%`, `95%`), 
                by = "area", suffix = c("", "_std")) %>% 
      rowwise() %>% 
      mutate(pct_rank = estimate_pct_rank(mean, 
                                          tibble(`5%` = `5%_std`, `25%` = `25%_std`, 
                                                `50%` = `50%_std`, `75%` = `75%_std`, `95%` = `95%_std`))) %>% 
      ungroup()
    
    fig <- ggplot() +
      geom_segment(data = stand_ticks, 
                   aes(x = x, xend = x, 
                       y = y_pos - ifelse(is_median, 0.16, 0.1), 
                       yend = y_pos + ifelse(is_median, 0.16, 0.1), 
                       linewidth = is_median), 
                   color = "grey50") +
      geom_text(data = stand_ticks, 
                aes(x = x, y = y_pos + label_offset, label = percentile, hjust = label_hjust), 
                size = 4, color = "grey50") +
      geom_rect(data = stand_band, 
               aes(xmin = mean - se, xmax = mean + se, 
                   ymin = y_pos - 0.06, ymax = y_pos + 0.06), 
               fill = "steelblue", alpha = 0.5) +
      geom_segment(data = stand_band, 
                   aes(x = mean, xend = mean, 
                       y = y_pos - 0.06, yend = y_pos + 0.06), 
                   color = "steelblue4", linewidth = 1.2) +
      geom_rect(data = user_band, 
               aes(xmin = mean - se, xmax = mean + se, 
                   ymin = y_pos - 0.12, ymax = y_pos + 0.12), 
               fill = "grey30", alpha = 0.4) +
      geom_segment(data = user_band, 
                   aes(x = mean, xend = mean, 
                       y = y_pos - 0.12, yend = y_pos + 0.12), 
                   color = "grey30", linewidth = 1.5) +
      geom_text(data = user_band, 
                aes(x = mean, y = y_pos - 0.2, 
                    label = paste0("~", pct_rank, sapply(pct_rank, ordinal_suffix), " percentile"),
                    hjust = ifelse(pct_rank > 50, 1, 0)), 
                size = 4, color = "grey30", fontface = "italic") +
      geom_text(data = stand_ticks %>% distinct(y_pos, area), 
                aes(x = Inf, y = y_pos, label = str_wrap(area, width = 20)), 
                hjust = 1, size = 4, color = "grey50", fontface = "bold", lineheight = 0.9) +
      geom_text(data = user_band, 
                aes(x = Inf, y = y_pos, label = str_wrap(wb_label(), width = 20)), 
                hjust = 1, size = 4, color = "grey30", fontface = "bold", lineheight = 0.9) +
      scale_x_continuous("CPUE", expand = expansion(mult = c(0.05, 0.55))) +
      scale_y_continuous(breaks = NULL, expand = expansion(mult = c(0.15, 0.25))) +
      scale_linewidth_manual(values = c("FALSE" = 0.6, "TRUE" = 1.3), guide = "none") +
      theme_classic(base_size = 20) +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
    
  }
  
  print(fig)
  
})

callModule(plotDownload, "CPUE_plot_UU", plotCPUEuser, width = 10, height = 3.5)

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
    "Catch per unit effort. The grey ticks above the ", stand_area, 
    " row mark the 5th, 25th, 50th (median, bold tick), 75th, and 95th percentiles of CPUE across many sampled datasets in ", stand_area, 
    ", so you can see roughly where ", wb_label(), "'s CPUE falls within that regional distribution. The blue line and band on that same row show the ", stand_area, 
    " mean CPUE +/- 1 standard error. The dark vertical line and shaded band on the ", wb_label(), 
    " row show the survey mean CPUE +/- 1 standard error, averaged across transects/net sets. The italic label below the ", wb_label(),
    " line gives an approximate percentile rank (estimated by interpolating between the 5th/25th/50th/75th/95th percentile points above) showing roughly where ", wb_label(),
    "'s mean CPUE falls within the ", stand_area, " distribution. CPUE units depend on collection method: boat and raft electrofishing are fish per hour, gill net is fish per net nights, drifting trammel net is fish per 100-m drift, large catfish hoopnet is fish per 24 hour set, bag seine is fish per 0.25 arc (small standing waters) or 0.5 arc (rivers), stream seine is fish per 10-15m haul, and backpack and tow barge electrofishing are fish per 100m²."
  ), fragment.only = TRUE))
})

}

shinyApp(ui, server)
