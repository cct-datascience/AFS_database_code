library(shiny)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(leaflet)
library(DT)
library(leafpop)
library(FSA)
library(knitr)
# library(periscope)

# Module code
plotDownloadUI <- function(id, height = 400) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        2, offset = 10,
        downloadButton(ns("download_plot"), "Download")
      )
    ),
    fluidRow(
      plotOutput(ns('plot'), height = height)
    )
    
  )
}

plotDownload <- function(input, output, session, plotFun) {
  output$plot <- renderPlot({
    plotFun()
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      "plot.png"
    },
    content = function(file) {
      ggsave(file, plotFun(), width = 6, height = 4)
    }
  )
}

# Read in summary fish metrics
metrics <- read_csv('standardized_fish_data.csv') %>%
  relocate(area) %>%
  relocate(N, .after = last_col()) %>%
  mutate(gcat = case_when(gcat == "Stock-Quality" ~ "S-Q",
                          gcat == "Quality-Preferred" ~ "Q-P",
                          gcat == "Preferred-Memorable" ~ "P-M",
                          gcat == "Memorable-Trophy" ~ "M-T",
                          gcat == "Trophy" ~ "T") %>%
           factor(levels = c("S-Q", "Q-P", "P-M", "M-T", "T"))) %>% 
  mutate(method = str_replace_all(method, " ", "_"), 
         waterbody_type = str_replace_all(waterbody_type, " ", "_"))
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
locs <- read_csv("sites.csv") %>%
  select(-date) # not parseable as is

# Read in example user upload data
ex <- read_csv("example_user_upload_data.csv")

# Create a "not in" function
`%nin%` <- negate(`%in%`)

#Shiny App
ui <- navbarPage("AFS Standard Sampling App",
                 theme = bslib::bs_theme(bootswatch = "sandstone"),
                 tabPanel("About",
                          wellPanel(
                            tags$head(tags$style(
                              type="text/css",
                              "#pics img {display: block; margin-left: auto; margin-right: auto; max-height: 90%; height: 90%; max-width: 100%, width: auto}"
                            )),
                            imageOutput("pics"),
                            h2("About Standard Sampling", align = "center"),
                            h4("Standardization of sampling methods allows fisheries professionals to compare data across large spatial and temporal scales, encourages data sharing and improves communication. 
In light of these benefits, the American Fisheries Society published the first edition of Standard Methods for Sampling North American Freshwater Fishes in 2009. 
A new edition of these methods will be published Winter 2023-2024. 
The goals of this project were to:"),
br(),
h4("(1) recommend standardized freshwater fish sampling methods for North America, and"),
h4("(2) provide an online database of fish data collected using AFS recommended standard methods where data from individual water bodies could be compared to existing rangewide, ecoregion, and state standards."),
br(),
h4("Since publication, numerous fisheries professionals have adopted the standard methods and many have noted the database as an important tool in management. 
The database allows for comparison of fish metrics commonly used in management to assess population health including growth, condition, length-frequency, and catch per unit effort data collected using standard methods.
When developing version two of the database, we examined the strengths and weaknesses of methods used for requesting, analyzing, and displaying data in an online format.
We used this information to achieve our goals of maximizing use and providing simple means to update this program for comparison of fisheries data."),
br(),
h4("We hope that these methods can be adopted by others, particularly those in data-poor regions, to maximize the ability to compare data. "),
h2("Details About App Usage", align = "center"), 
h4("Here, you can compare your data from an individual water body, collected using a standard AFS method to averages across the range of the species, ecoregion, and statewide.  This tool will help you assess if fish you have collected are high, average or low for the metric in question."),
h4("We collected thousands of data sets on fishes from across Canada, the United States, and some from Mexico to build comparison summaries. A ", strong("data set (N)"), " is defined as data collected with AFS standard gears and methods during routine monitoring programs of an entire fish community or entire populations of specific fish ", strong("in a single waterbody conducted once during a year"), ". For example, data collected from Bass Lake (a small standing water body) sampled by boat electrofishing for Largemouth Bass in 2014 would represent 1 data set. In some cases, data sets included multi-day surveys of the same waterbody (e.g., large reservoirs) where the effort was summed across all sampling days. This excludes surveys targeting specific size groups or those with other biases (e.g., egg counts, juveniles fish surveys)."), 
h4("Most of the app components can be translated into another language by going to ", 
   a("Google Translate", href="https://translate.google.com/?sl=auto&tl=en&op=websites", .noWS = "outside"), 
   " and entering the URL for the app, then selecting the desired language. Maps and plot can only currently be generated using the original version of the app. "), 
h2("How to Provide Feedback", align = "center"), 
h4("These data sets were collected and compiled by ", 
   a("Scott Bonar's lab", href="https://azcfwru.wixsite.com/azcfwru", .noWS = "outside"), 
   ". Only summaries can be shared. Individual data sets cannot be shared with others because of legal restrictions on their use. All feedback on this app is greatly appreciated, and can be provided by sending an email to ", 
   a("Dr. Bonar", href="mailto:SBonar@ag.arizona.edu", .noWS = "outside"), 
   " or by submitting an ", 
   a("issue", href="https://github.com/cct-datascience/AFS_database_code/issues", .noWS = "outside"), 
   " through the project's ", 
   a("GitHub repository", href="https://github.com/cct-datascience/AFS_database_code", .noWS = "outside"), 
   ", which is also where the code is located."), 
h4 ("Sponsored by:"), 
imageOutput("logo")
                          )
                 ),

                 tabPanel(title = "Explore",
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
                                          selected = 'Bluegill'),
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
                                tabPanel("Map", 
                                         leafletOutput("plotSites",
                                                       width = 700,
                                                       height = 500),
                                         textOutput("report_absent1"),
                                         textOutput("report_absent2")), 
                                tabPanel("Preview", 
                                         DTOutput("filtertable"))
                                )
                              )
                          )
                          ), 

                 tabPanel(title = "View",
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
                                   selected ='Bluegill'),
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
                               plotDownloadUI("RW_plot"),
                               plotDownloadUI("CPUE_plot", height = "200px")
                               )
                     )
                   ),
                tabPanel(title = "Compare Your Data",
                         sidebarLayout(
                           sidebarPanel(
                             "Welcome to the American Fisheries Society Standard Sampling Database App!",
                             br(),
                             br(),
                             "Upload your fish data for comparison here: ", 
                             fileInput("upload", NULL,
                                       buttonLabel =  "upload",
                                       multiple = FALSE, 
                                       accept = (".csv"), 
                                       placeholder = ""), 
                             "To view plots of standard fish data, select one option from each menu below.",
                             uiOutput("dyn_type"),
                             uiOutput("dyn_area3"),
                             uiOutput("dyn_spp3"),
                             uiOutput("dyn_method3"),
                             uiOutput("dyn_watertype3")
                           ),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Instructions", 
                                        uiOutput("instructions"), 
                                        DTOutput("example")),
                               tabPanel("Comparisons", 
                                        plotDownloadUI("LF_plot_UU"),
                                        plotDownloadUI("RW_plot_UU"),
                                        plotDownloadUI("CPUE_plot_UU", height = "200px"))
                             )
                           )
                          
                         ), 

                 ) 

)




server <- function(input, output) {
  
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
      src = file.path("www/AFS_sponsor_3.png"), 
      contentType = "image/png", 
      height = 250, 
      width = 150
    )
  }, deleteFile = FALSE)
  
  # Render a UI for selecting area depending on North America, Ecoregions, or State/Province
  output$dyn_area <- renderUI({
    if(input$typechoice == "North America") {
      temp <- "North America"
    } else if(input$typechoice == "Ecoregion") {
      temp <- uni.area[1:12]
    } else if(input$typechoice == "State/Province") {
      temp2 <-  uni.area[uni.area %nin% 'North America']
      temp <- temp2[-1:-12]
    }
    
    selectInput(inputId = "areachoice",
                label = "Select area:", 
                choices = temp,
                selected = temp[1],
                multiple = TRUE)
  })
  
  # Render a UI for selecting area depending on North America, Ecoregions, or State/Province for tab2
  output$dyn_area2 <- renderUI({
    if(input$typechoice2 == "North America") {
      temp <- "North America"
    } else if(input$typechoice2 == "Ecoregion") {
      temp <- uni.area[1:12]
    } else if(input$typechoice2 == "State/Province") {
      temp2 <-  uni.area[uni.area %nin% 'North America']
      temp <- temp2[-1:-12]
    }
    
    selectInput(inputId = "areachoice2",
                label = "Select area:", 
                choices = temp,
                selected = temp[1])
  })

  # Read in raw user upload data to be used in reactive UI's
  uu_raw <- reactive({
    inFile <- input$upload
    uu <- read_csv(inFile$datapath)
    
  })
  
  # Render a UI for selecting area 
  output$dyn_type <- renderUI({
    req(input$upload)
    
    temp <-  uu_raw() %>% # 
      select(type) %>%
      unique() %>%
      mutate(types = case_when(type == "all" ~ "North America",
                               type == "ecoregion" ~ "Ecoregion",
                               type == "state" ~ "State/Province")) %>%
      arrange(types) %>%
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
        select(area) %>%
        unique() %>%
        pull()
    } else if(input$typechoice3 == "State/Province") {
      temp <- uu_raw() %>%
        filter(type == "state") %>%
        select(area) %>%
        unique() %>%
        pull()
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
      select(common_name) %>%
      unique() %>%
      pull()
    
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
      select(method) %>%
      unique() %>%
      pull()
    
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
      select(waterbody_type) %>%
      unique() %>%
      pull()
    
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
  
  # For downloading the entire dataset for tab 1 explore
  all <- reactive({
    
    all_df <-  metrics %>%
      arrange(common_name) %>%
      select(area, common_name, method, waterbody_type, gcat, metric, N, mean, 
             se, `5%`, `25%`, `50%`, `75%`, `95%`) 
  })
  
  output$alldownload <- downloadHandler(
    filename = function() {
      paste0("AFS-all-", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      write.csv(all(), file)
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
    print(f_df)
    
  })

  observeEvent(input$areachoice2, {
    print(paste0("You have chosen: ", input$areachoice2))
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
                    "Gabelhouse" = gcat,
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
  
  uu_processed <- reactive({
    # Suppresses error messages for plots until inputs are chosen
    req(input$areachoice3)
    req(input$sppchoice3)
    req(input$methodchoice3)
    req(input$watertypechoice3)
    
    inFile <- input$upload
    uu <- read_csv(inFile$datapath)
    print("UU original")
    print(uu)
    
    # Calculate 3 metrics for user data
    uu_counts <- uu %>% 
      group_by(type, area, common_name, method, waterbody_type) %>%
      summarise(N = n())
    
    uu_cpue <- calculate_cpue(uu)
    uu_lf <- calculate_lf(uu)
    uu_rw <- calculate_rw(uu)
    
    uu_process <- bind_rows(uu_cpue, uu_lf, uu_rw) %>% 
      left_join(uu_counts, by = c("type", "area", "common_name", "method", "waterbody_type")) %>% 
      mutate(gcat = case_when(gcat == "stock" ~ "S-Q",
                              gcat == "quality" ~ "Q-P",
                              gcat == "preferred" ~ "P-M",
                              gcat == "memorable" ~ "M-T",
                              gcat == "trophy" ~ "T") %>%
               factor(levels = c("S-Q", "Q-P", "P-M", "M-T", "T")))

    f_df <-  uu_process %>%
      filter(metric %in% uni.metric,
             area %in% input$areachoice3,
             common_name %in% input$sppchoice3,
             method %in% input$methodchoice3,
             waterbody_type %in% input$watertypechoice3) %>%
      arrange(common_name) %>%
      select(area, common_name, method, waterbody_type, gcat, metric, N, mean,
             se, `5%`, `25%`, `50%`, `75%`, `95%`)

    print("UU filtered")
    print(f_df$metric)
    print(f_df, n = Inf)

  })
  
  # Map of ecoregions and sites
  output$plotSites <- renderLeaflet({
    
    # Remove 'exclude()' from plot_data if only 1 site or 1 year is represented
    plot_data <- locate() %>%
      anti_join(exclude())
    
    # print(plot_data)
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
      fig <- ggplot(temp, aes(x = gcat, y = mean, fill = "#F8766D")) +
        geom_bar(stat = "identity") +
        geom_errorbar(aes(ymin = mean - se,
                          ymax = mean + se),
                      width = 0) +
        scale_y_continuous("Frequency (%)",
                           limits = c(0, 100),
                           expand = c(0, 0)) +
        theme_classic(base_size = 16) +
        xlab("Gabelhouse length")
        theme(legend.position = "none") +
        ggtitle(paste0("N = ", N))
    }
    
    print(fig)
    
  })
  
  callModule(plotDownload, "LF_plot", plotLengthFrequency)
  
  plotRelativeWeight <- reactive({
    
    temp <- filtered2() %>%
      filter(metric == "Relative Weight")
      
    ypos <- min(temp$mean - temp$se,  na.rm = TRUE) - 2

    if(nrow(temp) == 0){
      fig <- ggplot() +
        annotate("text", x = 1, y = 1, size = 8,
                 label = "No relative weight data for selected options") +
        theme_void()
    } else {
      fig <- ggplot(temp, aes(x = gcat)) +
        geom_point(aes(y = mean),
                   size = 2.5) +
        geom_text(aes(y = ypos,
                      label = N),
                  vjust = 0.5) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                      width = 0) +
        scale_y_continuous("Relative weight") +
        theme_classic(base_size = 16) +
        xlab("Gabelhouse length") +
        theme(legend.position = "bottom",
              legend.title = element_blank())  
    }    

    print(fig)
    
  })
  
  callModule(plotDownload, "RW_plot", plotRelativeWeight)
  
  
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
      fig <- ggplot(temp, aes(y = area)) +
        geom_boxplot(aes(xmin = `5%`,
                         xlower = `25%`,
                         xmiddle = `50%`,
                         xupper = `75%`,
                         xmax = `95%`),
                     stat = "identity") +
        scale_x_continuous("CPUE (fish / hour)") +
        theme_classic(base_size = 16) +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) +
        ggtitle(paste0("N = ", N))  
    }

    print(fig)
    
  })
  
  callModule(plotDownload, "CPUE_plot", plotCPUE)
  
  mtext <- "
<br>
  
<center><b>How to Format Input Data</b></center>

You can upload your own data to compare to the standardized data. This needs to be provided as a csv file that will have length and/or weight measurements with one row per observation (a single fish or multiple fish summed). This app will calculate the three metrics of interest for each unique combination of area, species, collection method, type of water body, and year. The three metrics are: 

1. [Catch per unit effort](https://en.wikipedia.org/wiki/Catch_per_unit_effort) (CPUE)
2. Length frequency
3. Relative weight

<center><b>Column Details</b></center>

Below are the details of the required columns in the data. The order of the columns does not matter <b>BUT</b> the column names are <b>case sensitive and must be lower case</b>. Additionally, only upload data from <b>one</b> waterbody sampling effort at a time (under the column `waterbody_name`). There is an example dataset at the bottom of the page with simulated data that may be a helpful guide.

1. CPUE requires `effort` column
2. Length frequency requires `total_length` column 
3. Relative weight requires `weight` and `total_length` columns

Required columns in input dataframe: 

- **Location**: 
  - `type` is *all* or *state*
  - `area` is *North America* or state name, spelled out and capitalized
    - `waterbody_name` is the name of the water body
- **Date**: 
  - `year` is a four-digit numeric
- **Measurements**: 
  - `total_length` is fish record length (mm)
  - `weight` is fish record weight (g)
  - `effort` is specified in **Collection method**
- **Collection method**: see the table below for details
  - `method` must exactly match one of the options in 'Method name'
  - `effort` will contain a numeric value that corresponds to the associated 'Effort type' and 'Unit'; this should be should be the sum of each transect effort by year
  - The 'gill_net_spring' method is for gill netting done between January and June (months 1 - 6), and 'gill_net_fall' is for between July and December (months 7 - 12)

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

| **Method**          | **Effort** | **Unit** |
|--------------------------|-----------------|----------|
| boat_electrofishing      | Time            | seconds  |
| raft_electrofishing      | Time            | seconds  |
| trawl                    | Time            | seconds  |
| gill_net_fall            | Number of nets  | number   |
| gill_net_spring         | Number of nets  | number   |
| hoop_net                 | Number of nets  | number   |
| small_catfish_hoopnet    | Number of nets  | number   |
| large_catfish_hoopnet    | Number of nets  | number   |
| seine                    | Number of nets  | number   |
| bag_seine                | Number of nets  | number   |
| stream_seine             | Number of nets  | number   |
| backpack_electrofishing  | Area            | m<sup>2</sup>       |
| snorkel                  | Area            | m<sup>2</sup>       |
| tow_barge_electrofishing | Area            | m<sup>2</sup>       |

</div>

</center>

- **Type of water body**:
  - `waterbody_type` must exactly match one of the following: *large_standing_waters*, *small_standing_waters*, *two_story_standing_waters*, *wadeable_streams*, *rivers*
- **Species**:
  - `common_name` must exactly match one of following species, as from [`FSA::PSDlit`](https://fishr-core-team.github.io/FSA/):

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

| **Species**            | **Species**                 | **Species**                 | **Species**                 |
|------------------------|-----------------------------|-----------------------------|-----------------------------|
| Arctic Grayling        | Channel Catfish             | Pallid Sturgeon             | Spotted Bass                |
| Bighead Carp           | Chinook Salmon (landlocked) | Palmetto Bass               | Spotted Gar                 |
| Bigmouth Buffalo       | Common Carp                 | Palmetto Bass (original)    | Striped Bass (landlocked)   |
| Black Bullhead         | Cutthroat Trout             | Pumpkinseed                 | Striped Bass (hybrid)       |
| Black Carp             | Flathead Catfish            | Rainbow Trout               | Striped Bass X White Bass   |
| Black Crappie          | Freshwater Drum             | Redear Sunfish              | Suwannee Bass               |
| Blue Catfish           | Gizzard Shad                | River Carpsucker            | Utah Chub                   |
| Bluegill               | Golden Trout                | Rock Bass                   | Walleye                     |
| Brook Trout (lentic)   | Grass Carp                  | Ruffe                       | Warmouth                    |
| Brook Trout (lotic)    | Green Sunfish               | Sauger                      | White Bass                  |
| Brook Trout            | Kokanee                     | Saugeye                     | White Catfish               |
| Brown Bullhead         | Lake Trout                  | Shoal Bass                  | White Crappie               |
| Brown Trout (lentic)   | Largemouth Bass             | Shorthead Redhorse          | White Perch                 |
| Brown Trout (lotic)    | Longnose Gar                | Silver Carp                 | White Sucker                |
| Bull Trout             | Muskellunge                 | Smallmouth Bass             | Yellow Perch                |
| Burbot                 | Northern Pike               | Smallmouth Buffalo          | Yellow Bass                 |
| Chain Pickerel         | Paddlefish                  | Splake                      | Yellow Bullhead             |

</div>

</center>  

<br>

<center><b>Example Dataset</b></center>

<br>  
"
  
  output$instructions <- renderUI({
    tf <- tempfile()
    knit(text = mtext, output = tf)
    HTML(markdown::markdownToHTML(file = tf))
  })
  
  output$example <- renderDT(datatable(ex, options = list(lengthChange = FALSE, 
                                                pageLength = 25)) %>%
                               formatRound(c(8:10), 0) %>%
                               formatString(7))
  
  plotLengthFrequencyuser <- reactive({
    # uu_unfiltered() has a req() for uploaded data, so no more error messages
    temp <- uu_processed() %>% 
      bind_rows(filtered3(), .id = "id") %>% 
      rename(data_source = id) %>% 
      mutate(data_source = case_when(data_source == 1 ~ "User upload", 
                                     data_source == 2 ~ "Standardized")) %>% 
      filter(metric == "Length Frequency")
    
    stand_N <- temp %>% 
      filter(data_source == "Standardized") %>% 
      distinct(N) %>% 
      pull(N)

    user_N <- temp %>% 
      filter(data_source == "User upload") %>% 
      distinct(N) %>% 
      pull(N)
    
    stand_only <- temp %>% 
      filter(data_source == "Standardized")
    
    if(nrow(stand_only) == 0){
      fig <- ggplot() +
        annotate("text", x = 1, y = 1, size = 8,
                 label = "No standardized data \n corresponding to uploaded data") +
        theme_void()
      
    } else {
      fig <- ggplot(temp, aes(x = gcat, y = mean, fill = data_source)) +
        geom_bar(stat = "identity", position = position_dodge(preserve = "single"))  +
        geom_errorbar(aes(ymin = mean - se,
                          ymax = mean + se),
                      position = position_dodge(width = 0.9, preserve = "single"),
                      width = 0) +
        scale_y_continuous("Frequency (%)",
                           limits = c(0, 100),
                           expand = c(0, 0)) +
        scale_fill_discrete("Data source") +
        theme_classic(base_size = 16) +
        xlab("Gabelhouse length") +
        theme(legend.position = c(0.85, 0.85)) +
        ggtitle(paste0("Standardized N = ", stand_N, "; User N = ", user_N))
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
    
    if(nrow(stand_only) == 0){
      
      fig <- ggplot() +
        annotate("text", x = 1, y = 1, size = 8,
                 label = "No standardized data \n corresponding to uploaded data") +
        theme_void()
      
    } else {
      
    fig <- ggplot(temp, aes(x = gcat)) +
      geom_point(aes(y = mean,
                     color = data_source),
                 size = 2.5, 
                 position = position_dodge(width = 0.05)) +
      geom_errorbar(aes(ymin = mean - se, 
                        ymax = mean + se, 
                        color = data_source),
                    width = 0, 
                    position = position_dodge(width = 0.05)) +
      scale_y_continuous("Relative weight") +
      scale_color_discrete("Data source") +
      theme_classic(base_size = 16) +
      xlab("Gabelhouse length") +
      theme(legend.position = "bottom") 
    
    }
    
    print(fig)
    
  })
    
    callModule(plotDownload, "RW_plot_UU", plotRelativeWeightuser)
    
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
    
    user_N <- temp %>% 
      filter(data_source == "User upload") %>% 
      distinct(N) %>% 
      pull(N)
    
    stand_only <- temp %>% 
      filter(data_source == "Standardized")
    
    if(nrow(stand_only) == 0){
      
      fig <- ggplot() +
        annotate("text", x = 1, y = 1, size = 8,
                 label = "No standardized data \n corresponding to uploaded data") +
        theme_void()
      
    } else {
      
      fig <- ggplot(temp, aes(y = area, fill = data_source)) +
        geom_boxplot(aes(xmin = `5%`,
                         xlower = `25%`,
                         xmiddle = `50%`,
                         xupper = `75%`,
                         xmax = `95%`),
                     stat = "identity") +
        scale_x_continuous("CPUE (fish / hour)") +
        scale_fill_discrete("Data source") +
        theme_classic(base_size = 16) +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) +
        ggtitle(paste0("Standardized N = ", stand_N, "; User N = ", user_N))
      
    }
    
    print(fig)
    
  })
    
    callModule(plotDownload, "CPUE_plot_UU", plotCPUEuser)
  
}

shinyApp(ui, server)
