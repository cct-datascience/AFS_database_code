library(shiny)
library(tidyverse)
# library(data.table)
# library(dplyr)
# library(rsconnect)
# library(rmarkdown)
# library(DT)

metrics <- read_csv('Full_results_112122.csv') %>%
  rename(area = region,
         N = count)
uni.area <- c("All", sort(unique(metrics$area)))
uni.spp <- c('All', sort(unique(metrics$common_name)))
uni.method <- c("All", sort(unique(metrics$method)))
uni.watertype <- c("All", sort(unique(metrics$waterbody_type)))
uni.metric <- c("CPUE", "Relative Weight", "Length Frequency") # CPUE_distance not needed?

`%nin%` <- negate(`%in%`)

#Shiny App
ui <- navbarPage("AFS Standard Sampling App",
                 theme = bslib::bs_theme(bootswatch = "darkly"),
                 tabPanel("About",
                          wellPanel(
                            h2("About Standard Sampling", align = "center"),
                            h4("Standardization of sampling methods allows fisheries professionals to compare data across large spatial and temporal scales, encourages data sharing and improves communication. 
In light of these benefits, the American Fisheries Society published Standard Methods for Sampling North American Freshwater Fishes in 2009. 
The goals of this project, were to (1) recommend standardized freshwater fish sampling methods, and (2) provide an online database where existing and future data could be accessed. Since publication, numerous fisheries professionals have adopted the standard methods and many have noted the database as an important tool in management. 
The database allows for comparison of fish metrics commonly used in management to assess population health including growth, condition, length-frequency, and catch per unit effort data collected using standard methods.
When developing version two of the database we examined strengths and weaknesses of methods used for requesting, analyzing, and displaying data in an online format.
We used this information to achieve our goals of maximizing use and providing simple means to update this program for comparison of fisheries data.
Furthermore, our hope is that these methods can be adopted by others, particularly those in data-poor regions, in the development of their own standard methods and fisheries databases. "),
                            h4 ("Sponsored by:"),
                            tags$img(src="AFS_sponsor_3.png", height = 250, width = 150)
                          )
                 ),
                 
                 tabPanel(title = "Obtain Averages",
                          sidebarLayout(
                            sidebarPanel(
                              "Welcome to the American Fisheries Society Standard Sampling Database App!", br(), br(),
                              selectInput(inputId = "areachoice",
                                          label = "Select North America, State(s) or Ecoregion(s):",
                                          choices = uni.area,
                                          multiple=T,
                                          selected='Arizona'),
                              selectInput(inputId = "sppchoice",
                                          label = "Select species:",
                                          choices = uni.spp,
                                          multiple=T,
                                          selected='Bluegill'),
                              selectInput(inputId = "methodchoice",
                                          label = "Select method type(s):",
                                          choices = uni.method,
                                          multiple = T,
                                          selected = "boat_electrofishing"),
                              selectInput(inputId = "watertypechoice",
                                          label = "Select waterbody type(s):",
                                          choices = uni.watertype,
                                          multiple = T,
                                          selected = "large_standing_waters"),
                              selectInput(inputId = "metricchoice",
                                          label = "Select metric(s):",
                                          choices = uni.metric,
                                          multiple = T,
                                          selected = uni.metric),
                              downloadButton("filterdownload", "Download filtered"),
                              downloadButton("alldownload", "Download all")
                              ),
                            
                            mainPanel(
                              tabsetPanel(
                                id = 'metrics',
                                tabPanel("Map", 
                                         # tableOutput("mytable1")
                                         ),
                                tabPanel("Plot", 
                                         # tableOutput("mytable2")
                                         ),
                                tabPanel("Preview", 
                                         tableOutput("filtertable")) 
                                
                                
                              )
                            )
                          )
                 ), 
                 tabPanel(title = "Compare Your Data",
                          fileInput("upload", NULL, 
                                    buttonLabel =  "Upload Your Data", 
                                    multiple = FALSE, accept = (".csv")),
                          tableOutput("user_table"), 
                          dataTableOutput("view_user_table")
                 )
)




server <- function(input, output) {
  
  ## If want to separate state and ecoregion in menu, add reactive part
  ## here to update table selection and subset data
  
  # make filtered, a reactive data object  
  filtered <- reactive({
    
    f_df <-  metrics %>%
      filter(metric %in% input$metricchoice,
             case_when("All" %in% input$areachoice ~ area %in% uni.area,
                       "All" %nin% input$areachoice ~ area %in% input$areachoice),
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
  
  output$filterdownload <- downloadHandler(
    filename = function() {
      paste0("AFS-filtered-", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      write_csv(filtered(), file)
    }
  )
  
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
  
  # Making the table look nice
  output$filtertable <- renderTable({  
    
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
                    "Gable House" = gcat,
                    "N" = N,
                    "Mean" = mean,
                    "SE" = se,
                    "5%" = `5%`,
                    "25%" = `25%`,
                    "50%" = `50%`,
                    "75%" = `75%`,
                    "95%" = `95%`)
  })

  output$user_table <- renderTable({
    inFile <- input$upload
    data <- read.csv(inFile$datapath)
    output$view_user_table <- renderDataTable({
      datatable(data)
  })
  })
  
  #user.results <- reactive({
   # req(input$upload)
    # user.dat.df <- read.csv(input$upload$datapath)
    
    ## Insert code to derive results (equations etc.) for PSD, Wr, CPUE
    #return(user.results.df)}) ## Return the results data.frame you make
  
  
}


shinyApp(ui, server)
