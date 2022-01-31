library(shiny)
library(tidyverse)
library(data.table)
library(dplyr)
library(rsconnect)
library(rmarkdown)

setwd("")
metrics=read.csv('Final_AFSaverages_results_ALL_Ngreaterthanorequalto5.csv',as.is=T)
colnames(metrics)[1] <- "method"
uni.spp=c('All',sort(unique(metrics$common_name)))
uni.method=c("All",sort(unique(metrics$method)))
uni.watertype=c("All",sort(unique(metrics$waterbody_type)))
uni.area=c("All",sort(unique(metrics$area)))

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
                 
                 tabPanel(title = "Database Averages",
                          sidebarLayout(
                            sidebarPanel(
                              "Welcome to the American Fisheries Society Standard Sampling Database App!", br(), br(),
                              "Please select which averages you would like to view by filtering by North America, State or Ecoregion, Fish Species, Sampling Method, or Waterbody Type.", br(), br(),
                              selectInput(inputId = "areachoice",
                                          label = "Select North America, a State or an Ecoregion",
                                          choices = uni.area,
                                          multiple=T,
                                          selected='All'),
                              selectInput(inputId = "fishchoice",
                                          label = "Select a fish species",
                                          choices = uni.spp,
                                          multiple=T,
                                          selected='All'),
                              selectInput(inputId = "gearchoice",
                                          label = "Select a gear type",
                                          choices = uni.method,
                                          multiple = T,
                                          selected = "All"),
                              selectInput(inputId = "watertypechoice",
                                          label = "Select a waterbody type",
                                          choices = uni.watertype,
                                          multiple = T,
                                          selected = "All"),
                              downloadButton("cpuedownload", "Download CPUE summary"),
                              downloadButton("wrdownload", "Download Relative Weight summary"),
                              downloadButton("psddownload", "Download Length Frequency summary")
                              
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                id = 'metrics',
                                tabPanel("CPUE", tableOutput("mytable1")),
                                tabPanel("Relative Weight", tableOutput("mytable2")),
                                tabPanel("Length Frequency", tableOutput("mytable3")) 
                                
                                
                              )
                            )
                          )
                 )
                 #tabPanel(title = "Compare Your Data",
                          #fileInput("upload", NULL, buttonLabel =  "Upload Your Data", multiple = FALSE, accept = (".csv")),
                          #tableOutput("user_table")
                          
                 #)
)




server <- function(input, output) {
  
  ## If want to separate state and ecoregion in menu, add reactive part
  ## here to update table selection and subset data
  
  # make cpue, a reactive data object  
  CPUE <- reactive({
    
    cpue <-  metrics %>%
      # keep rows where metric = cpue
      filter(metric == "CPUE",
             # if all is selected, keep all, else filter to input
             case_when("All" %in% input$fishchoice ~ common_name %in% uni.spp,
                       "All" %nin% input$fishchoice ~ common_name %in% input$fishchoice))
    cpue <- cpue %>% filter(metric == "CPUE",
                            case_when("All" %in% input$gearchoice ~ method %in% uni.method,
                                      "All" %nin% input$gearchoice ~ method %in% input$gearchoice)) 
    cpue <- cpue %>% filter(metric == "CPUE",
                            case_when("All" %in% input$areachoice ~ area %in% uni.area,
                                      "All" %nin% input$areachoice ~ area %in% input$areachoice)) 
    cpue <- cpue %>% filter(metric == "CPUE",     
                            case_when("All" %in% input$watertypechoice ~ waterbody_type %in% uni.watertype,
                                      "All" %nin% input$watertypechoice ~ waterbody_type %in% input$watertypechoice)) %>%
      # sort by common name
      arrange(common_name) %>%
      # keep our preferred columns
      select(area, common_name, method, waterbody_type, N, mean, 
             se, X5., X25., X50., X75., X95.) 
    
  })
  
  output$cpuedownload <- downloadHandler(
    filename = function() {
      paste0("CPUE", ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(CPUE(), file)
    }
  )
  
  Wr <- reactive({
    
    wr <-  metrics %>%
      filter(metric == "Relative Weight",
             case_when("All" %in% input$fishchoice ~ common_name %in% uni.spp,
                       "All" %nin% input$fishchoice ~ common_name %in% input$fishchoice))
    wr <- wr %>% filter(metric == "Relative Weight",
                        case_when("All" %in% input$gearchoice ~ method %in% uni.method,
                                  "All" %nin% input$gearchoice ~ method %in% input$gearchoice)) 
    wr <- wr %>% filter(metric == "Relative Weight",
                        case_when("All" %in% input$areachoice ~ area %in% uni.area,
                                  "All" %nin% input$areachoice ~ area %in% input$areachoice)) 
    wr <- wr %>% filter(metric == "Relative Weight",     
                        case_when("All" %in% input$watertypechoice ~ waterbody_type %in% uni.watertype,
                                  "All" %nin% input$watertypechoice ~ waterbody_type %in% input$watertypechoice)) %>%
      arrange(common_name) %>%
      select(area, common_name, method, waterbody_type, gcat, N, mean, 
             se, X5., X25., X50., X75., X95.) 
  })
  
  output$wrdownload <- downloadHandler(
    filename = function() {
      paste0("RelativeWeight", ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(Wr(), file)
    }
  )
  
  PSD <- reactive({
    
    psd <-  metrics %>%
      filter(metric == "Length Frequency",
             case_when("All" %in% input$fishchoice ~ common_name %in% uni.spp,
                       "All" %nin% input$fishchoice ~ common_name %in% input$fishchoice))
    psd <- psd %>% filter(metric == "Length Frequency",
                          case_when("All" %in% input$gearchoice ~ method %in% uni.method,
                                    "All" %nin% input$gearchoice ~ method %in% input$gearchoice)) 
    psd <- psd %>% filter(metric == "Length Frequency",
                          case_when("All" %in% input$areachoice ~ area %in% uni.area,
                                    "All" %nin% input$areachoice ~ area %in% input$areachoice)) 
    psd <- psd %>% filter(metric == "Length Frequency",     
                          case_when("All" %in% input$watertypechoice ~ waterbody_type %in% uni.watertype,
                                    "All" %nin% input$watertypechoice ~ waterbody_type %in% input$watertypechoice)) %>%
      
      arrange(common_name) %>%
      select(area, common_name, method, waterbody_type, gcat, N, mean, se) 
  })
  
  output$psddownload <- downloadHandler(
    filename = function() {
      paste0("LengthFrequency", ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(PSD(), file)
    }
  )
  
  
  # Making the table look nice
  output$mytable1 <- renderTable({  
    
    CPUE() %>%
      mutate(method = str_replace_all(method, "_", " "),
             area = str_replace_all(area, "_", " "),
             waterbody_type = str_replace_all(waterbody_type, "_", " ")) %>%
      dplyr::rename("Common Name" = common_name,
                    "Method" = method,
                    "Waterbody Type" = waterbody_type,
                    "N" = N,
                    "Mean" = mean,
                    "Standard Error" = se,
                    "5%" = X5.,
                    "25%" = X25.,
                    "50%" = X50.,
                    "75%" = X75.,
                    "95%" = X95.,
                    "Area" = area)
  },
  digits = 2)
  
  output$mytable2 <- renderTable({  
    Wr() %>%
      mutate(method = str_replace_all(method, "_", " "),
             area = str_replace_all(area, "_", " "),
             waterbody_type = str_replace_all(waterbody_type, "_", " ")) %>%
      dplyr::rename("Common Name" = common_name,
                    "Method" = method,
                    "Waterbody Type" = waterbody_type,
                    "PSD category" = gcat,
                    "N" = N,
                    "Mean" = mean,
                    "Standard Error" = se,
                    "5%" = X5.,
                    "25%" = X25.,
                    "50%" = X50.,
                    "75%" = X75.,
                    "95%" = X95.,
                    "Area" = area) 
  },
  digits = 2)
  
  output$mytable3 <- renderTable({  
    PSD() %>%
      mutate(method = str_replace_all(method, "_", " "),
             area = str_replace_all(area, "_", " "),
             waterbody_type = str_replace_all(waterbody_type, "_", " ")) %>%
      dplyr::rename("Common Name" = common_name,
                    "Method" = method,
                    "PSD category" = gcat,
                    "Waterbody Type" = waterbody_type,
                    "N" = N,
                    "Mean%" = mean,
                    "Standard Error" = se,
                    "Area" = area, )
  },
  digits = 2) 
  
  #user.results <- reactive({
   # req(input$upload)
    # user.dat.df <- read.csv(input$upload$datapath)
    
    ## Insert code to derive results (equations etc.) for PSD, Wr, CPUE
    #return(user.results.df)}) ## Return the results data.frame you make
  
  
}


shinyApp(ui, server)
