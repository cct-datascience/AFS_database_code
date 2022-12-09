library(shiny)
library(shinyWidgets)
library(tidyverse)
# library(data.table)
# library(dplyr)
# library(rsconnect)
# library(rmarkdown)
# library(DT)

metrics <- read_csv('Full_results_112122.csv') %>%
  rename(area = region,
         N = count)
uni.type <- c("North America", "Ecoregion", "State/Province")
uni.area <- sort(unique(metrics$area))
uni.spp <- sort(unique(metrics$common_name))
uni.method <- sort(unique(metrics$method))
uni.watertype <- sort(unique(metrics$waterbody_type))
uni.metric <- c("Length Frequency", "Relative Weight", "CPUE") 

`%nin%` <- negate(`%in%`)

# Test data

foo <- metrics %>%
  filter(area == "Arizona",
         common_name == "Bluegill",
         method == "boat_electrofishing",
         waterbody_type == "large_standing_waters")

#Shiny App
ui <- navbarPage("AFS Standard Sampling App",
                 theme = bslib::bs_theme(bootswatch = "sandstone"),
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
                                          label = "Select method type(s):",
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
                                tabPanel("Map"),
                                tabPanel("Preview", 
                                         tableOutput("filtertable"))
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
                                   label = "Select method type(s):",
                                   choices = uni.method,
                                   selected = "boat_electrofishing"),
                       selectInput(inputId = "watertypechoice2",
                                   label = "Select waterbody type(s):",
                                   choices = uni.watertype,
                                   selected = "large_standing_waters")
                       ),
                     mainPanel(plotOutput("plotLengthFrequency"),
                               plotOutput("plotRelativeWeight"),
                               plotOutput("plotCPUE", height = "200px")
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
  
  # Render a UI for selecting area depending on North America, Ecoregions, or State/Province
  output$dyn_area <- renderUI({
    if(input$typechoice == "North America") {
      temp <- "North_America"
    } else if(input$typechoice == "Ecoregion") {
      temp <- uni.area[1:11]
    } else if(input$typechoice == "State/Province") {
      temp2 <-  uni.area[uni.area %nin% 'North_America']
      temp <- temp2[-1:-11]
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
      temp <- "North_America"
    } else if(input$typechoice2 == "Ecoregion") {
      temp <- uni.area[1:11]
    } else if(input$typechoice2 == "State/Province") {
      temp2 <-  uni.area[uni.area %nin% 'North_America']
      temp <- temp2[-1:-11]
    }
    
    selectInput(inputId = "areachoice2",
                label = "Select area:", 
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
  
  output$plotLengthFrequency <- renderPlot({
    temp <- filtered2() %>%
      filter(metric == "Length Frequency") %>%
      mutate(gcat = factor(gcat, 
                           levels = c("stock", "quality", "preferred", "memorable", "trophy")))
    N <- unique(temp$N)
    
    fig <- ggplot(temp, aes(x = gcat, y = mean)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = mean - se,
                        ymax = mean + se),
                    width = 0) +
      scale_y_continuous("Frequency (%)",
                         limits = c(0, 100),
                         expand = c(0, 0)) +
      theme_classic(base_size = 16) +
      theme(axis.title.x = element_blank()) +
      ggtitle(paste0("N = ", N))
    
    print(fig)
    
  })
  
  output$plotRelativeWeight <- renderPlot({
    
    temp <- filtered2() %>%
      filter(metric == "Relative Weight") %>%
      mutate(gcat = factor(gcat, 
                           levels = c("stock", "quality", "preferred", "memorable", "trophy")))
    
    ypos <- min(temp$`25%`,  na.rm = TRUE) - 2
    
    fig <- ggplot(temp, aes(x = gcat)) +
      geom_point(aes(y = mean,
                     color = "Mean"),
                 size = 2.5) +
      geom_line(aes(y = mean,
                    group = metric, 
                    color = "Mean"),
                size = 0.75) +
      geom_text(aes(y = ypos,
                    label = N),
                vjust = 0.5) +
      geom_line(aes(y = `25%`,
                    group = metric,
                    color = "25th percentile"),
                lty = 2) +
      geom_line(aes(y = `75%`,
                    group = metric,
                    color = "75th percentile"),
                lty = 2) +
      scale_y_continuous("Relative weight") +
      scale_color_manual(values = c("darkblue", "darkred", "black")) +
      theme_classic(base_size = 16) +
      theme(axis.title.x = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()) +
      guides(color = guide_legend(override.aes = 
                                   list(shape = c(NA, NA, 16),
                                        lty = c(2, 2, 1))))  
    
    print(fig)
    
  })
  
  output$plotCPUE <- renderPlot({
    
    temp <- filtered2() %>%
      filter(metric == "CPUE") %>%
      mutate(gcat = factor(gcat, 
                           levels = c("stock", "quality", "preferred", "memorable", "trophy")))

    N <- unique(temp$N)
    
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
    
    print(fig)
    
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
