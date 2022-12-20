library(shiny)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(leaflet)
library(DT)

metrics <- read_csv('Full_results_112122.csv') %>%
  rename(area = region,
         N = count)
uni.type <- c("North America", "Ecoregion", "State/Province")
uni.area <- sort(unique(metrics$area))
uni.spp <- sort(unique(metrics$common_name))
uni.method <- sort(unique(metrics$method))
uni.watertype <- sort(unique(metrics$waterbody_type))
uni.metric <- c("Length Frequency", "Relative Weight", "CPUE") 

ecoregions <- read_sf("ecoregions1/NA_CEC_Eco_Level1.shp")
ecoregions_crs <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
ecoregions_trans <- ecoregions %>% 
  rmapshaper::ms_simplify() %>% 
  st_set_crs(ecoregions_crs) %>%
  st_transform("+proj=longlat +datum=WGS84")

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
                                tabPanel("Map", 
                                         leafletOutput("plotSites")), #incrase height of map
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
                                   label = "Select method type:",
                                   choices = uni.method,
                                   selected = "boat_electrofishing"),
                       selectInput(inputId = "watertypechoice2",
                                   label = "Select waterbody type:",
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
                         sidebarLayout(
                           sidebarPanel(
                             "Welcome to the American Fisheries Society Standard Sampling Database App!",
                             br(),
                             br(),
                             "Upload your fish data for comparison here: ", 
                             fileInput("upload", NULL,
                                       buttonLabel =  "Upload Your Data",
                                       multiple = FALSE, accept = (".csv")), 
                             "To view plots of standard fish data, select one option from each menu below.",
                             radioGroupButtons(inputId = "typechoice3",
                                               label = "Show data by:",
                                               choices = uni.type,
                                               direction = "vertical",
                                               selected = "North America"),
                             uiOutput("dyn_area3"),
                             uiOutput("dyn_spp3"),
                             uiOutput("dyn_method3"),
                             uiOutput("dyn_watertype3")
                           ),
                           mainPanel(plotOutput("plotLengthFrequencyuser"), 
                                     plotOutput("plotRelativeWeightuser"), 
                                     plotOutput("plotCPUEuser"))
                         ), 
# ,
#                          tableOutput("user_table"),
#                          dataTableOutput("view_user_table")
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

  # Read in raw user upload data to be used in reactive UI's
  uu_raw <- reactive({
    inFile <- input$upload
    uu <- read_csv(inFile$datapath)
    
  })
  
  
  # Render a UI for selecting area depending on North America, Ecoregions, or State/Province for tab 3
  output$dyn_area3 <- renderUI({
    req(input$upload)
    
    if(input$typechoice3 == "North America") {
      temp <- "North_America"
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
    
    temp <- uu_raw() %>%
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
    
    temp <- uu_raw() %>%
      select(method) %>%
      unique() %>%
      pull()
    
    selectInput(inputId = "methodchoice3",
                label = "Select method type::",
                choices = temp,
                selected = temp[1])
  })  
  
  # Render a UI for selecting species depending on user upload data in tab 3
  output$dyn_watertype3 <- renderUI({
    req(input$upload)
    
    temp <- uu_raw() %>%
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
  
  uu_filtered <- reactive({
    # Suppresses error messages for plots until inputs are chosen
    req(input$areachoice3)
    req(input$sppchoice3)
    req(input$methodchoice3)
    req(input$watertypechoice3)
    
    inFile <- input$upload
    uu <- read_csv(inFile$datapath)
    print("UU original")
    print(uu)
    f_df <-  uu %>%
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
  
  output$plotSites <- renderLeaflet({
    plot_data <- filtered() %>%  
      mutate(lat = runif(nrow(filtered()), min = 30, max = 45), 
             lon = ifelse(area == "8", 
                          runif(nrow(filtered()), min = -90, max = -75), 
                          runif(nrow(filtered()), min = -120, max = -90)), 
             lat = as.numeric(lat), 
             lon = as.numeric(lon))
    
    factpal <- colorFactor(rainbow(length(unique(ecoregions_trans$NA_L1CODE))), 
                           ecoregions_trans$NA_L1CODE)
    
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data = plot_data, lng = ~lon, lat = ~lat, stroke = FALSE, 
                       radius = 3, fillOpacity = 1) %>% 
      addPolygons(data = ecoregions_trans, color = ~factpal(NA_L1CODE),
                  fillOpacity = 0.5, popup = ~htmltools::htmlEscape(NA_L1NAME),
                  stroke = FALSE)
  })
  
  output$plotLengthFrequency <- renderPlot({
    temp <- filtered2() %>%
      filter(metric == "Length Frequency") %>%
      mutate(gcat = factor(gcat, 
                           levels = c("stock", "quality", "preferred", "memorable", "trophy")))
    
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
        theme(axis.title.x = element_blank(), 
              legend.position = "none") +
        ggtitle(paste0("N = ", N))
    }
    
    print(fig)
    
  })
  
  output$plotRelativeWeight <- renderPlot({
    
    temp <- filtered2() %>%
      filter(metric == "Relative Weight") %>%
      mutate(gcat = factor(gcat, 
                           levels = c("stock", "quality", "preferred", "memorable", "trophy")))
    
    ypos <- min(temp$`25%`,  na.rm = TRUE) - 2

    if(nrow(temp) == 0){
      fig <- ggplot() +
        annotate("text", x = 1, y = 1, size = 8,
                 label = "No relative weight data for selected options") +
        theme_void()
    } else {
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
    }    

    print(fig)
    
  })
  
  output$plotCPUE <- renderPlot({
    
    temp <- filtered2() %>%
      filter(metric == "CPUE") %>%
      mutate(gcat = factor(gcat, 
                           levels = c("stock", "quality", "preferred", "memorable", "trophy")))

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

  output$plotLengthFrequencyuser <- renderPlot({
    #TODO: don't plot standardized data if there is no corresponding user upload data
    #if nrow uu_filtered is 0...
    temp <- uu_filtered() %>% 
      bind_rows(filtered3(), .id = "id") %>% 
      rename(data_source = id) %>% 
      mutate(data_source = case_when(data_source == 1 ~ "User upload", 
                                     data_source == 2 ~ "Standardized")) %>% 
      filter(metric == "Length Frequency") %>%
      mutate(gcat = factor(gcat, 
                           levels = c("stock", "quality", "preferred", "memorable", "trophy")))
    
    stand_N <- temp %>% 
      filter(data_source == "Standardized") %>% 
      distinct(N) %>% 
      pull(N)

    user_N <- temp %>% 
      filter(data_source == "User upload") %>% 
      distinct(N) %>% 
      pull(N)
    
    fig <- ggplot(temp, aes(x = gcat, y = mean, fill = data_source)) +
      geom_bar(stat = "identity", position = "dodge")  +
      geom_errorbar(aes(ymin = mean - se,
                        ymax = mean + se),
                    position = position_dodge(width = 0.9), 
                    width = 0) +
      scale_y_continuous("Frequency (%)",
                         limits = c(0, 100),
                         expand = c(0, 0)) +
      scale_fill_discrete("Data source") +
      theme_classic(base_size = 16) +
      theme(axis.title.x = element_blank(),
            legend.position = c(0.85, 0.85)) +
      ggtitle(paste0("Standardized N = ", stand_N, "; User N = ", user_N))
    
    print(fig)

  })
  
  output$plotRelativeWeightuser <- renderPlot({
    
    temp <- uu_filtered() %>% 
      bind_rows(filtered3(), .id = "id") %>% 
      rename(data_source = id) %>% 
      mutate(data_source = case_when(data_source == 1 ~ "User upload", 
                                     data_source == 2 ~ "Standardized")) %>% 
      filter(metric == "Relative Weight") %>%
      mutate(gcat = factor(gcat, 
                           levels = c("stock", "quality", "preferred", "memorable", "trophy")))
    
    fig <- ggplot(temp, aes(x = gcat)) +
      geom_point(aes(y = mean,
                     color = data_source),
                 size = 2.5) +
      geom_line(aes(y = mean,
                    group = data_source, 
                    color = data_source),
                size = 0.75) +
      geom_line(aes(y = `25%`,
                    group = data_source,
                    color = data_source),
                lty = 2) +
      geom_line(aes(y = `75%`,
                    group = data_source,
                    color = data_source),
                lty = 2) +
      scale_y_continuous("Relative weight") +
      scale_color_discrete("Data source") +
      theme_classic(base_size = 16) +
      theme(axis.title.x = element_blank(),
            legend.position = "bottom") 
    
    print(fig)
    
  })
  
  output$plotCPUEuser <- renderPlot({
    temp <- uu_filtered() %>% 
      bind_rows(filtered3(), .id = "id") %>% 
      rename(data_source = id) %>% 
      mutate(data_source = case_when(data_source == 1 ~ "User upload", 
                                     data_source == 2 ~ "Standardized")) %>% 
      filter(metric == "CPUE")
    
    print(temp, n = Inf)
    
    stand_N <- temp %>% 
      filter(data_source == "Standardized") %>% 
      distinct(N) %>% 
      pull(N)
    
    user_N <- temp %>% 
      filter(data_source == "User upload") %>% 
      distinct(N) %>% 
      pull(N)
    
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
    
    print(fig)
    
  })
  
}

shinyApp(ui, server)
