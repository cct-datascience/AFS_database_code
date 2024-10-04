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

plotDownload <- function(input, output, session, plotFun) {
  output$plot <- renderPlot({
    plotFun()
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      "plot.png"
    },
    content = function(file) {
      ggsave(file, plotFun(), width = 9, height = 6)
    }
  )
}