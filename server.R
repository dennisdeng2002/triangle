
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(ggtern)
library(gridSVG)
library(svgPanZoom)
library(rhandsontable)

shinyServer(function(input, output) {
  # Used to resize graph (zoom)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # Generate ternary plot
  output$TernPlot <- renderPlot({
  # Store data file
  infile <- input$file1
  # Check if data file is valid
  if (is.null(infile))
    return(NULL)
  else{
    myData = read.csv(infile$datapath)
    # Fill table with uploaded data
    setHot(myData)
    output$hot <- renderRHandsontable({
      DF = NULL
      # Set table values after change
      if (!is.null(input$hot)) {
        DF = hot_to_r(input$hot)
        setHot(DF)
        values[["hot"]] = DF} 
      # Check if table values isn't empty
      else if (!is.null(values[["hot"]])) {
        DF = values[["hot"]]
      }
      # If empty initialize table = myData
      else {
        DF = myData
        setHot(DF)
        rhandsontable(DF) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE)}
      
      if (!is.null(DF)){
        rhandsontable(DF) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE)
      }
    })
  }
  
  # Render ternary diagram
  gg <- ggtern(data = myData, aes_string(x=colnames(myData)[1], y=colnames(myData)[2], z=colnames(myData)[3])) +
  geom_point() + 
  coord_tern(T = getOption("tern.default.T"), L = getOption("tern.default.L"),
             R = getOption("tern.default.R"), xlim = ranges$x, ylim = ranges$y,
             Tlim = NULL, Llim = NULL, Rlim = NULL, clockwise)
  svgPanZoom(gg, controlIconsEnabled = TRUE)
  })
  
  # Check for brush/double-click used for zoom
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }})
  # Output data for click, double-click, hover, and brush
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
   
  # Update table data
  values = list()
  setHot = function(x) values[["hot"]] <<- x
  
  # Graph Button
  observeEvent(input$plot_button,{
    if(!is.null(values[["hot"]])){
      myData = values[["hot"]]
    }
    else{
      myData = data.frame(matrix(0, nrow=10, ncol=3))
    }
    output$TernPlot <- renderPlot({
    gg <- ggtern(data = myData, aes_string(x=colnames(myData)[1], y=colnames(myData)[2], z=colnames(myData)[3])) +
      geom_point() + 
      coord_tern(T = getOption("tern.default.T"), L = getOption("tern.default.L"),
                 R = getOption("tern.default.R"), xlim = ranges$x, ylim = ranges$y,
                 Tlim = NULL, Llim = NULL, Rlim = NULL, clockwise)
    svgPanZoom(gg, controlIconsEnabled = TRUE)})
  })
  
  output$hot <- renderRHandsontable({
    DF = NULL
    # Set table values after change
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
      setHot(DF)
      values[["hot"]] = DF} 
    # Check if table values isn't empty
    else if (!is.null(values[["hot"]])) {
      DF = values[["hot"]]
    }
    # If empty initialize table = 0
    else {
      DF = data.frame(matrix(0, nrow=10, ncol=3))
      setHot(DF)
      rhandsontable(DF) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)}
    
    if (!is.null(DF)){
      rhandsontable(DF) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }
  })
 
})
