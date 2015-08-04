
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(ggtern)
library(plotly)
library(gridSVG)
library(svgPanZoom)
library(rhandsontable)

shinyServer(function(input, output) {
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # Generate Ternary Plot
  output$TernPlot <- renderPlot({

  # Store data file
  infile <- input$file1
  # Check if data file is valid
  if (is.null(infile))
    return(NULL)
  else{
    myData = read.csv(infile$datapath)
    }
  # Check for brush/double click - zoom
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }})
  
  # Render ternary diagram
  gg <- ggtern(data = myData, aes_string(x=colnames(myData)[1], y=colnames(myData)[2], z=colnames(myData)[3])) +
  geom_point() + 
  coord_tern(T = getOption("tern.default.T"), L = getOption("tern.default.L"),
             R = getOption("tern.default.R"), xlim = ranges$x, ylim = ranges$y,
             Tlim = NULL, Llim = NULL, Rlim = NULL, clockwise)
  svgPanZoom(gg, controlIconsEnabled = TRUE)
  })
  
  
  # Update table data
  values = list()
  setHot = function(x) values[["hot"]] <<- x
  
  observe({
    input$plot_button
    if (!is.null(values[["hot"]])) 
      print(values[["hot"]])
  })
  
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
  
  output$hot <- renderRHandsontable({
  
  # Check if table is empty - intialize table
  if (!is.null(input$tabledata)) {
    DF = hot_to_r(input$tabledata)
    setHot(DF)
    rhandsontable(DF) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)} 
  else {
    DF = data.frame(matrix("", nrow=10, ncol=3))
    setHot(DF)
    rhandsontable(DF) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)}
  })
 
})
