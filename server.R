
library(shiny)
library(ggtern)
library(gridSVG)
library(svgPanZoom)
library(SVGAnnotation)
library(rhandsontable)
library(tools)

shinyServer(function(input, output, session) {
  
  # Used to resize graph (zoom)
  ranges <- reactiveValues(x = NULL, y = NULL)
  # Counter used to initialize table at startup
  counter <- reactiveValues(i = 0, j = 0, k = 0)
  # Update table data structure
  values = list()
  setEQhot = function(x) values[["EQhot"]] <<- x
  setTLhot = function(x) values[["TLhot"]] <<- x
  sethot = function(x) values[["hot"]] <<- x
  
  # Set equilibrium graph data
  myEQData <- reactive({
    
    # Call reactive function for data file, graph/clear button
    input$EQfile
    input$graph_button
    input$clear_button
    
    # Initialize empty table during startup
    if(counter$i == 0){
      # Set table to default (0)
      DF = data.frame(matrix(0.0, nrow=10, ncol=3))
      setEQhot(DF)
      counter$i = isolate(counter$i) + 1
    }
    
    # Check if file has been uploaded
    observeEvent(input$EQfile, priority = 1,{
      infile <- input$EQfile
      #  Validate file contents
      validate(
        need(file_ext(infile$name) %in% c(
          'text/csv',
          'text/comma-separated-values',
          'text/tab-separated-values',
          'text/plain',
          'csv',
          'tsv'
        ), "Incorrect File Format try again!"))
      # Check if data file is valid
      if (is.null(infile)){
        # Set table to default (0)
        DF = data.frame(matrix(0.0, nrow=10, ncol=3))
        setEQhot(DF)}
      else{
      # Set table to uploaded data
      DF = read.csv(infile$datapath)
      setEQhot(DF)}
    })
    
    # Check if graph button has been pressed
    observeEvent(input$graph_button, priority = 1,{
      # Check if table input exists
      if (!is.null(input$EQhot)) {
        # Set graph data to current table data
        DF = hot_to_r(input$EQhot)
        setEQhot(DF)}
    })
    
    # Check if clear button has been pressed
    observeEvent(input$clear_button, priority = 1,{
      # Set table to default (0)
      DF = data.frame(matrix(0.0, nrow=10, ncol=3))
      setEQhot(DF)
    })
    
    # Return updated data
    values[["EQhot"]]
  })
  
  # Set tie-line graph data
  myTLData <- reactive({
    
    # Call reactive function for data file, graph/clear button
    input$TLfile
    input$graph_button
    input$clear_button
    
    # Initialize empty table during startup
    if(counter$j == 0){
      # Set table to default (0)
      DF = data.frame(matrix(0.0, nrow=4, ncol=6))
      setTLhot(DF)
      counter$j = isolate(counter$j) + 1
    }
    
    # Check if file has been uploaded
    observeEvent(input$TLfile, priority = 1,{
      infile <- input$TLfile
      #  Validate file contents
      validate(
        need(file_ext(infile$name) %in% c(
          'text/csv',
          'text/comma-separated-values',
          'text/tab-separated-values',
          'text/plain',
          'csv',
          'tsv'
        ), "Incorrect File Format try again!"))
      
      # Check if data file is valid
      if (is.null(infile)){
        # Set table to default (0)
        DF = data.frame(matrix(0.0, nrow=4, ncol=6))
        setTLhot(DF)}
      else{
        # Set table to uploaded data
        DF = read.csv(infile$datapath)
        setTLhot(DF)}
    })
    
    # Check if graph button has been pressed
    observeEvent(input$graph_button, priority = 1,{
      # Check if table input exists
      if (!is.null(input$TLhot)) {
        # Set graph data to current table data
        DF = hot_to_r(input$TLhot)
        setTLhot(DF)}
    })
    
    # Check if clear button has been pressed
    observeEvent(input$clear_button, priority = 1,{
      # Set table to default (0)
      DF = data.frame(matrix(0.0, nrow=4, ncol=6))
      setTLhot(DF)
    })
    
    # Return updated data
    values[["TLhot"]]
  })
  
  # Manually add point to graph
  myData <- reactive({

    # Call reactive function for clear button
    input$clear_button
    
    # Initialize empty table during startup
    if(counter$k == 0){
      # Set table to default (0)
      DF = data.frame(matrix(0.0, nrow=1, ncol=3))
      sethot(DF)
      counter$k = isolate(counter$k) + 1
    }
    
    # Call reactive function for double click - store as variable A
    A <- input$plot_dblclick
    if(length(A)!=0){
      x1 = sqrt(3)/2*(1-A$x-A$y)
      x2 = 0.5*(1+A$x-A$y)
      x3 = 1-x1-x2
      DF=data.frame(x1,x2,x3)
      sethot(DF)
    }
    # Check if clear button has been pressed
    observeEvent(input$clear_button, priority = 1,{
    })
    
    # Return data point
    values[["hot"]]
  })

  # Generate ternary plot
  output$TernPlot <- renderPlot({
    # Render ternary diagram
    gg <- ggtern(data = myEQData(), aes_string(x=colnames(myEQData())[1], y=colnames(myEQData())[2], z=colnames(myEQData())[3])) +
    geom_point() +
    # Render tie-lines
    geom_segment(data = myTLData(), aes_string(x=colnames(myTLData())[1],y=colnames(myTLData())[2],z=colnames(myTLData())[3],xend=colnames(myTLData())[4],yend=colnames(myTLData())[5],zend=colnames(myTLData())[6])) +
    # Add point
    geom_point(data = myData(), aes_string(x=colnames(myData())[1], y=colnames(myData())[2], z=colnames(myData())[3]))
    # Used to resize graph during zoom
#     + coord_tern(T = getOption("tern.default.T"), L = getOption("tern.default.L"),
#                R = getOption("tern.default.R"), xlim = ranges$x, ylim = ranges$y,
#                Tlim = NULL, Llim = NULL, Rlim = NULL, clockwise)
    
    # Works with both renderPlot/plotOutput (mouse events) and renderSvgPanZoom/svgPanZoomOutput (native pan/zoom)
    svgPanZoom(gg, controlIconsEnabled = TRUE)
    
    # Faster loading time - only works with renderSvgPanZoom/svgPanZoomOutput (native pan/zoom)
    # svgPanZoom(svgPlot(show(gg), addInfo = F), panEnabled = FALSE)
    
    # Non SVG plot - renderPlot/plotOutput (mouse events)
    # gg
  })
  
  # Render equilibrium data table
  output$EQhot <- renderRHandsontable({
    rhandsontable(myEQData()) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  # Render tie-line data table
  output$TLhot <- renderRHandsontable({
    rhandsontable(myTLData()) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
#   # Check for brush/double-click, used for zoom
#   observeEvent(input$plot_dblclick, {
#     
#     brush <- input$plot_brush
#     if (!is.null(brush)) {
#       ranges$x <- c(brush$xmin, brush$xmax)
#       ranges$y <- c(brush$ymin, brush$ymax)
#       
#     } else {
#       ranges$x <- NULL
#       ranges$y <- NULL
#     }})
  
  # Output data for click, double-click, hover, and brush
  output$info <- renderText({
    
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 2), " y=", round(e$y, 2), "\n")
    }
    
#     xy_range_str <- function(e) {
#       if(is.null(e)) return("NULL\n")
#       paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
#              " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
#     }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover)
      # "brush: ", xy_range_str(input$plot_brush)
    )
  })
  
})
