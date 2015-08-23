library(shiny)
library(ggtern)
library(gridSVG)
library(svgPanZoom)
library(SVGAnnotation)
library(rhandsontable)
library(tools)

source("functions/interpolate.R")
source("functions/interpolateTL.R")
source("functions/sortDecreasing.R")
source("functions/normalize.R")
source("functions/plotit.R")




shinyServer(function(input, output, session) {
  
  # Used to resize graph (zoom)
  #zoomranges <- reactiveValues(x = NULL, y = NULL)
  # Counter used to initialize table at startup
  counter <- reactiveValues(i = 0, j = 0, k = 0)
  # Used to distinguish raffinate/extract values
  ranges <- reactiveValues(E = seq(), R = seq())
  # Update table data structure
  values = list()
  setTable = function(x, name) values[[name]] <<- x
  # Make toggle values reactive
  toggle = reactiveValues(status = NULL, hit = 1, rowsEQ = 0)

  
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
      setTable(DF, name = "EQhot")
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
        return(NULL) 
        }
      else{
      
      # Set table to uploaded data
      DF = read.csv(infile$datapath)
      setTable(DF, name = "EQhot")
      # Update sliders based on number of equilibrium points (initial estimate)
      # Data sets with odd numbers will require rounding (not yet implemented)
      updateSliderInput(session, "raffinate", min = 1, max = nrow(DF), value = c(1,nrow(DF)/2))
      updateSliderInput(session, "extract", min = 1, max = nrow(DF), value = c((nrow(DF)/2)+1,nrow(DF)))
      # Extract Column Headings
      col_head <- colnames(myEQData())
      toggle$on <-TRUE
      toggle$rowsEQ <- nrow(myEQData())
      }
    })
    
    # Check if graph button has been pressed
    observeEvent(input$graph_button, priority = 1,{
      # Check if table input exists
      if (!is.null(input$EQhot)) {
        # Set graph data to current table data
        DF = hot_to_r(input$EQhot)
        setTable(DF, name = "EQhot")}
    })
    
    # Check if clear button has been pressed
    observeEvent(input$clear_button, priority = 1,{
      # Set table to default (0)
      
      DF = data.frame(matrix(0.0, nrow=toggle$rowsEQ, ncol=3))
      setTable(DF, name = "EQhot")
     
    })
    
    # Return updated data
    values[["EQhot"]]
    

  })
  
  
  
  # Set tie-line graph data
  myTLData <- reactive({
    
    # Call reactive function for data file, graph/clear button
    input$TLfile
    input$EQfile
    input$graph_button
    input$clear_button
    # Call reactive function for slider inputs, set ranges for raffinate and extract
    Raf <- input$raffinate
    Ext <- input$extract
    # Set reactive ranges
    ranges$R <- seq(Raf[1],Raf[2])
    ranges$E <- seq(Ext[1],Ext[2])
    # Initialize empty table during startup
    if(counter$j == 0){
      # Set table to default (0)
      DF2 = data.frame(matrix(0.0, nrow=4, ncol=2))
      setTable(DF2, name = "TLhot")
      # Set returned graph data to default (0)
      TLData = data.frame(matrix(0.0, nrow=4, ncol=6))
      setTable(TLData, name = "TLgraph")
      counter$j = isolate(counter$j) + 1
    }
    
    # Check if file has been uploaded
    observeEvent(input$TLfile, priority = 1,{
      infile   <- input$TLfile
      infileEQ <- input$EQfile
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
      validate(
        need(file_ext(infileEQ$name) %in% c(
          'text/csv',
          'text/comma-separated-values',
          'text/tab-separated-values',
          'text/plain',
          'csv',
          'tsv'
        ), "Incorrect File Format try again!"))
      
      # Check if data file is valid
      if ((is.null(infile))){
        # Set table to default (0)
        DF2 = data.frame(matrix(0.0, nrow=4, ncol=2))
        setTable(DF2, name = "TLhot")
        # Set returned graph data to default (0)
        TLData = data.frame(matrix(0.0, nrow=4, ncol=6))
        setTable(TLData, name = "TLgraph")}
      else{
        # Set table to uploaded data
         DF = read.csv(infileEQ$datapath)
         setTable(DF, name = "EQhot")
         DF2 = read.csv(infile$datapath)
         setTable(DF2, name = "TLhot")
        # Set returned graph data to interpolated valuesL(values, ranges)
         TLData = interpolateTL(values, ranges)
         setTable(TLData, name = "TLgraph")
        }
    })
    
    # Check if graph button has been pressed
    observeEvent(input$graph_button, priority = 1,{
      # Check if table input exists
      if (!is.null(input$TLhot)) {
        # Set graph data to current table data
        DF2 = hot_to_r(input$TLhot)
        setTable(DF2, name = "TLhot")
        # Set returned graph data to default (0)
        TLData = interpolateTL(values, ranges)
        setTable(TLData, name = "TLgraph")
        }
    })
    
    # Check if clear button has been pressed
    observeEvent(input$clear_button, priority = 1,{
      # Set table to default (0)
      DF2 = data.frame(matrix(0.0, nrow=4, ncol=2))
      setTable(DF2, name = "TLhot")
      # Set returned graph data to default (0)
      TLData = data.frame(matrix(0.0, nrow=4, ncol=6))
      setTable(TLData, name = "TLgraph")

    
    })
    
    # Return list of table & graph data
    result = list(values[["TLgraph"]], values[["TLhot"]])
  })
  


  # Generate ternary plot
  output$TernPlot <- renderSvgPanZoom({
    # Extract data from myTLData() as a data frame instead of a list value
    TLData <- as.data.frame(myTLData()[1])
    # Render ternary diagram
    
    
    gg <- plotit(myEQData, TLData, toggle$hit)
    
    # Works with both renderPlot/plotOutput (mouse events) and renderSvgPanZoom/svgPanZoomOutput (native pan/zoom)
    # svgPanZoom(gg, controlIconsEnabled = TRUE)
    
    # Faster loading time - only works with renderSvgPanZoom/svgPanZoomOutput (native pan/zoom)
    svgPanZoom(svgPlot(show(gg), addInfo = F), panEnabled = FALSE)
    
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
    # Extract data from myTLData() as a data frame instead of a list value
    TLData <- as.data.frame(myTLData()[2])
    rhandsontable(TLData) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
 
  # Toggle options
  output$fileUploaded <- reactive({
    return(!is.null(toggle$on))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  observeEvent(input$axistog, {
    toggle$hit <- ((input$axistog[1]) %% 6 ) + 1
  })

})
