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
    input$EQgraph_button
    input$EQclear_button
    
    # Initialize empty table during startup
    if(counter$i == 0){
      # Set table to default (0)
      DF = data.frame(matrix(0.0, nrow=10, ncol=3))
      setTable(DF, name = "EQhot")
      counter$i = isolate(counter$i) + 1
    }
    
    # Return updated data
    values[["EQhot"]]

  })
  
  # Equilibrium Data Events
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
      updateSliderInput(session, "raffinate", min = 1, max = nrow(DF), value = c(1,nrow(DF)/2))
      updateSliderInput(session, "extract", min = 1, max = nrow(DF), value = c((nrow(DF)/2)+1,nrow(DF)))
      # Extract Column Headings
      col_head <- colnames(myEQData())
      toggle$on <- TRUE
      toggle$rowsEQ <- nrow(myEQData())
      # Update component names
      updateSelectInput(session, "TLcomponent", choices = col_head)
      DF3 = values[["hot"]]
      col_head = colnames(DF)
      if(!is.null(colnames(DF3))){
        colnames(DF3) <- c(col_head[1], col_head[2], col_head[3], "Label")
        setTable(DF3, name = "hot")
      }
    }
  })
  
  # Check if graph button has been pressed
  observeEvent(input$EQgraph_button, priority = 1,{
    # Check if table input exists
    if (!is.null(input$EQhot)) {
      # Set graph data to current table data
      DF = hot_to_r(input$EQhot)
      setTable(DF, name = "EQhot")}
  })
  
  # Check if clear button has been pressed
  observeEvent(input$EQclear_button, priority = 1,{
    # Set table to default (0)
    DF = data.frame(matrix(0.0, nrow=10, ncol=3))
    setTable(DF, name = "EQhot")
    # Reset component input and sliders
    updateSelectInput(session, "TLcomponent", choices = colnames(values[["EQhot"]]))
    updateSliderInput(session, "raffinate", min = 1, max = nrow(DF), value = c(1,5))
    updateSliderInput(session, "extract", min = 1, max = nrow(DF), value = c(6,10))
    # Close any alerts
    closeAlert(session, "TLalert")
    # Clear uploaded file
    session$sendCustomMessage(type = "resetFileInputHandler", 'EQfile')
    # Reset column names for additional data
    DF3 = values[["hot"]]
    col_head = colnames(DF)
    if(!is.null(colnames(DF3))){
      colnames(DF3) <- c(col_head[1], col_head[2], col_head[3], "Label")
      setTable(DF3, name = "hot")
    }
  })
  
  # Set tie-line graph data
  myTLData <- reactive({
    
    # Call reactive function for data file, graph/clear button
    input$TLfile
    input$EQfile
    input$TLgraph_button
    input$TLclear_button
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
      colnames(DF2) <- c("Raffinate", "Extract")
      setTable(DF2, name = "TLhot")
      # Set returned graph data to default (0)
      TLData = data.frame(matrix(0.0, nrow=4, ncol=6))
      setTable(TLData, name = "TLgraph")
      counter$j = isolate(counter$j) + 1
    }
    
    # Return list of table & graph data
    result = list(values[["TLgraph"]], values[["TLhot"]])
  })
  
  # Check if file has been uploaded
  observeEvent(input$TLfile, priority = 1,{
    infile   <- input$TLfile
    infileEQ <- input$EQfile
    component <- input$TLcomponent
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
      colnames(DF2) <- c("Raffinate", "Extract")
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
      # Set returned graph data to interpolated values
      TLData = interpolateTL(values, ranges, component, session)
      setTable(TLData, name = "TLgraph")
    }
  })
  
  # Check if graph button has been pressed
  observeEvent(input$TLgraph_button, priority = 1,{
    component <- input$TLcomponent
    # Check if table input exists
    if (!is.null(input$TLhot)) {
      # Set graph data to current table data
      DF2 = hot_to_r(input$TLhot)
      setTable(DF2, name = "TLhot")
      # Set returned graph data to interpolated values
      TLData = interpolateTL(values, ranges, component, session)
      setTable(TLData, name = "TLgraph")
    }
  })
  
  # Check if clear button has been pressed
  observeEvent(input$TLclear_button, priority = 1,{
    # Set table to default (0)
    DF2 = data.frame(matrix(0.0, nrow=4, ncol=2))
    colnames(DF2) <- c("Raffinate", "Extract")
    setTable(DF2, name = "TLhot")
    # Set returned graph data to default (0)
    TLData = data.frame(matrix(0.0, nrow=4, ncol=6))
    setTable(TLData, name = "TLgraph")
    # Close any alerts
    closeAlert(session, "TLalert")
    # Clear uploaded file
    session$sendCustomMessage(type = "resetFileInputHandler", 'TLfile')
  })
  
  # Check if switch button has been pressed
  observeEvent(input$switch_button, priority = 1,{
    toggle <- input$switch_button
    component <- input$TLcomponent
    DF = values[["EQhot"]]
    # Check if switch button has been pressed (initial value = 0)
    if((toggle%%2)!=0){
      # Switch raffinate with extract (based on initial ranges)
      updateSliderInput(session, "extract", min = 1, max = nrow(DF), value = c(1,nrow(DF)/2))
      updateSliderInput(session, "raffinate", min = 1, max = nrow(DF), value = c((nrow(DF)/2)+1,nrow(DF)))
      ranges$E <- seq(1,nrow(DF)/2)
      ranges$R <- seq((nrow(DF)/2)+1,nrow(DF))
    }
    else{
      updateSliderInput(session, "raffinate", min = 1, max = nrow(DF), value = c(1,nrow(DF)/2))
      updateSliderInput(session, "extract", min = 1, max = nrow(DF), value = c((nrow(DF)/2)+1,nrow(DF)))
      ranges$R <- seq(1,nrow(DF)/2)
      ranges$E <- seq((nrow(DF)/2)+1,nrow(DF))
    }
    # Set returned graph data to interpolated values
    TLData = interpolateTL(values, ranges, component, session)
    setTable(TLData, name = "TLgraph")
  })

  # Set additional graph data
  myData <- reactive({
    
    # Call reactive function for graph/clear button
    input$graph_button
    input$clear_button
    # Column names are dependent on equilibrium data
    input$EQclear_button
    input$EQfile
    
    # Initialize empty table during startup
    if(counter$k == 0){
      # Set table to default (0)
      DF3 = data.frame(matrix(0.0, nrow=4, ncol=4))
      DF3[4] = c("","","","")
      # Get header names from equlibrium data
      col_head = colnames(values[["EQhot"]])
      colnames(DF3) <- c(col_head[1], col_head[2], col_head[3], "Label")
      setTable(DF3, name = "hot")
      counter$k = isolate(counter$k) + 1
    }
    
    # Return data
    values[["hot"]]
  })
  
  # Check if graph button has been pressed
  observeEvent(input$graph_button, priority = 1,{
    # Check if table input exists
    if (!is.null(input$hot)) {
      # Set graph data to current table data
      DF3 = hot_to_r(input$hot)
      setTable(DF3, name = "hot")
    }
  })
  
  # Check if clear button has been pressed
  observeEvent(input$clear_button, priority = 1,{
    # Set table to default (0)
    DF3 = data.frame(matrix(0.0, nrow=4, ncol=4))
    DF3[4] = c("","","","")
    col_head = colnames(values[["EQhot"]])
    colnames(DF3) <- c(col_head[1], col_head[2], col_head[3], "Label")
    setTable(DF3, name = "hot")
  })
  
  # Generate ternary plot
  output$TernPlot <- renderSvgPanZoom({
    # Extract data from myTLData() as a data frame instead of a list value
    TLData <- as.data.frame(myTLData()[1])
    # Render ternary diagram
    gg <- plotit(myEQData, TLData, myData, toggle$hit)
    
    # Works with both renderPlot/plotOutput (mouse events) and renderSvgPanZoom/svgPanZoomOutput (native pan/zoom)
    # svgPanZoom(gg, controlIconsEnabled = TRUE)
    
    # Faster loading time - only works with renderSvgPanZoom/svgPanZoomOutput (native pan/zoom)
    svgPanZoom(svgPlot(show(gg), addInfo = F), panEnabled = FALSE, zoomEnabled = FALSE)
    
    # Non SVG plot - renderPlot/plotOutput (mouse events)
    # gg
  })
  
  # Render equilibrium data table
  output$EQhot <- renderRHandsontable({
    rhandsontable(myEQData(), stretchH = "all") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  # Render tie-line data table
  output$TLhot <- renderRHandsontable({
    # Extract data from myTLData() as a data frame instead of a list value
    TLData <- as.data.frame(myTLData()[2])
    rhandsontable(TLData, stretchH = "none") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  # Render data table
  output$hot <- renderRHandsontable({
    rhandsontable(myData(), stretchH = "none") %>%
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
