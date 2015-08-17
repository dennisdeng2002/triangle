library(shiny)
library(ggtern)
library(gridSVG)
library(svgPanZoom)
library(SVGAnnotation)
library(rhandsontable)
library(tools)

shinyServer(function(input, output, session) {
  
  # Used to resize graph (zoom)
  zoomranges <- reactiveValues(x = NULL, y = NULL)
  # Counter used to initialize table at startup
  counter <- reactiveValues(i = 0, j = 0, k = 0)
  # Used to distinguish raffinate/extract values
  ranges <- reactiveValues(E = seq(), R = seq())
  # Update table data structure
  values = list()
  setTable = function(x, name) values[[name]] <<- x
  
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
        # Set table to default (0)
        DF = data.frame(matrix(0.0, nrow=10, ncol=3))
        setTable(DF, name = "EQhot")}
      else{
      # Set table to uploaded data
      DF = read.csv(infile$datapath)
      setTable(DF, name = "EQhot")
      # Update sliders based on number of equilibrium points (initial estimate)
      # Data sets with odd numbers will require rounding (not yet implemented)
      updateSliderInput(session, "raffinate", min = 1, max = nrow(DF), value = c(1,nrow(DF)/2))
      updateSliderInput(session, "extract", min = 1, max = nrow(DF), value = c((nrow(DF)/2)+1,nrow(DF)))
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
      DF = data.frame(matrix(0.0, nrow=10, ncol=3))
      setTable(DF, name = "EQhot")
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
    # Call reactive function for slider inputs, set ranges for raffinate and extract
    Raf <- input$raffinate
    Ext <- input$extract
    # Set reactive ranges
    ranges$R <- seq(Raf[1],Raf[2])
    ranges$E <- seq(Ext[1],Ext[2])
    # Initialize empty table during startup
    if(counter$j == 0){
      # Set table to default (0)
      DF = data.frame(matrix(0.0, nrow=4, ncol=2))
      setTable(DF, name = "TLhot")
      # Set returned graph data to default (0)
      TLData = data.frame(matrix(0.0, nrow=4, ncol=6))
      setTable(TLData, name = "TLgraph")
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
        DF = data.frame(matrix(0.0, nrow=4, ncol=2))
        setTable(DF, name = "TLhot")
        # Set returned graph data to default (0)
        TLData = data.frame(matrix(0.0, nrow=4, ncol=6))
        setTable(TLData, name = "TLgraph")}
      else{
        # Set table to uploaded data
        DF = read.csv(infile$datapath)
        setTable(DF, name = "TLhot")
        # Set returned graph data to interpolated values
        TLData = interpolateTL()
        setTable(TLData, name = "TLgraph")
        }
    })
    
    # Check if graph button has been pressed
    observeEvent(input$graph_button, priority = 1,{
      # Check if table input exists
      if (!is.null(input$TLhot)) {
        # Set graph data to current table data
        DF = hot_to_r(input$TLhot)
        setTable(DF, name = "TLhot")
        # Set returned graph data to default (0)
        TLData = interpolateTL()
        setTable(TLData, name = "TLgraph")
        }
    })
    
    # Check if clear button has been pressed
    observeEvent(input$clear_button, priority = 1,{
      # Set table to default (0)
      DF = data.frame(matrix(0.0, nrow=4, ncol=2))
      setTable(DF, name = "TLhot")
      # Set returned graph data to default (0)
      TLData = data.frame(matrix(0.0, nrow=4, ncol=6))
      setTable(TLData, name = "TLgraph")
    })
    
    # Return list of table & graph data
    result = list(values[["TLgraph"]], values[["TLhot"]])
  })
  
#   # Manually add point to graph
#   myData <- reactive({
# 
#     # Call reactive function for clear button
#     input$clear_button
#     
#     # Initialize empty table during startup
#     if(counter$k == 0){
#       # Set table to default (0)
#       DF = data.frame(matrix(0.0, nrow=1, ncol=3))
#       sethot(DF)
#       counter$k = isolate(counter$k) + 1
#     }
#     
#     # Call reactive function for double click - store as variable A
#     A <- input$plot_dblclick
#     # Check if double click occurs within plot (non-zero vector)
#     if(length(A)!=0){
#       x1 = sqrt(3)/2*(1-A$x-A$y)
#       x2 = 0.5*(1+A$x-A$y)
#       x3 = 1-x1-x2
#       DF=data.frame(x1,x2,x3)
#       sethot(DF)
#     }
#     # Check if clear button has been pressed
#     observeEvent(input$clear_button, priority = 1,{
#     })
#     
#     # Return data point
#     values[["hot"]]
#   })

  # Generate ternary plot
  output$TernPlot <- renderSvgPanZoom({
    # Extract data from myTLData() as a data frame instead of a list value
    TLData <- as.data.frame(myTLData()[1])
    # Render ternary diagram
    gg <- ggtern(data = myEQData(), aes_string(x=colnames(myEQData())[1], y=colnames(myEQData())[2], z=colnames(myEQData())[3])) +
    geom_point() + theme() +
    # Render tie-lines
    geom_segment(data = TLData, aes_string(x=colnames(TLData)[1],y=colnames(TLData)[2],z=colnames(TLData)[3],xend=colnames(TLData)[4],yend=colnames(TLData)[5],zend=colnames(TLData)[6]))
#     # Add point
#     geom_point(data = myData(), aes_string(x=colnames(myData())[1], y=colnames(myData())[2], z=colnames(myData())[3])) +
# #     Used to resize graph during zoom
#     coord_tern(T = getOption("tern.default.T"), L = getOption("tern.default.L"),
#                R = getOption("tern.default.R"), xlim = zoomranges$x, ylim = zoomranges$y,
#                Tlim = NULL, Llim = NULL, Rlim = NULL, clockwise)
    
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
  
  interpolateTL <- function(){
    # Normalize tie-line data
    TLData = normalize(values[["TLhot"]])
    # Sort equilibrium data based on raffinate/extract side
    EQData = sortDecreasing(values[["EQhot"]])
    # For loop counter
    rows = seq(1,nrow(TLData))
    # Initialize placeholder matrix
    interTLData = data.frame(matrix(0.0, nrow = nrow(TLData), ncol = 2*ncol(EQData)))
    for(i in rows){ 
      interTLData[i,1] = TLData[i,1]
      interTLData[i,2] = interpolate(TLData[i,1], range = ranges$R, EQData)[1]
      interTLData[i,3] = interpolate(TLData[i,1], range = ranges$R, EQData)[2]
      interTLData[i,4] = TLData[i,2]
      interTLData[i,5] = interpolate(TLData[i,2], range = ranges$E, EQData)[1]
      interTLData[i,6] = interpolate(TLData[i,2], range = ranges$E, EQData)[2]
    }
    interTLData
  }
  
  interpolate <- function(x, range, myData){
    # Set range to 1 minus inputted range
    range = seq(range[1],range[length(range)-1])
    # Generic form for a linear equation
    first_order <- function(newdist, model) {
      coefs <- coef(model)
      #y = b + mx
      res <- coefs[1] + (coefs[2] * newdist)
      return(res)
    }
    # Example: x = Acetone, y = Water, z = TCE
    for(i in range){
      # Check whether tie-line data = equilibrium data
      if(myData[i,1] == x){
        # Modeling water as a function of acetone
        y <- myData[i,2]
        z <- myData[i,3]
        break
      } 
      # Check whether tie-line data falls between equilibrium data
      else if(myData[i,1] > x & x > myData[i+1,1]){
        # Modeling water as a function of acetone
        fit <- lm(myData[(i):(i+1),2]~myData[(i):(i+1),1])
        # Calculate theoretical water and TCE values
        y <- first_order(x, fit)
        z <- 1-y-x
        break
      } 
      else{
      }
    }
    # Return both y and z
    yz = c(y,z)
    return(yz)
  }
  
  sortDecreasing <- function(myData){
    myData = normalize(myData)
    columns = seq(1,ncol(myData))
    tempR = myData[ranges$R,]
    row.names(tempR) <- 1:nrow(tempR)
    tempR = tempR[order(tempR[,1], decreasing = TRUE),]
    tempE = myData[ranges$E,]
    row.names(tempE) <- 1:nrow(tempE)
    tempE = tempE[order(tempE[,1], decreasing = TRUE),]
    if(ranges$R[1]==1){
      myData = rbind(tempR, tempE)
    }
    else if(ranges$E[1]==1){
      myData = rbind(tempE, tempR)  
    }
    row.names(myData) <- 1:nrow(myData)
    myData
  }
  
  normalize <- function(myData){
    rows = seq(1,nrow(myData))
    columns = seq(1,ncol(myData))
    counter = 0
    # Loop used to determine whether values are given as strictly decimals or whole numbers (which can also contain decimals)
    for(i in rows){
      for(j in columns){
        if(myData[i,j] > 1){
          counter = counter + 1
          # Assume that if more than 4 (arbitrary) values are > 1, then all values need to be rounded
          if(counter > 4){
            break
          }
        }
      }
    }
    # Normalize to 1 if values aren't given as decimals
    if(counter > 4){
      for(i in rows){
        for(j in columns){
            myData[i,j] = myData[i,j] / 100
        }
      }
    }
    myData
  }
  
#   # Zoom based on slider input
#   observeEvent(input$zoom_slider, {
#       
#     zoom <- input$zoom_slider
#     if (!is.null(zoom)) {
#       zoomranges$x <- c(0, zoom)
#       zoomranges$y <- c(0, zoom)
#         
#     } else {
#       zoomranges$x <- NULL
#       zoomranges$y <- NULL
#     }
#     print(zoomranges$x)
#     print(zoomranges$y)
#   })
#   
#   # Output data for click, double-click, hover, and brush
#   output$info <- renderText({
#     
#     xy_str <- function(e) {
#       if(is.null(e)) return("NULL\n")
#       paste0("x=", round(e$x, 2), " y=", round(e$y, 2), "\n")
#     }
#     
#     paste0(
#       "click: ", xy_str(input$plot_click),
#       "dblclick: ", xy_str(input$plot_dblclick),
#       "hover: ", xy_str(input$plot_hover)
#     )
#   })
  
})
