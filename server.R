library(ggtern)
library(gridSVG)
library(SVGAnnotation)
library(xlsx)
library(ggthemes)
library(R.utils)

# Load all .R files in functions folder
sapply(list.files(pattern="[.]R$", path="functions/", full.names=TRUE), source);

shinyServer(function(input, output, session) {
  
  # Counter used to initialize table at startup
  counter <- reactiveValues(i = 0, j = 0, k = 0, l = 0)
  # Used to distinguish raffinate/extract values
  ranges <- reactiveValues(E = seq(), R = seq())
  # Update table data structure
  values = list()
  setTable = function(x, name) values[[name]] <<- x
  # Make toggle values reactive
  toggle = reactiveValues(status = NULL, hit = 1, hitRT = 1, rowsEQ = 0)
  # Counter for adding points interactively 
  addcounter <- reactiveValues(point = 1, x = 1, y = 2)
  
  # Set equilibrium graph data
  myEQData <- reactive({
    
    # Call reactive function for data file, graph/clear button
    input$EQfile
    input$EQgraph_button
    input$EQclear_button
    input$submit_header_button
    input$clear_header_button
    
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
  observeEvent(input$EQfile,{
    infile <- input$EQfile
    # Check if data file is valid
    if (is.null(infile)){
      # Set table to default (0)
      DF = data.frame(matrix(0.0, nrow=10, ncol=3))
      setTable(DF, name = "EQhot")
    }
    else{
      if(infile$type=="csv" || infile$type=="text/csv" || infile$type=="text/comma-separated-values"){
        # Read uploaded CSV
        DF = read.csv(infile$datapath)
      }
      else if(infile$type=="tsv" || infile$type=="text/tsv" || infile$type=="text/tab-separated-values"){
        # Read uploaded TSV
        DF = read.delim(infile$datapath)
      }
      else if(infile$type=="xlsx" || infile$type=="xls" || infile$type=="application/vnd.ms-excel" || infile$type=="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"){
        # Read uploaded excel file (issues with xls files and Chrome browser)
        DF = read.xlsx(infile$datapath,1)
      }
      else if(infile$type=="text/plain"){
        separator = input$EQseparator
        DF <- tryCatch(
          {
            # Read uploaded text file
            read.table(infile$datapath, header = TRUE, sep = separator)
          },
          error=function(cond) {
            # Incorrect data format error message
            createAlert(session, "alert", "EQDataUnequalColumnAlert", content = "Error: Unequal Columns", append = FALSE)
            return(data.frame(matrix(0.0, nrow=10, ncol=3)))
            }
        )
        # If data has 1 column, assume incorrect uploading and let user choose delimiter
        if(ncol(DF)==1){
          toggleModal(session, "EQtextfile_box", toggle = "toggle")
          # Set table to default (0)
          DF = data.frame(matrix(0.0, nrow=10, ncol=3))
        }
        else if(ncol(DF)>3){
          # Set table to default (0)
          DF = data.frame(matrix(0.0, nrow=10, ncol=3))
          # Incorrect data format error message
          createAlert(session, "alert", "EQDataTrailAlert", content = "Error: Trailing Delimiter (End of Rows)", append = FALSE)
        }
      }
      else{
        # Set table to default (0)
        DF = data.frame(matrix(0.0, nrow=10, ncol=3))
      }
      # If only two data columns are given calculate third data column
      if(ncol(DF)==2){
        DF[3] = 1 - DF[2] - DF[1]
        # Set third column to X3
        colnames(DF)[3] <- "X3"
        # Incorrect data format error message
        createAlert(session, "alert", "EQcolumnAlert", content = "Warning: Incorrect Data Format (Missing 3rd Column)", append = FALSE)
      }
      # Assume incorrect data set if there aren't 3 columns
      else if(ncol(DF)!=3){
        # Set table to default (0)
        DF = data.frame(matrix(0.0, nrow=10, ncol=3))
        # Incorrect data format error message
        createAlert(session, "alert", "EQDataAlert", content = "Error: Incorrect Data Format", append = FALSE)
      }
      # Set table to uploaded data
      setTable(DF, name = "EQhot")
      # Update sliders based on number of equilibrium points (initial estimate)
      updateSliderInput(session, "raffinate", min = 1, max = nrow(DF), value = c(1,floor(nrow(DF)/2)))
      updateSliderInput(session, "extract", min = 1, max = nrow(DF), value = c(floor(nrow(DF)/2)+1,nrow(DF)))
      # Extract Column Headings
      col_head <- colnames(values[["EQhot"]])
      toggle$on <- TRUE
      toggle$rowsEQ <- nrow(values[["EQhot"]])
      # Update component names
      updateSelectInput(session, "TLcomponent", choices = col_head)
      # Set header for additional data (ternary)
      DF3 = values[["hot"]]
      if(!is.null(colnames(DF3))){
        colnames(DF3) <- c(col_head[1], col_head[2], col_head[3], "Label")
        setTable(DF3, name = "hot")
      }
      # Set header for additional data (right triangular)
      DF4 = values[["RThot"]]
      if(!is.null(colnames(DF4))){
        colnames(DF4) <- c(col_head[1], col_head[2], "Label")
        setTable(DF4, name = "RThot")
      }
      # Update component names (in link above equilibrium table)
      updateTextInput(session, "X1", value = col_head[1])
      updateTextInput(session, "X2", value = col_head[2])
      updateTextInput(session, "X3", value = col_head[3])
      # Update right triangle component choices
      updateSelectInput(session, "component1", choices = colnames(values[["EQhot"]]), selected = colnames(values[["EQhot"]])[1])
      updateSelectInput(session, "component2", choices = colnames(values[["EQhot"]]), selected = colnames(values[["EQhot"]])[2])
    }
  })
  
  # Check if graph button has been pressed
  observeEvent(input$EQgraph_button,{
    # Check if table input changes
    if (!is.null(input$EQhot)) {
      # Set graph data to current table data
      DF = hot_to_r(input$EQhot)
      setTable(DF, name = "EQhot")}
  })
  
  # Check if clear button has been pressed
  observeEvent(input$EQclear_button,{
    # Set table to default (0)
    DF = data.frame(matrix(0.0, nrow=10, ncol=3))
    setTable(DF, name = "EQhot")
    # Reset component input and sliders
    updateSelectInput(session, "TLcomponent", choices = colnames(values[["EQhot"]]))
    updateSliderInput(session, "raffinate", min = 1, max = nrow(DF), value = c(1,5))
    updateSliderInput(session, "extract", min = 1, max = nrow(DF), value = c(6,10))
    # Close any alerts
    closeAlert(session, "interpolateAlert")
    closeAlert(session, "sliderAlert")
    closeAlert(session, "EQfileAlert")
    closeAlert(session, "EQcolumnAlert")
    closeAlert(session, "EQDataAlert")
    closeAlert(session, "EQDataTrailAlert")
    closeAlert(session, "EQDataUnequalColumnAlert")
    # Clear uploaded file
    session$sendCustomMessage(type = "resetFileInputHandler", "EQfile")
    # Reset column names for additional data
    DF3 = values[["hot"]]
    col_head = colnames(DF)
    if(!is.null(colnames(DF3))){
      colnames(DF3) <- c(col_head[1], col_head[2], col_head[3], "Label")
      setTable(DF3, name = "hot")
    }
    # Update component names (in link above equilibrium table)
    updateTextInput(session, "X1", value = "")
    updateTextInput(session, "X2", value = "")
    updateTextInput(session, "X3", value = "")
    # Update right triangle component choices
    updateSelectInput(session, "component1", choices = colnames(values[["EQhot"]]), selected = colnames(values[["EQhot"]])[1])
    updateSelectInput(session, "component2", choices = colnames(values[["EQhot"]]), selected = colnames(values[["EQhot"]])[2])
  })
  
  # Modal box clear button
  observeEvent(input$clear_header_button,{
    # If additional data is null (hasn't been initialized) call myData() to initialize
    if(is.null(values[["hot"]])){
      myData()
    }
    # Clear text boxes
    updateTextInput(session, "X1", value = "")
    updateTextInput(session, "X2", value = "")
    updateTextInput(session, "X3", value = "")
    DF = values[["EQhot"]]
    DF3 = values[["hot"]]
    col_head <- c("X1", "X2", "X3")
    col_headL = c("X1", "X2", "X3", "Label")
    colnames(DF) <- col_head
    colnames(DF3) <- col_headL
    # Update global equilibrium data
    setTable(DF, name = "EQhot")
    setTable(DF3, name = "hot")
    # Update component names
    updateSelectInput(session, "TLcomponent", choices = col_head)
    updateSelectInput(session, "component1", choices = col_head, selected = col_head[1])
    updateSelectInput(session, "component2", choices = col_head, selected = col_head[2])
  })
  
  # Modal box submit button
  observeEvent(input$submit_header_button,{
    # If additional data is null (hasn't been initialized) call myData() to initialize
    if(is.null(values[["hot"]])){
      myData()
    }
    # Store inputs
    X1 <- input$X1
    X2 <- input$X2
    X3 <- input$X3
    # Store current data
    DF = values[["EQhot"]]
    DF3 = values[["hot"]]
    # Check if inputs are empty
    if(X1 == ""){
      X1 <- "X1" 
    }
    if(X2 == ""){
      X2 <- "X2" 
    }
    if(X3 == ""){
      X3 <- "X3" 
    }
    # Set header names for equilibrium data to inputs
    headernames = c(X1, X2, X3)
    headernamesL = c(X1, X2, X3, "Label")
    colnames(DF) <- headernames
    colnames(DF3) <- headernamesL
    # Update global equilibrium data
    setTable(DF, name = "EQhot")
    setTable(DF3, name = "hot")
    # Update component names
    updateSelectInput(session, "TLcomponent", choices = headernames)
    updateSelectInput(session, "component1", choices = headernames, selected = headernames[1])
    updateSelectInput(session, "component2", choices = headernames, selected = headernames[2])
  })
  
  # Set tie-line graph data
  myTLData <- reactive({
    
    # Call reactive function for data file, graph/clear button
    input$TLfile
    input$EQfile
    input$TLgraph_button
    input$TLclear_button
    input$setRanges_button
    input$submit_header_button
    input$clear_header_button
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
  observeEvent(input$TLfile,{
    infile   <- input$TLfile
    infileEQ <- input$EQfile
    component <- input$TLcomponent
    if(is.null(infileEQ)){
      # Equilibrium file error message
      createAlert(session, "alert", "EQfileAlert", content = "Error: Equilibrium Data Has Not Been Uploaded", append = FALSE)
      # Clear uploaded file
      session$sendCustomMessage(type = "resetFileInputHandler", "TLfile")
    }
    # Check if tie-line data file is valid
    else if ((is.null(infile))){
      # Set table to default (0)
      DF2 = data.frame(matrix(0.0, nrow=4, ncol=2))
      colnames(DF2) <- c("Raffinate", "Extract")
      setTable(DF2, name = "TLhot")
      # Set returned graph data to default (0)
      TLData = data.frame(matrix(0.0, nrow=4, ncol=6))
      setTable(TLData, name = "TLgraph")}
    else{
      if(infile$type=="csv" || infile$type=="text/csv" || infile$type=="text/comma-separated-values"){
        # Read uploaded CSV
        DF2 = read.csv(infile$datapath)
      }
      else if(infile$type=="tsv" || infile$type=="text/tsv" || infile$type=="text/tab-separated-values"){
        # Read uploaded TSV
        DF2 = read.delim(infile$datapath)
      }
      else if(infile$type=="xlsx" || infile$type=="xls" || infile$type=="application/vnd.ms-excel" || infile$type=="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"){
        # Read uploaded excel file (issues with xls files and Chrome browser)
        DF2 = read.xlsx(infile$datapath,1)
      }
      else if(infile$type=="text/plain"){
        separator = input$TLseparator
        DF2 <- tryCatch(
          {
            # Read uploaded text file
            read.table(infile$datapath, header = TRUE, sep = separator)
          },
          error=function(cond) {
            # Incorrect data format error message
            createAlert(session, "alert", "TLDataUnequalColumnAlert", content = "Error: Unequal Columns", append = FALSE)
            return(data.frame(matrix(0.0, nrow=4, ncol=2)))
          }
        )
        # If data has 1 column, assume incorrect uploading and let user choose delimiter
        if(ncol(DF2)==1){
          toggleModal(session, "TLtextfile_box", toggle = "toggle")
          # Set table to default (0)
          DF2 = data.frame(matrix(0.0, nrow=4, ncol=2))
          # Clear uploaded file
          session$sendCustomMessage(type = "resetFileInputHandler", "TLfile")
        }
        else if(ncol(DF2)>2){
          # Set table to default (0)
          DF2 = data.frame(matrix(0.0, nrow=4, ncol=2))
          # Incorrect data format error message
          createAlert(session, "alert", "TLDataTrailAlert", content = "Error: Trailing Delimiter (End of Rows)", append = FALSE)
        }
      }
      else{
        # Set table to default (0)
        DF2 = data.frame(matrix(0.0, nrow=4, ncol=2))
      }
      # Assume incorrect data set if there aren't three columns
      if(ncol(DF2)!=2){
        # Set table to default (0)
        DF2 = data.frame(matrix(0.0, nrow=4, ncol=2))        # Incorrect data format error message
        createAlert(session, "alert", "TLDataAlert", content = "Error: Incorrect Data Format", append = FALSE)
      }
      # Set table to uploaded data
      setTable(DF2, name = "TLhot")
      # Check if DF2 has same dimensions as default
      if(nrow(DF2)==4 && ncol(DF2)==2){
        # If DF2 is not the default data frame
        if(DF2 != data.frame(matrix(0.0, nrow=4, ncol=2))){
          # Set returned graph data to interpolated values
          TLData = interpolateTL(values, ranges, component, session)
          setTable(TLData, name = "TLgraph")
        }
      }
      else{
        # Set returned graph data to interpolated values
        TLData = interpolateTL(values, ranges, component, session)
        setTable(TLData, name = "TLgraph")
      }
    }
  })
  
  # Check if graph button has been pressed
  observeEvent(input$TLgraph_button,{
    component <- input$TLcomponent
    # Check if table input changes
    if (!is.null(input$TLhot)) {
      # Set graph data to current table data
      DF2 = hot_to_r(input$TLhot)
      setTable(DF2, name = "TLhot")
      # Set returned graph data to interpolated values
      TLData = interpolateTL(values, ranges, component, session)
      setTable(TLData, name = "TLgraph")
    }
    else{
      # Set returned graph data to interpolated values
      TLData = interpolateTL(values, ranges, component, session)
      setTable(TLData, name = "TLgraph")
    }
  })
  
  # Check if clear button has been pressed
  observeEvent(input$TLclear_button,{
    # Set table to default (0)
    DF2 = data.frame(matrix(0.0, nrow=4, ncol=2))
    colnames(DF2) <- c("Raffinate", "Extract")
    setTable(DF2, name = "TLhot")
    # Set returned graph data to default (0)
    TLData = data.frame(matrix(0.0, nrow=4, ncol=6))
    setTable(TLData, name = "TLgraph")
    # Close any alerts
    closeAlert(session, "interpolateAlert")
    closeAlert(session, "sliderAlert")
    closeAlert(session, "EQfileAlert")
    closeAlert(session, "TLcolumnAlert")
    closeAlert(session, "TLDataAlert")
    closeAlert(session, "TLDataTrailAlert")
    closeAlert(session, "TLDataUnequalColumnAlert")
    # Clear uploaded file
    session$sendCustomMessage(type = "resetFileInputHandler", "TLfile")
  })
  
  # Check if set button has been pressed
  observeEvent(input$setRanges_button,{
    Raf <- input$raffinate
    Ext <- input$extract
    component <- input$TLcomponent
    # Set reactive ranges
    ranges$R <- seq(Raf[1],Raf[2])
    ranges$E <- seq(Ext[1],Ext[2])
    # Set returned graph data to interpolated values
    TLData = interpolateTL(values, ranges, component, session)
    setTable(TLData, name = "TLgraph")
  })
  
  # Check if switch button has been pressed
  observeEvent(input$switchRanges_button,{
    component <- input$TLcomponent
    DF = values[["EQhot"]]
    # Store slider values and ranges as temporary values
    tempRaf = input$raffinate
    tempExt = input$extract
    tempR = ranges$R
    tempE = ranges$E
    # Update sliders
    updateSliderInput(session, "extract", min = 1, max = nrow(DF), value = tempRaf)
    updateSliderInput(session, "raffinate", min = 1, max = nrow(DF), value = tempExt)
    # Update ranges
    ranges$R <- tempE
    ranges$E <- tempR
  })
  
  # Check if raffinate slider has been changed
  observeEvent(input$raffinate,{
    Raf <- input$raffinate
    Ext <- input$extract
    # Check if raffinate has been set incorrectly (greater than max, overlapping)
    if(Raf[2]==Ext[2] || Raf[1]==Ext[1]){
      if(!is.null(values[["EQhot"]])){
        DF = values[["EQhot"]]
        # Update sliders based on number of equilibrium points (initial estimate)
        updateSliderInput(session, "raffinate", min = 1, max = nrow(DF), value = c(1,nrow(DF)/2))
        updateSliderInput(session, "extract", min = 1, max = nrow(DF), value = c((nrow(DF)/2)+1,nrow(DF)))
      }
      else{
        # Update sliders to default
        updateSliderInput(session, "raffinate", min = 1, max = 10, value = c(1,5))
        updateSliderInput(session, "extract", min = 1, max = 10, value = c(6,10))
      }
      # Slider error message
      createAlert(session, "alert", "sliderAlert", content = "Error: Incorrect Slider Setting", append = FALSE)
    }
    # Change extract based on how much user changes raffinate slider
    else if(Raf[2]>Ext[2]){
      maxvalue = Raf[2]
      Ext <- c(Ext[1],Raf[1]-1)
      # Update extract
      updateSliderInput(session, "extract", value = Ext)
      # Update extract range
      ranges$E <- seq(Ext[1],Ext[2])
    }
    else if(Raf[2]<Ext[2]){
      maxvalue = Ext[2]
      Ext <- c(Raf[2]+1,maxvalue)
      # Update extract
      updateSliderInput(session, "extract", value = Ext)
      # Update extract range
      ranges$E <- seq(Ext[1],Ext[2])
    }
  })
  
  # Check if extract slider has been changed
  observeEvent(input$extract,{
    Raf <- input$raffinate
    Ext <- input$extract
    # Check if extract has been set incorrectly (greater than max, overlapping)
    if(Ext[2]==Raf[2] || Ext[1]==Raf[1]){
      if(!is.null(values[["EQhot"]])){
        DF = values[["EQhot"]]
        # Update sliders based on number of equilibrium points (initial estimate)
        updateSliderInput(session, "raffinate", min = 1, max = nrow(DF), value = c(1,nrow(DF)/2))
        updateSliderInput(session, "extract", min = 1, max = nrow(DF), value = c((nrow(DF)/2)+1,nrow(DF)))
      }
      else{
        # Update sliders to default
        updateSliderInput(session, "raffinate", min = 1, max = 10, value = c(1,5))
        updateSliderInput(session, "extract", min = 1, max = 10, value = c(6,10))
      }
      # Slider Error Message
      createAlert(session, "alert", "sliderAlert", content = "Error: Incorrect Slider Setting", append = FALSE)
    }
    # Change raffinate based on how much user changes extract slider
    else if(Ext[2]>Raf[2]){
      maxvalue = Ext[2]
      Raf <- c(Raf[1],Ext[1]-1)
      # Update extract
      updateSliderInput(session, "raffinate", value = Raf)
      # Update extract range
      ranges$R <- seq(Raf[1],Raf[2])
    }
    else if(Ext[2]<Raf[2]){
      maxvalue = Raf[2]
      Raf <- c(Ext[2]+1,maxvalue)
      # Update extract
      updateSliderInput(session, "raffinate", value = Raf)
      # Update extract range
      ranges$R <- seq(Raf[1],Raf[2])
    }
  })

  # Set additional graph data
  myData <- reactive({
    
    # Call reactive function for graph/clear button
    input$graph_button
    input$clear_button
    # Column names are dependent on equilibrium data
    input$EQfile
    input$EQclear_button
    input$submit_header_button
    input$clear_header_button
    
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
  observeEvent(input$graph_button,{
    # Check if table input exists
    if (!is.null(input$hot)) {
      # Set graph data to current table data
      DF3 = hot_to_r(input$hot)
      setTable(DF3, name = "hot")
    }
  })
  
  # Check if clear button has been pressed
  observeEvent(input$clear_button,{
    # Set table to default (0)
    DF3 = data.frame(matrix(0.0, nrow=4, ncol=4))
    DF3[4] = c("","","","")
    col_head = colnames(values[["EQhot"]])
    colnames(DF3) <- c(col_head[1], col_head[2], col_head[3], "Label")
    setTable(DF3, name = "hot")
  })
  
  # Set additional graph data
  myRTData <- reactive({
    # Call reactive function for graph/clear button
    input$RTgraph_button
    input$RTclear_button
    # Column names are dependent on equilibrium data/component choice
    input$EQfile
    input$EQclear_button
    input$component1
    input$component2
    input$submit_header_button
    input$clear_header_button
    input$RTplot_dblclick
    
    # Initialize empty table during startup
    if(counter$l == 0){
      # Set table to default (0)
      DF4 = data.frame(matrix(0.0, nrow=4, ncol=3))
      DF4[3] = c("","","","")
      # Get header names from equlibrium data
      col_head = colnames(values[["EQhot"]])
      colnames(DF4) <- c(col_head[1], col_head[2], "Label")
      setTable(DF4, name = "RThot")
      counter$l = isolate(counter$l) + 1
    }
    
    # Return data
    values[["RThot"]]
  })
  
  # Check if graph button has been pressed
  observeEvent(input$RTgraph_button,{
    # Check if table input exists
    if (!is.null(input$RThot)) {
      # Set graph data to current table data
      DF4 = hot_to_r(input$RThot)
      setTable(DF4, name = "RThot")
    }
  })
  
  # Check if clear button has been pressed
  observeEvent(input$RTclear_button,{
    # Set table to default (0)
    DF4 = data.frame(matrix(0.0, nrow=4, ncol=3))
    DF4[3] = c("","","","")
    colnames(DF4) <- c(input$component1, input$component2, "Label")
    setTable(DF4, name = "RThot")
  })
  
  # Check if component1 has been changed
  observeEvent(input$component1,{
    # Set table to default (0)
    DF4 = values[["RThot"]]
    # Get header names from equlibrium data
    col_head = colnames(values[["EQhot"]])
    # Null at startup
    if(is.null(DF4)){
      # Set table to default (0)
      DF4 = data.frame(matrix(0.0, nrow=4, ncol=3))
      DF4[3] = c("","","","")
      colnames(DF4) <- c(col_head[1], col_head[2], "Label")
      setTable(DF4, name = "RThot")
    }
    colnames(DF4)[1] <- input$component1
    setTable(DF4, name = "RThot")
  })
  
  # Check if component2 has been changed
  observeEvent(input$component2,{
    # Set table to default (0)
    DF4 = values[["RThot"]]
    # Get header names from equlibrium data
    col_head = colnames(values[["EQhot"]])
    # Null at startup
    if(is.null(DF4)){
      # Set table to default (0)
      DF4 = data.frame(matrix(0.0, nrow=4, ncol=3))
      DF4[3] = c("","","","")
      colnames(DF4) <- c(col_head[1], col_head[2], "Label")
      setTable(DF4, name = "RThot")
    }
    colnames(DF4)[2] <- input$component2
    setTable(DF4, name = "RThot")
  })
  
  # Set theme for ternary
  myTheme <- reactive({
    size <- input$pointsize
    thickness <- input$linethickness
    theme <- input$overalltheme
    input$default_link
    # 1 = point size, 2 = tie-line thickness, 3 = overall theme
    return(list(size, thickness, theme))
  })
  
  # Set theme to defaults (ternary)
  observeEvent(input$default_link, {
    updateNumericInput(session, "pointsize", value = 1)
    updateNumericInput(session, "linethickness", value = 0.5)
    updateRadioButtons(session, "overalltheme", selected = "Gray")
  })
  
  # Generate ternary plot
  output$TernPlot <- renderSvgPanZoom({
    # Extract data from myTLData() as a data frame instead of a list value
    TLData <- as.data.frame(myTLData()[1])
    # Render ternary diagram
    gg <- plotit(myEQData(), TLData, myData(), toggle$hit, myTheme())
    # Works with both renderPlot/plotOutput (mouse events) and renderSvgPanZoom/svgPanZoomOutput (native pan/zoom)
    # svgPanZoom(gg, controlIconsEnabled = TRUE)
    
    # Faster loading time - only works with renderSvgPanZoom/svgPanZoomOutput (native pan/zoom)
    svgPanZoom(svgPlot(show(gg), addInfo = F), panEnabled = FALSE, zoomEnabled = FALSE, dblClickZoomEnabled = FALSE, mouseWheelZoomEnabled = FALSE)
    
    # Non SVG plot - renderPlot/plotOutput (mouse events)
#     gg
  })
  
  # Download ternary plot
  output$download <- downloadHandler(
    # Combine names of all components
    filename = function() {paste(paste(colnames(myEQData())[1], colnames(myEQData())[2], colnames(myEQData())[3], sep = "-"), ".pdf", sep="")},
    content = function(file) {
      # Generate graph
      TLData <- as.data.frame(myTLData()[1])
      gg <- plotit(myEQData(), TLData, myData(), toggle$hit, myTheme())
      # Save as pdf
      pdf(file)
      print(gg)
      dev.off()
    }
  )
  
  # Set theme for right triangular
  myRTTheme <- reactive({
    size <- input$RTpointsize
    thickness <- input$RTlinethickness
    theme <- input$RToveralltheme
    input$RTdefault_link
    # 1 = point size, 2 = tie-line thickness, 3 = overall theme
    return(list(size, thickness, theme))
  })
  
  # Set theme to defaults (right triangular)
  observeEvent(input$RTdefault_link, {
    updateNumericInput(session, "RTpointsize", value = 1.5)
    updateNumericInput(session, "RTlinethickness", value = 0.5)
    updateRadioButtons(session, "RToveralltheme", selected = "Gray")
  })
  
  # Generate right triangular plot
  output$RightPlot <- renderPlot({
    # Extract data from myTLData() as a data frame instead of a list value
    TLData <- as.data.frame(myTLData()[1])
    gg <- plotitRT(myEQData(), TLData, myRTData(), input$component1, input$component2, toggle$hitRT, session, myRTTheme(), 1)
    gg
  })

  # Download right triangular plot
  output$RTdownload <- downloadHandler(
    # Combine names of selected components
    filename = function() {paste(paste(input$component1, input$component2, sep = "-"), ".pdf", sep="")},
    content = function(file) {
      # Generate graph
      TLData <- as.data.frame(myTLData()[1])
      gg <- plotitRT(myEQData(), TLData, myRTData(), input$component1, input$component2, toggle$hitRT, session, myRTTheme(), 1)
      gg
      # Save as pdf
      pdf(file)
      print(gg)
      dev.off()
    }
  )
  
  # Generate right triangular plot (plotly)
  output$RightPlotly <- renderPlotly({
    # Extract data from myTLData() as a data frame instead of a list value
    TLData <- as.data.frame(myTLData()[1])
    # Point size in plotly requires different scale than R (3x)
    RTTheme <- myRTTheme()
    RTTheme[[1]] <- RTTheme[[1]]
    gg <- plotitRT(myEQData(), TLData, myRTData(), input$component1, input$component2, toggle$hitRT, session, RTTheme, 0.1)
    p <- ggplotly(gg)
    p
  })
  
  # Render equilibrium data table
  output$EQhot <- renderRHandsontable({
    rhandsontable(myEQData(), stretchH = "all") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  # Download equilibrium data
  output$EQData_download <- downloadHandler(
    # Combine names of all components
    filename = function() {paste(paste(colnames(myEQData())[1], colnames(myEQData())[2], colnames(myEQData())[3], sep = "-"), ".csv", sep="")},
    content = function(file) {
      write.csv(myEQData(), file, row.names = FALSE)
    }
  )
  
  # Render tie-line data table
  output$TLhot <- renderRHandsontable({
    # Extract data from myTLData() as a data frame instead of a list value
    TLData <- as.data.frame(myTLData()[2])
    rhandsontable(TLData, stretchH = "none") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  # Download tie-line data
  output$TLData_download <- downloadHandler(
    # Set name to selected component name
    filename = function() {paste(paste(input$TLcomponent, "Tie-Line", sep="_"), ".csv", sep="")},
    content = function(file) {
      write.csv(myTLData()[1], file, row.names = FALSE)
    }
  )
  
  # Render data table for additional points (ternary)
  output$hot <- renderRHandsontable({
    rhandsontable(myData(), stretchH = "none") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  # Render data table for additional points (right triangular)
  output$RThot <- renderRHandsontable({
    rhandsontable(myRTData(), stretchH = "none") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  observeEvent(input$RTplot_dblclick, {
    coord <- input$RTplot_dblclick
    DF4 <- values[["RThot"]]
    rows = nrow(DF4)
    # Allows user to still double click before toggle is enabled
    DF4[addcounter$point,addcounter$x] <- coord$x
    DF4[addcounter$point,addcounter$y] <- coord$y
    # Cycles through all points
    if(addcounter$point==rows){
      addcounter$point <- 1
    }
    else{addcounter$point <- addcounter$point + 1}
    setTable(DF4, name = "RThot")
  })
  
  observeEvent(input$sendmail,{
    sender <- input$sender
    message <- input$message
    
    # Check if email is valid
    if(isValidEmail(sender)==TRUE){

      # Send email and output alert
      js$doorbell(message, sender)
      createAlert(session, "emailalert", "emailsent", content = "Success! Response Time: 24-48 Hours", append = FALSE)
    }
    else{
      createAlert(session, "emailalert", "invalidemail", content = "Failure! Invalid Email Address", append = FALSE)
    }
  })
  
  # Render textbox
  output$textbox <- renderUI({
    HTML('<textarea id="message" rows="20" cols="" style="resize:none"></textarea>')
  })
  
  observeEvent(input$clearmail,{
    # Clear text inputs
    updateTextInput(session, "sender", value = "")
    # Clear message textbox
    output$textbox <- renderUI({
      HTML('<textarea id="message" rows="20" cols="" style="resize:none"></textarea>')
    })
    closeAlert(session, "emailsent")
    closeAlert(session, "invalidemail")
  })
  
  # Toggle options
  output$fileUploaded <- reactive({
    return(!is.null(toggle$on))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden=FALSE)
  observeEvent(input$axistog, {
    toggle$hit <- ((input$axistog[1]) %% 6 ) + 1
  })
  observeEvent(input$axistogRT, {
    toggle$hitRT <- ((input$axistogRT[1]) %% 2 ) + 1
    # Switch coordinate system for double click point addition
    switch(((input$axistogRT[1]) %% 2 ) + 1,
           {addcounter$x <- 1
           addcounter$y <- 2},
           {addcounter$x <- 2
           addcounter$y <- 1}
    )
  })
  
  # Download 8.11 equilibrium data
  output$EQ811_link <- downloadHandler(
    # Combine names of all components
    filename = function() {"Acetone-Water-TCE.csv"},
    content = function(file) {
      # R needs to read data file before writing to CSV
      write.csv(read.csv("./EQ_data/8_11EQ.csv"), file, row.names = FALSE)
    }
  )
  
  # Download 8.11 tie-line data
  output$TL811_link <- downloadHandler(
    # Combine names of all components
    filename = function() {"Acetone_Tie-Line.csv"},
    content = function(file) {
      # R needs to read data file before writing to CSV
      write.csv(read.csv("./EQ_data/8_11TL.csv"), file, row.names = FALSE)
    }
  )
  
  # Download 8.14 equilibrium data
  output$EQ814_link <- downloadHandler(
    # Combine names of all components
    filename = function() {"TMA-Water-Benzene.csv"},
    content = function(file) {
      # R needs to read data file before writing to CSV
      write.csv(read.csv("./EQ_data/8_14EQ.csv"), file, row.names = FALSE)
    }
  )
  
  # Download 8.14 tie-line data
  output$TL814_link <- downloadHandler(
    # Combine names of all components
    filename = function() {"TMA_Tie-Line.csv"},
    content = function(file) {
      # R needs to read data file before writing to CSV
      write.csv(read.csv("./EQ_data/8_14TL.csv"), file, row.names = FALSE)
    }
  )
  
  # Download 8.15 equilibrium data
  output$EQ815_link <- downloadHandler(
    # Combine names of all components
    filename = function() {"Docosane-DPH-Furfural"},
    content = function(file) {
      # R needs to read data file before writing to CSV
      write.csv(read.csv("./EQ_data/8_15EQ.csv"), file, row.names = FALSE)
    }
  )
  
  # Download 8.15 tie-line data
  output$TL815_link <- downloadHandler(
    # Combine names of all components
    filename = function() {"Docosane_Tie-Line.csv"},
    content = function(file) {
      # R needs to read data file before writing to CSV
      write.csv(read.csv("./EQ_data/8_15TL.csv"), file, row.names = FALSE)
    }
  )

})
