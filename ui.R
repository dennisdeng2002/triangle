library(shinydashboard)
library(rhandsontable)
library(tools)
library(shinyBS)
library(svgPanZoom)
library(plotly)

shinyUI(
  dashboardPage(
    title = "Ternary Diagram Generator",
    dashboardHeader(title = "Liquid-Liquid Extraction"),
    # Sidebar
    dashboardSidebar(
      sidebarMenu(
        menuItem("LLE Data", tabName = "data", icon = icon("table")),
        menuItem("Ternary Plot", tabName = "Tplot", icon = icon("area-chart")),
        menuItem("Right Triangular Plot", tabName = "RTplot", icon = icon("area-chart")),
        menuItem("User Guide", tabName = "guide", icon = icon("book"),
                 menuSubItem("General Procedure", tabName = "general", icon = icon("angle-right")),
                 menuSubItem("Details/Features", tabName = "details", icon = icon("angle-right")),
                 menuSubItem("Datasets", tabName = "datasets", icon = icon("angle-right"))
                 ),
        menuItem("Source Code", tabName = "code", icon = icon("file-text-o"), 
                 menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                 menuSubItem("server.R", tabName = "server", icon = icon("angle-right")),
                 menuSubItem("functions", tabName = "functions", icon = icon("angle-right"))
                 ),
        menuItem("Contact Info", tabName = "info", icon = icon("info"))
      )),
    # Generate plot
    dashboardBody(
      tags$head(includeScript("google-analytics.js")),
      # Alter header font
      tags$head(tags$style(HTML('.main-header .logo {font-size: 18px;}'))),
      tabItems(
        tabItem(tabName = "data",
                tags$script('
                    Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {   
                    var el = $("#" + x);
                    el.replaceWith(el = el.clone(true));
                    var id = "#" + x + "_progress";     
                    $(id).css("visibility", "hidden");
                        });
                      '),
                fluidRow(
                  bsAlert("alert"),
                  box(
                    title = "Equilibrium Data", status = "primary", solidHeader = TRUE,
                    bsModal("add_headerbox", "Component Names", trigger = "add_header_link",
                            textInput(inputId="X1", label="X1", value="", width="50%"),
                            textInput(inputId="X2", label="X2", value="", width="50%"),
                            textInput(inputId="X3", label="X3", value="", width="50%"),
                            actionButton("submit_header_button", "Submit"),
                            actionButton("clear_header_button", "Clear"),
                            size = "small"
                    ),
                    div(style="width: 50%; margin: 0 auto;",
                    # Upload CSV file of equilibrium data
                    fileInput("EQfile", "Upload Equilibrium Data:", accept=c(
                      'text/csv',
                      'text/comma-separated-values',
                      'text/tab-separated-values',
                      'text/plain',
                      'csv',
                      'tsv',
                      'application/vnd.ms-excel',
                      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                      '.xls',
                      '.xlsx'
                    )),
                    # Set delimiter for equilibrium data (only triggered when user uploads text file)
                    bsModal("EQtextfile_box", "Error: Incorrect Delimiter", trigger = NULL,
                            radioButtons("EQseparator", label = NULL, 
                                         choices = c("Space" = " ",
                                                     "Comma" = ",",
                                                     "Semicolon" = ";",
                                                     "Tab" = "    "
                                                     ),
                                         selected = " "),
                            size = "small"
                    ),
                    # Used to open header modal
                    actionLink("add_header_link", "Change Component Names"),
                    # Generate equilibrium data table
                    rHandsontableOutput("EQhot"),
                    br(),
                    # Graph data table changes
                    actionButton("EQgraph_button","Graph"),
                    # Clear table/plot
                    actionButton("EQclear_button","Clear"),
                    # Download data file
                    downloadButton("EQData_download", label = NULL, class = NULL))),
                  
                  box(
                    title = "Tie-Line Data", status = "primary", solidHeader = TRUE,
                    div(style="width: 50%; margin: 0 auto;",
                    # Upload CSV file of tie-line data
                    fileInput("TLfile", "Upload Tie-Line Data:", accept=c(
                      'text/csv',
                      'text/comma-separated-values',
                      'text/tab-separated-values',
                      'text/plain',
                      'csv',
                      'tsv',
                      'application/vnd.ms-excel',
                      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                      '.xls',
                      '.xlsx'
                    )),
                    # Set delimiter for equilibrium data (only triggered when user uploads text file)
                    bsModal("TLtextfile_box", "Error: Incorrect Delimiter", trigger = NULL,
                            radioButtons("TLseparator", label = NULL, 
                                         choices = c("Space" = " ",
                                                     "Comma" = ",",
                                                     "Semicolon" = ";",
                                                     "Tab" = "    "
                                         ),
                                         selected = " "),
                            size = "small"
                    ),
                    selectInput("TLcomponent", "Component", choices = c("X1", "X2", "X3"),
                                selected = "X1", width = "150px"),
                    # Generate tie-line data table
                    rHandsontableOutput("TLhot"),
                    br(),
                    # Graph data table changes
                    actionButton("TLgraph_button","Graph"),
                    # Clear table/plot
                    actionButton("TLclear_button","Clear"),
                    # Download data file
                    downloadButton("TLData_download", label = NULL, class = NULL)),
                    width = 5),
                  
                  box(
                    title = "Raffinate/Extract Ranges", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    sliderInput("raffinate", label = "Raffinate", min = 1, max = 10, value = c(1,5)),
                    sliderInput("extract", label = "Extract", min = 1, max = 10, value = c(6,10)),
                    div(style="width: 50%; margin: 0 auto;",
                    actionButton("setRanges_button", "Set Ranges"),
                    bsButton("switchRanges_button", "Switch", type = "action", block = FALSE,
                             icon = icon("arrows-v"))), width = 5)
                )
        ),
        tabItem(tabName = "Tplot",
                fluidRow(
                  box(
                    div(style="width: 50%; margin: 0 auto;",
                    svgPanZoomOutput("TernPlot", height = "500px", width = "500px")),
                    # Toggle
                    conditionalPanel("output.fileUploaded", actionLink("axistog", "Toggle Axis Display")),
                    bsModal("add_box", "Additional Data Points", trigger = "tern_add_button",
                            # Generate data table
                            rHandsontableOutput("hot"),
                            br(),
                            actionButton("graph_button", "Graph"),
                            actionButton("clear_button", "Clear"),
                            size = "small"
                            ),
                    bsModal("tern_theme_box", "Graph Elements", trigger = "tern_theme_button",
                            # Resize numericInput boxes
                            tags$head(tags$style(HTML("input[type=\"number\"] {width: 125px;}"))),
                            numericInput("pointsize", "Point Size", value = .5, min = 0.1, max = 1.5, step = 0.1),
                            numericInput("linethickness", "Tie-Line Thickness", value = 0.5, min = 0.1, max = 1, step = 0.1),
                            radioButtons("overalltheme", "Overall Theme", choices = c("Gray" = "Gray", "B/W" = "B/W", "RGBG" = "RGBG", "RGBW" = "RGBW"),
                                         selected = "Gray", width = "125px"),
                            actionLink("default_link", 'Defaults'),
                            size = "small"
                    ),
                    bsButton("tern_add_button", label = NULL, icon = icon("plus")),
                    bsButton("tern_theme_button", label = NULL, icon = icon("cog", lib = "glyphicon")),
                    downloadButton("download", label = NULL, class = NULL),
                    width = 12)
                )
              ),
        tabItem(tabName = "RTplot",
                fluidRow(
                  box(
                    div(style="width: 50%; margin: 0 auto;",
                        plotOutput("RightPlot", height = "500px", width = "500px", dblclick = "RTplot_dblclick")),
                    # Toggle
                    conditionalPanel("output.fileUploaded", actionLink("axistogRT", "Toggle Axis Display")),
                    bsModal("right_add_box", "Additional Data Points", trigger = "right_add_button",
                            p("Double Click Plot to Add Points"),
                            # Generate data table
                            rHandsontableOutput("RThot"),
                            br(),
                            actionButton("RTgraph_button", "Graph"),
                            actionButton("RTclear_button", "Clear"),
                            size = "small"
                    ),
                    bsModal("select_box", "Components", trigger = "select_button",
                            selectInput("component1", "X Axis", choices = c("X1", "X2", "X3"), selected = "X1", width = "150px"),
                            selectInput("component2", "Y Axis", choices = c("X1", "X2", "X3"), selected = "X2", width = "150px"),
                            size = "small"
                    ),
                    bsModal("right_theme_box", "Graph Elements", trigger = "right_theme_button",
                            # Resize numericInput boxes
                            tags$head(tags$style(HTML("input[type=\"number\"] {width: 125px;}"))),
                            numericInput("RTpointsize", "Point Size", value = 1.5, min = 0.5, max = 3, step = 0.25),
                            numericInput("RTlinethickness", "Tie-Line Thickness", value = 0.5, min = 0.1, max = 1, step = 0.1),
                            selectInput("RToveralltheme", "Overall Theme",
                                        choices = c("Gray" = "Gray", "B/W" = "B/W", "Calc" = "Calc",
                                                    "Economist" = "Economist", "Excel" = "Excel",
                                                    "Few" = "Few", "Google Docs" = "Google Docs",
                                                    "Highcharts" = "Highcharts", "Pander" = "Pander",
                                                    "Solarized" = "Solarized", "Stata" = "Stata", 
                                                    "Tufte" = "Tufte", "WSJ" = "WSJ"),
                                        selected = "Gray", width = "125px"),
                            actionLink("RTdefault_link", 'Defaults'),
                            size = "small"
                    ),
                    bsModal("plotly_box", title = NULL, trigger = "plotly_button",
                            plotlyOutput("RightPlotly"),
                            size = "large"
                    ),
                    bsButton("select_button", label = NULL, icon = icon("list", lib = "glyphicon")),
                    bsButton("right_add_button", label = NULL, icon = icon("plus")),
                    bsButton("right_theme_button", label = NULL, icon = icon("cog", lib = "glyphicon")),
                    downloadButton("RTdownload", label = NULL, class = NULL),
                    bsButton("plotly_button", label = NULL, icon = icon("meh-o")),
                    width = 12)
                )
        ),
        tabItem(tabName = "general",
                fluidRow(
                  box(title = "General Procedure", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,
                      tags$ul(
                        tags$li("Enter or upload equilbrium data"),
                        img(src = "1a.png", height = 300, width = 500),
                        img(src = "1b.png", height = 300, width = 300)
                      ),
                      br(),
                      tags$ul(
                        tags$li("Select component, then enter/upload tie-line data"),
                        img(src = "2a.png", height = 300, width = 300),
                        img(src = "2b.png", height = 300, width = 300)
                      ),
                      br(),
                      tags$ul(
                        tags$li("If necessary, alter extract/raffinate ranges (press set ranges to update graph)"),
                        img(src = "3a.png", height = 200, width = 275),
                        img(src = "3b.png", height = 200, width = 275)
                      ),
                      br(),
                      tags$ul(
                        tags$li("View ternary plot (options: toggle axis, add points, change theme, download)"),
                        img(src = "4.png", height = 600, width = 1000)
                      ),
                      br(),
                      tags$ul(
                        tags$li("View right triangular plot (options: toggle axis, choose components, add points, change theme, download, plot.ly)"),
                        img(src = "5.png", height = 600, width = 1000)
                      ),
                    width = 12)
                )
        ),
        tabItem(tabName = "details",
                fluidRow(
                  box(title = "Equilibrium Data", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,
                      tags$ul(
                        tags$li("Accepted file types include .csv, .tsv, .txt, .xls (doesn't work in Chrome), and .xlsx (rtf files must be converted to plain text)"),
                        tags$li("Table options can be accessed by right clicking a cell"),
                        tags$li("First row of uploaded data must be your component names"),
                        tags$li("For .txt files the default delimiter is a space (make sure there aren't any hanging spaces at the end of rows)"),
                        tags$li("Commas, semicolons, and tabs are also accepted as delimiters, and you will be prompted to select the delimiter and reupload the data"),
                        tags$li("If only two data columns are given the third will be calculated (summation = 1)")
                      ),
                      width = 12),                  
                  box(title = "Tie-Line Data", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,
                      tags$ul(
                        tags$li("Uploading or entering tie-line data without equilibrium data will result in an error."),
                        tags$li("Tie-line data should be entered using a single component for both extract and raffinate"),
                        tags$li("Actual data points (all three coordinates) will be calculated using a linear interpolation method"),
                        tags$li("Errors in interpolation generally occur due to incorrectly set ranges (next section), and an alert will be prompted if interpolated values are zero or negative"),
                        tags$li("Graph button is used to commit any changes in tie-line data or component choice and update plots")
                      ),
                      width = 12),
                  box(title = "Ranges", status = "primary", solidHeader = TRUE,
                      tags$ul(
                        tags$li("Initial ranges are automatically calculated (equilibrium data is assumed symmetrical)"),
                        tags$li("Plots will only update when the set ranges button is pressed (pressing switch button therefore will not update plots)"),
                        tags$li("Ranges are programmed to sum up to the number of rows given in the equilibrium data")
                      ),
                      collapsible = TRUE,
                      
                      width = 12),
                  box(title = "Ternary Plot", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,
                      tags$ul(
                        tags$li("Toggle Axis: only enabled once equilibrium data is uploaded"),
                        tags$li("Add Option: additional points can be added with their corresponding labels by entering data and pressing graph"),
                        tags$li("Graph Elements: point size, line thickness, and overall theme are customizable"),
                        tags$li("Download: save plot as pdf (preserves overall quality)")
                      ),
                      width = 12),
                  box(title = "Right Triangular Plot", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,
                      tags$ul(
                        tags$li("Toggle Axis: only enabled once equilibrium data is uploaded"),
                        tags$li("Choose Components: allows you to choose what components to plot on the x and y axis"),
                        tags$li("Add Option: additional points can be added either by entering data manually or double clicking the plot itself (cycles through points)"),
                        tags$li("Graph Elements: point size, line thickness, and overall theme are customizable"),
                        tags$li("Download: save plot as pdf (preserves overall quality)"),
                        tags$li("Plot.ly: open plot using plot.ly API")
                      ),
                      width = 12)
                )
        ),
        tabItem(tabName = "datasets",
                fluidRow(
                  box(title = "Seader 8.11 - Acetone, Water, TCE", status = "primary", solidHeader = TRUE,
                      tags$ul(
                        tags$li(downloadLink("EQ811_link", "Equilibrium Data")),
                        tags$li(downloadLink("TL811_link", "Tie-Line Data"))
                      ), width = 4),
                  box(title = "Seader 8.14 - TMA, Water, Benzene", status = "primary", solidHeader = TRUE,
                      tags$ul(
                        tags$li(downloadLink("EQ814_link", "Equilibrium Data")),
                        tags$li(downloadLink("TL814_link", "Tie-Line Data"))
                      ), width = 4),
                  box(title = "Seader 8.15 - Docosane, DPH, Furfural", status = "primary", solidHeader = TRUE,
                      tags$ul(
                        tags$li(downloadLink("EQ815_link", "Equilibrium Data (45 °C)")),
                        tags$li(downloadLink("TL815_link", "Tie-Line Data (45 °C)"))
                      ), width = 4)
                )),
        tabItem(tabName = "ui",
                fluidRow(
                  box(
                      pre(includeText("ui.R")),
                      width = 12)
                )
              ),
        tabItem(tabName = "server",
                fluidRow(
                  box(
                      pre(includeText("server.R")),
                      width = 12)
                )
              ),
        tabItem(tabName = "functions",
                fluidRow(
                  box(
                      pre(includeText("functions/checkifdecimals.R")),
                      pre(includeText("functions/interpolate.R")),
                      pre(includeText("functions/interpolateTL.R")),
                      pre(includeText("functions/isValidEmail.R")),
                      pre(includeText("functions/normalize.R")),
                      pre(includeText("functions/plotit.R")),
                      pre(includeText("functions/plotitRT.R")),
                      pre(includeText("functions/sortDecreasing.R")),
                      width = 12)
                )
              ),
        tabItem(tabName = "info",
                bsAlert("emailalert"),
                fluidRow(
                  box(title = "Email", status = "primary", solidHeader = TRUE,
                    textInput("sender", "Email Address:", value = "", width = "500px"),
                    textInput("subject", "Subject:", value = "", width = "500px"),
                    tags$style(type="text/css", "textarea {width:100%}"),
                    uiOutput("textbox"),
                    bsButton("sendmail", "Send", block = TRUE, icon = icon("send")),
                    bsButton("clearmail", "Clear", block = TRUE, icon = icon("trash")),
                    width = 6),
                  box(title = "About Us", status = "primary", solidHeader = TRUE,
                    p("This website was developed using Shiny,
                      \"an elegant and powerful web framework for building web applications using R.\"
                      It is intended to help chemical engineering students generate ternary equilibrium diagrams,
                      and is not intended for commercial use. Feel free to contact us with any feedback or questions."),
                    br(),
                    h5("Dennis Deng"),
                    h5("dennisdeng2002@yahoo.com"),
                    br(),
                    h5("Ike Okoro"),
                    h5("iokoro@umd.edu"),
                    br(),
                    h5("Kai Chen"),
                    h5("kchen128@terpmail.umd.edu"),
                    br(),
                    h5("Mark Vujnovich"),
                    h5("mark.vujnovich@gmail.com"),
                    br(),
                    h5(a("Github Page", href = "https://github.com/dennisdeng2002/triangle", target = "_blank")),
                    width = 6)
                )
              )
            )
          )
    ))