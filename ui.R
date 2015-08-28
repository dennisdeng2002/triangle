library(shinydashboard)
library(rhandsontable)
library(tools)
library(shinyBS)
library(svgPanZoom)
library(plotly)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Liquid-Liquid Extraction"),
    # Sidebar
    dashboardSidebar(
      sidebarMenu(
        menuItem("LLE Data", tabName = "data", icon = icon("table")),
        menuItem("Ternary Plot", tabName = "Tplot", icon = icon("area-chart")),
        menuItem("Right Triangular Plot", tabName = "RTplot", icon = icon("area-chart")),
        menuItem("User Guide", tabName = "guide", icon = icon("book")),
        menuItem("Source Code", tabName = "code", icon = icon("file-text-o"), 
                 menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                 menuSubItem("server.R", tabName = "server", icon = icon("angle-right")),
                 menuSubItem("functions", tabName = "functions", icon = icon("angle-right"))
                 ),
        menuItem("Contact Info", tabName = "info", icon = icon("info"))
      )),
    # Generate plot
    dashboardBody(
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
                    collapsible = TRUE,
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
                    collapsible = TRUE,
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
                            numericInput("pointsize", "Point Size", value = 1.5, min = 0.5, max = 3, step = 0.25),
                            numericInput("linethickness", "Tie-Line Thickness", value = 0.5, min = 0.1, max = 1, step = 0.1),
                            radioButtons("overalltheme", "Overall Theme", choices = c("Gray" = "Gray", "B/W" = "B/W", "RGB" = "RGB"),
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
        tabItem(tabName = "guide",
                fluidRow(
                  box(title = "Equilibrium Data", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,
                      # Download sample data
                      downloadLink("EQsample_link", "Sample Equilibrium Data"),
                      width = 12),                  
                  box(title = "Tie-Line Data", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,
                      # Download sample data
                      downloadLink("TLsample_link", "Sample Tie-Line Data"),
                      width = 12)
                )
        ),
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
                      pre(includeText("functions/normalize.R")),
                      pre(includeText("functions/plotit.R")),
                      pre(includeText("functions/plotitRT.R")),
                      pre(includeText("functions/sortDecreasing.R")),
                      width = 12)
                )
              )
            )
          )
    ))