library(shinydashboard)
library(rhandsontable)
library(tools)
library(shinyBS)
library(svgPanZoom)
library(plotly)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "LLE"),
    # Sidebar
    dashboardSidebar(
      sidebarMenu(
        menuItem("LLE Data", tabName = "data", icon = icon("table")),
        menuItem("Ternary Plot", tabName = "Tplot", icon = icon("area-chart")),
        menuItem("Right Triangular Plot", tabName = "RTplot", icon = icon("area-chart"))
      )),
    #   sliderInput("zoom_slider", label = "Zoom", min = 0, max = 2.0, value = 1.0, step = 0.1),
    # Generate plot
    dashboardBody(
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
                      'tsv'
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
                    actionButton("EQclear_button","Clear"))),
                  
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
                      'tsv'
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
                    selectInput("TLcomponent", "Component", choices = c("X1", "X2", "X3"), selected = "X1", width = "150px"),
                    # Generate tie-line data table
                    rHandsontableOutput("TLhot"),
                    br(),
                    # Graph data table changes
                    actionButton("TLgraph_button","Graph"),
                    # Clear table/plot
                    actionButton("TLclear_button","Clear")), width = 5),
                  
                  box(
                    title = "Raffinate/Extract Ranges", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    sliderInput("raffinate", label = "Raffinate", min = 1, max = 10, value = c(1,5)),
                    sliderInput("extract", label = "Extract", min = 1, max = 10, value = c(6,10)),
                    div(style="width: 50%; margin: 0 auto;",
                    bsButton("setRanges_button", "Set Ranges", type = "action", block = FALSE),
                    bsButton("switchRanges_button", "Switch", type = "action", block = FALSE, icon = icon("arrows-v"))), width = 5)
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
                        plotOutput("RightPlot", height = "500px", width = "500px")),
                    # Toggle
                    conditionalPanel("output.fileUploaded", actionLink("axistogRT", "Toggle Axis Display")),
                    bsModal("right_add_box", "Additional Data Points", trigger = "right_add_button",
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
        )
            )
          )
    ))