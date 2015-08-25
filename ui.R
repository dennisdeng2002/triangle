library(shinydashboard)
library(rhandsontable)
library(tools)
library(shinyBS)
library(svgPanZoom)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "LLE"),
    # Sidebar
    dashboardSidebar(
      sidebarMenu(
        menuItem("LLE Data", tabName = "data", icon = icon("wheelchair")),
        menuItem("LLE Plot", tabName = "plot", icon = icon("recycle"))
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
                    div(style="width: 50%; margin: 0 auto;",
                    # Upload CSV file of equilibrium data
                    fileInput("EQfile", "Upload Equilibrium Data:"),
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
                    fileInput("TLfile", "Upload Tie-Line Data:"),
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
                    bsButton("switch_button", "Switch Ranges", type = "action", block = TRUE, icon = icon("arrows-h")), width = 5)
                )
        ),
        tabItem(tabName = "plot",
                fluidRow(
                  box(
                    div(style="width: 50%; margin: 0 auto;",
                    svgPanZoomOutput("TernPlot", height = "550px", width = "500px")),
                    # Toggle
                    conditionalPanel("output.fileUploaded", actionLink("axistog", "Toggle Axis Display")),
                    bsModal("add_box", "Additional Data Points", trigger = "add_button",
                            # Generate data table
                            rHandsontableOutput("hot"),
                            br(),
                            actionButton("graph_button", "Graph"),
                            actionButton("clear_button", "Clear"),
                            size = "small"
                            ),
                    actionButton("add_button", "Add Points"),
                    width = 12)
                )
              )
            )
          )
    ))