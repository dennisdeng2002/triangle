# library(shiny)
# # library(ggtern)
# # library(gridSVG)
# # library(svgPanZoom)
# # library(SVGAnnotation)
# # library(rhandsontable)
# # library(tools)
library(shinydashboard)

shinyUI(
  navbarPage("LLE",
             tabPanel("Ternary",
  # Application title
  headerPanel("Ternary Diagram"),
  
  # Sidebar
  sidebarPanel(
    # Upload CSV file of equilibrium data
    fileInput("EQfile", "Upload Equilibrium Data:"),
    # Upload CSV file of tie-line data
    fileInput("TLfile", "Upload Tie-Line Data:"),
    # Generate equilibrium data table
    h4("Equilibrium Data"),
    rHandsontableOutput("EQhot"),
    # Generate tie-line data table
    h4("Tie-Line Data"),
    rHandsontableOutput("TLhot"),
    # Graph data table changes
    actionButton("graph_button","Graph"),
    # Clear table/plot
    actionButton("clear_button","Clear"),
    conditionalPanel("output.fileUploaded",
      actionLink("axistog", "Toggle axis display")),
    width = 4
    ),
#   verbatimTextOutput("info"),
  sliderInput("raffinate", label = "Raffinate", min = 1, max = 12, value = c(1,6)),
  sliderInput("extract", label = "Extract", min = 1, max = 12, value = c(7,12)),
#   sliderInput("zoom_slider", label = "Zoom", min = 0, max = 2.0, value = 1.0, step = 0.1),
  # Generate plot
  mainPanel(
    column(width = 12, class = "well",
           svgPanZoomOutput("TernPlot")
#                       , click = "plot_click",
#                dblclick = "plot_dblclick",
#                hover = "plot_hover")
#                brush = brushOpts(
#                id = "plot_brush",
#                resetOnNew = TRUE))
  ))),
  tabPanel("Help")
))