
library(shiny)
library(ggtern)
library(gridSVG)
library(svgPanZoom)
library(SVGAnnotation)
library(rhandsontable)
library(tools)

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
    width = 4
    ),
  verbatimTextOutput("info"),
  # Generate plot
  mainPanel(
    column(width = 12, class = "well",
           plotOutput("TernPlot"
                      , click = "plot_click",
               dblclick = "plot_dblclick",
               hover = "plot_hover")
#                brush = brushOpts(
#                id = "plot_brush",
#                resetOnNew = TRUE))
  ))),
  tabPanel("Help")
))