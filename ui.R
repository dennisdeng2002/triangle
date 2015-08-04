
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(ggtern)
library(gridSVG)
library(svgPanZoom)
library(rhandsontable)

shinyUI(fluidPage(
  
  # Application title
  headerPanel("Ternary Diagram"),
  
  # Sidebar
  sidebarPanel(
  # Upload CSV file  
  fileInput("file1", "Upload Data:"),
  # Generate table
  rHandsontableOutput("hot"),
  # Commit changes to table
  actionButton("plot_button","Graph")
  ),
  verbatimTextOutput("info"),
  # Generate plot
  mainPanel(
    column(width = 12, class = "well",
    plotOutput("TernPlot"
                     , click = "plot_click",
               dblclick = "plot_dblclick",
               hover = "plot_hover",
               brush = brushOpts(
               id = "plot_brush",
               resetOnNew = TRUE))
  ))
))