#### ui.R
# Issues: 
### LOAD LIBRARIES ###
library(shiny)         # SHINY - Web interface
library(ggtern)        # Plotting ternary diagram
library(gridSVG)       # Converting Image to SVG
library(svgPanZoom)    # Pan and Zoom function
library(rhandsontable) # Table interface
library(tools)
library(plyr)
library(Cairo)  
library(grid)

shinyUI(
    navbarPage("LLE",
          tabPanel("Ternary",
          headerPanel("Ternary Diagram"),
                            
          # Sidebar
            sidebarPanel(
            # Upload CSV file  
              fileInput("file1", "Upload Data:", accept = c('.csv',
                                                            '.tsv')),
            # Generate table
              rHandsontableOutput("hot"),
            # Commit changes to table
              actionButton("plot_button","Graph")),
            
            
            # verbatimTextOutput("info"),
          
            
            # Generate plot
            mainPanel(
              column(width = 12, class = "well",
#                plotOutput("TernPlot", 
#                width = "100%",
#                click = "plot_click",
#                dblclick = "plot_dblclick",
#                hover = "plot_hover",
#                brush = brushOpts(
#                 id = "plot_brush",
#                 resetOnNew = TRUE))
                 svgPanZoomOutput(outputId = "TernPlot")
               )
              )),
#                  ))
          tabPanel("Help",
             headerPanel("Ternary Diagram"))
 
))