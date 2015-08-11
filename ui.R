#### ui.R
# Issues: 
### LOAD LIBRARIES ###
library(shiny)         # SHINY - Web interface
library(ggtern)        # Plotting ternary diagram
library(gridSVG)       # Converting Image to SVG
library(svgPanZoom)    # Pan and Zoom function
library(rhandsontable) # Table interface
library(tools)
library(grid)         
library(markdown)      # Used for Help

<<<<<<< HEAD
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
=======
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
               plotOutput("TernPlot", 
               width = "100%",
               click = "plot_click",
               dblclick = "plot_dblclick",
               hover = "plot_hover",
               brush = brushOpts(
                id = "plot_brush",
                resetOnNew = TRUE)))
              )),
#                  ))
          tabPanel("Help",
             headerPanel("Ternary Diagram"),
             fluidRow(
               column(4,
                      includeMarkdown("HELP.md")
               )))
>>>>>>> c79f4e669bbfd731ddcddc035cde073a0b9ad3d0
))