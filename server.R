
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(ggtern)
shinyServer(function(input, output) {
   
  output$linePlot <- renderPlot({
    infile <- input$file1
    if (is.null(infile))
      return(NULL)
    else{
    myData = read.csv(infile$datapath)
    # generate data
    #x    <- myData[,1]
    #y    <- myData[,2]
      
    # draw the graph
    # plot(x, y)
    #Create Some Random (Trivial) Data and ensure the rows sum to unity
 
    
    # For Rendering the Lines, use Segment geometry
    lines <- data.frame(x = c(0.5, 0, 0.5),
                        y = c(0.5, 0.5, 0),
                        z = c(0, 0.5, 0.5),
                        xend = c(1, 1, 1)/3,
                        yend = c(1, 1, 1)/3,
                        zend = c(1, 1, 1)/3)
    # Render.
    ggtern(data = myData, aes(Acetone, Water, TCE)) +
      geom_point() +
      geom_segment(data = lines,
                   aes(x, y, z,
                       xend = xend, yend = yend, zend = zend),
                   color = 'red', size = 1)
    
    }
  })
  
})
