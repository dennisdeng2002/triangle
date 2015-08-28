plotitRT <- function(myEQData, myRTData, component1, component2, hitRT, session, myRTTheme) { 
  # Loop counter
  columns = seq(1,ncol(myEQData()))
  # Check whether header names for equilbrium data equals component1
  for(i in columns){
    if(component1==colnames(myEQData())[i]){
      component1num = i
      break
    }
    else{
      component1num = 1
    }
  }
  # Check whether header names for equilbrium data equals component2
  for(j in columns){
    if(component2==colnames(myEQData())[j]){
      component2num = j
      break
    }
    else{
      component2num = 1
    }
  }
  if(checkifdecimals(myEQData())==TRUE){
    # Create reference line (0,1) and (1,0)
    reference = data.frame(matrix(c(0,1,1,0), nrow = 2, ncol = 2))
  }
  else{
    # Create reference line (0,100) and (100,0)
    reference = data.frame(matrix(c(0,100,100,0), nrow = 2, ncol = 2))
  }
  # Set reference line header to plotted data
  colnames(reference) <- c(colnames(myEQData()[component1num]), colnames(myEQData()[component2num]))
  
  # Plot data
  a <- ggplot(data = myEQData()) + geom_point(size = myRTTheme()[[1]])

  # Set theme
  a <- a + switch(myRTTheme()[[3]],
                 "Gray" = {theme_gray()},
                 "B/W" = {theme_bw()},
                 "Calc" = {theme_calc()},
                 "Economist" = {theme_economist()},
                 "Excel" = {theme_excel()},
                 "Few" = {theme_few()},
                 "Google Docs" = {theme_gdocs()},
                 "Highcharts" = {theme_hc()},
                 "Pander" = {theme_pander()},
                 "Solarized" = {theme_solarized()},
                 "Stata" = {theme_stata()},
                 "Tufte" = {theme_tufte()},
                 "WSJ" = {theme_wsj()}
  )
  
  # Toggle axis label
  switch(hitRT,
         {updateSelectInput(session, "component1", "X Axis")
           updateSelectInput(session, "component2", "Y Axis")},
         {updateSelectInput(session, "component1", "Y Axis")
           updateSelectInput(session, "component2", "X Axis")}
  )
  
  # Toggle equilibrium data
  a <- a + switch(hitRT,
    aes_string(x=colnames(myEQData())[component1num], y=colnames(myEQData())[component2num]),
    aes_string(x=colnames(myEQData())[component2num], y=colnames(myEQData())[component1num])
  )
  
  # Add point
  a <- a + switch(hitRT,
    geom_point(data = myRTData(), aes_string(x=colnames(myRTData())[1], y=colnames(myRTData())[2]), size = myRTTheme()[[1]]),
    geom_point(data = myRTData(), aes_string(x=colnames(myRTData())[2], y=colnames(myRTData())[1]), size = myRTTheme()[[1]])
                  )
  
  # Add labels
  a <- a + geom_text(data = myRTData(), aes_string(label=colnames(myRTData()[3])), hjust=0, vjust=-0.5)
  
  # Plot reference line
  a <- a + geom_line(data=reference, aes_string(x=colnames(myEQData())[component1num], y=colnames(myEQData())[component2num]))
}