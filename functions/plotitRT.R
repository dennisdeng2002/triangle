plotitRT <- function(myEQData, myRTData, component1, component2, hitRT, session) { 
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
  a <- ggplot(data = myEQData()) + geom_point()
  
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
  
  # Add additional data points
  a <- a + switch(hitRT,
    geom_point(data = myRTData(), aes_string(x=colnames(myRTData())[component1num], y=colnames(myRTData())[component2num])),
    geom_point(data = myRTData(), aes_string(x=colnames(myRTData())[component2num], y=colnames(myRTData())[component1num]))
    )
  
  # Add labels
  a <- a + geom_text(data = myRTData(), aes_string(label=colnames(myRTData()[3])), hjust=0, vjust=-0.5)

  # Plot reference line
  a <- a + geom_line(data=reference, aes_string(x=colnames(myEQData())[component1num], y=colnames(myEQData())[component2num]))
}