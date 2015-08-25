interpolateTL <- function(values, ranges, component, session){
  # Normalize tie-line data
  TLData = normalize(values[["TLhot"]])
  EQData = values[["EQhot"]]
  # For loop counter
  rows = seq(1,nrow(TLData))
  columns = seq(1,ncol(EQData))
  # Check whether header names for equilbrium data equals selected component
  for(i in columns){
    if(component==colnames(EQData)[i]){
      colnum = i
      break
    }
    else{
      colnum = 0
    }
  }
  # Check if selected component matches component in equilibrium data
  if(colnum!=0){
    # Sort equilibrium data based on raffinate/extract side
    EQData = sortDecreasing(values[["EQhot"]], ranges, colnum)
  }
  # Initialize placeholder matrix
  interTLData = data.frame(matrix(0.0, nrow = nrow(TLData), ncol = 2*ncol(EQData)))
  # Interpolate data based on selected column
  if(colnum==1){
    for(i in rows){ 
      interTLData[i,1] = TLData[i,1]
      interTLData[i,2:3] = interpolate(TLData[i,1], range = ranges$R, EQData, 1, 2, 3, session)
      interTLData[i,4] = TLData[i,2]
      interTLData[i,5:6] = interpolate(TLData[i,2], range = ranges$E, EQData, 1, 2, 3, session)
    }
  }
  else if(colnum==2){
    for(i in rows){ 
      interTLData[i,1] = interpolate(TLData[i,1], range = ranges$R, EQData, 2, 1, 3, session)[1]
      interTLData[i,2] = TLData[i,1]
      interTLData[i,3] = interpolate(TLData[i,1], range = ranges$R, EQData, 2, 1, 3, session)[2]
      interTLData[i,4] = interpolate(TLData[i,2], range = ranges$E, EQData, 2, 1, 3, session)[1]
      interTLData[i,5] = TLData[i,2]
      interTLData[i,6] = interpolate(TLData[i,2], range = ranges$E, EQData, 2, 1, 3, session)[2]
    }
  }
  else if(colnum==3){
    for(i in rows){ 
      interTLData[i,1] = interpolate(TLData[i,1], range = ranges$R, EQData, 3, 2, 1, session)[2]
      interTLData[i,2] = interpolate(TLData[i,1], range = ranges$R, EQData, 3, 2, 1, session)[1]
      interTLData[i,3] = TLData[i,1]
      interTLData[i,4] = interpolate(TLData[i,2], range = ranges$E, EQData, 3, 2, 1, session)[2]
      interTLData[i,5] = interpolate(TLData[i,2], range = ranges$E, EQData, 3, 2, 1, session)[1]
      interTLData[i,6] = TLData[i,2]
    }
  }
  interTLData
}