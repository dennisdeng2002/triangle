library(shiny)


interpolateTL <- function(values, ranges){
  
  # Normalize tie-line data
  TLData = normalize(values[["TLhot"]])
  # Sort equilibrium data based on raffinate/extract side
  EQData = sortDecreasing(values[["EQhot"]], ranges)
  # For loop counter
  rows = seq(1,nrow(TLData))
  # Initialize placeholder matrix
  interTLData = data.frame(matrix(0.0, nrow = nrow(TLData), ncol = 2*ncol(EQData)))
  for(i in rows){ 
    interTLData[i,1] = TLData[i,1]
    interTLData[i,2] = interpolate(TLData[i,1], range = ranges$R, EQData)[1]
    interTLData[i,3] = interpolate(TLData[i,1], range = ranges$R, EQData)[2]
    interTLData[i,4] = TLData[i,2]
    interTLData[i,5] = interpolate(TLData[i,2], range = ranges$E, EQData)[1]
    interTLData[i,6] = interpolate(TLData[i,2], range = ranges$E, EQData)[2]
  }
  interTLData
}