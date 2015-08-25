sortDecreasing <- function(myData, ranges, colnum){
  # Normalize data
  myData = normalize(myData)
  columns = seq(1,ncol(myData))
  # Store raffinate in temporary placeholder
  tempR = myData[ranges$R,]
  # Reset row numbering to 1:nrow
  row.names(tempR) <- 1:nrow(tempR)
  # Set temporary data based on order of designated column
  tempR = tempR[order(tempR[,colnum], decreasing = TRUE),]

  tempE = myData[ranges$E,]
  row.names(tempE) <- 1:nrow(tempE)
  tempE = tempE[order(tempE[,colnum], decreasing = TRUE),]
  
  # Check whether raffinate/extract occurs first in data set
  # If first number in R is less than last number in E - Raffinate occurs first
  if(ranges$R[1]<ranges$E[length(ranges$E)]){
    myData = rbind(tempR, tempE)
  }
  else if(ranges$R[1]>ranges$E[length(ranges$E)]){
    myData = rbind(tempE, tempR)  
  }
  # Reset row number to 1:nrow
  row.names(myData) <- 1:nrow(myData)
  myData
}