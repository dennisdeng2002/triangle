sortDecreasing <- function(myData, ranges){
  myData = normalize(myData)
  columns = seq(1,ncol(myData))
  tempR = myData[ranges$R,]
  row.names(tempR) <- 1:nrow(tempR)
  tempR = tempR[order(tempR[,1], decreasing = TRUE),]
  tempE = myData[ranges$E,]
  row.names(tempE) <- 1:nrow(tempE)
  tempE = tempE[order(tempE[,1], decreasing = TRUE),]
  if(ranges$R[1]==1){
    myData = rbind(tempR, tempE)
  }
  else if(ranges$E[1]==1){
    myData = rbind(tempE, tempR)  
  }
  row.names(myData) <- 1:nrow(myData)
  myData
}