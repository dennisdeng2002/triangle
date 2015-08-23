normalize <- function(myData){
counter = sum(as.numeric(myData>1))  
  # Normalize to 1 if values aren't given as decimals
  if(counter > 4){
    myData = myData / 100
  }
  myData
}