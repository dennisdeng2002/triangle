checkifdecimals <- function(myData){
  counter = sum(as.numeric(myData>1))  
  # Whole numbers
  if(counter > 4){
    return (FALSE)
  }
  # Decimals
  else{
    return (TRUE)
  }
}