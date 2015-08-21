normalize <- function(myData){
  rows = seq(1,nrow(myData))
  columns = seq(1,ncol(myData))
  counter = 0
  # Loop used to determine whether values are given as strictly decimals or whole numbers (which can also contain decimals)
  for(i in rows){
    for(j in columns){
      if(myData[i,j] > 1){
        counter = counter + 1
        # Assume that if more than 4 (arbitrary) values are > 1, then all values need to be rounded
        if(counter > 4){
          break
        }
      }
    }
  }
  # Normalize to 1 if values aren't given as decimals
  if(counter > 4){
    for(i in rows){
      for(j in columns){
        myData[i,j] = myData[i,j] / 100
      }
    }
  }
  myData
}