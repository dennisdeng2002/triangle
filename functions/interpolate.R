interpolate <- function(x, range, myData){
  # Initialize y and z
  y <- seq(0,0,l=length(x))
  z <- seq(0,0,l=length(x))
  # Set range to 1 minus inputted range
  range = seq(range[1],range[length(range)-1])
  # Generic form for a linear equation
  first_order <- function(newdist, model) {
    coefs <- coef(model)
    #y = b + mx
    res <- coefs[1] + (coefs[2] * newdist)
    return(res)
  }
  # Example: x = Acetone, y = Water, z = TCE
  for(i in range){
    # Check whether tie-line data = equilibrium data
    if(myData[i,1] == x){
      # Modeling water as a function of acetone
      y <- myData[i,2]
      z <- myData[i,3]
      break
    } 
    # Check whether tie-line data falls between equilibrium data
    else if(myData[i,1] > x & x > myData[i+1,1]){
      # Modeling water as a function of acetone
      fit <- lm(myData[(i):(i+1),2]~myData[(i):(i+1),1])
      # Calculate theoretical water and TCE values
      y <- first_order(x, fit)
      z <- 1-y-x
      break
    } 
    else{
    }
  }
  # Return both y and z
  yz = c(y,z)
  return(yz)
}