interpolate <- function(x, range, myData, x_num, y_num, z_num, session){
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
    if(myData[i,x_num] == x){
      # Set water and TCE to equilibrium data
      y <- myData[i,y_num]
      z <- myData[i,z_num]
      break
    } 
    # Check whether tie-line data falls between equilibrium data
    else if(myData[i,x_num] > x & x > myData[i+1,x_num]){
      # Modeling water as a function of acetone
      fit <- lm(myData[(i):(i+1),y_num]~myData[(i):(i+1),x_num])
      # Calculate theoretical water and TCE values
      y <- first_order(x, fit)
      z <- 1-y-x
      break
    } 
    else{
      y = 0
      z = 0
    }
  }
  # Create alert message for any zero/negative values
  if(y<0 || z<0 || (y==0 && z==0)){
    print("a")
    createAlert(session, "alert", "interpolateAlert", content = "Error: Tie-Line Interpolation", append = FALSE)
  }
  # Return both y and z
  yz = c(y,z)
  return(yz)
}