rightTL <- function(myData , TLData , x , y , s , R , E, Raf){
  # Variables
  # x - x-axis component [1,2 3 : referes to column #]
  # y - y-axis component [1,2 3 : referes to column #]
  # s - solute component [1,2 3 : referes to column #]
  # R - range of raffinate on myData
  # E - range of extract
  # Raf - column of raffinate from TLData
  
conv <- ifelse((sum(myData>1) > 4), 100, 1)
  
  # Second Order
  sec_order <- function(newdist, model) {
    coefs <- coef(model)
    #y = a + b*x + c*x^2
    res <- coefs[1] + (coefs[2] * newdist) + (coefs[3] * newdist^2) 
    return(res)
  }

  
  Ext <- setdiff(1:2,Raf)
  
  # Perform interpolation
  # This is the Solvent or Carrier
  a <- sample(setdiff(1:3,s),1)
  # This is the Solvent or Carrier , but not a 
  b <- setdiff(1:3,c(a,s))
  
  fitE <- lm(myData[E,a] ~ poly(myData[E,s], 2, raw=TRUE))
  fitR <- lm(myData[R,a] ~ poly(myData[R,s], 2, raw=TRUE))
  
  # TLData <- is column 1 for the raffinate or extraction? same for column 2
  # For this example, Extract: column 2, and Raffinate: column 1
  
  InterE <- sec_order(TLData[,Ext],fitE)
  InterR <- sec_order(TLData[,Raf],fitR)
  

  
  SE <- data.frame(TLData[,Ext])
  SR <- data.frame(TLData[,Raf])
  AE <- data.frame(InterE)
  AR <- data.frame(InterR)
  BE <- data.frame(conv - SE - AE)
  BR <- data.frame(conv - SR - AR) 

 o <- order(c(a,b,s), decreasing = 0)  
 Extract <- cbind(AE,BE,SE)
 Raffinate <- cbind(AR,BR,SR)
 
 Extract <- Extract[o]
 Raffinate <- Raffinate[o]
 Extract <- Extract[ order(Extract[,s]) ,]
 Raffinate <- Raffinate[ order(Raffinate[,s]) ,  ]
 
return(cbind(Extract[x],Extract[y],Raffinate[x],Raffinate[y]))
 
  
  
  
  
}