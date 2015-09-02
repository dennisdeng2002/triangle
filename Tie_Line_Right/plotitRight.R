plotitRight <- function(myData, TLData, E, R, C1, C2){
  
  sec_order <- function(newdist, model) {
    coefs <- coef(model)
    #y = a + b*x + c*x^2
    res <- coefs[1] + (coefs[2] * newdist) + (coefs[3] * newdist^2) 
    return(res)
  }
  
  fitE <- lm(myData[E,C2] ~ poly(myData[E,C1], 2, raw=TRUE))
  fitR <- lm(myData[R,C2] ~ poly(myData[R,C1], 2, raw=TRUE))
  InterE <- sec_order(TLData[2],fitE)
  InterR <- sec_order(TLData[1],fitR)
  Inter <- data.frame(Raf = InterR, Ext = InterE)
  print(InterE)
  b <- ggplot(myData, aes_string(x=myData[C2], y=myData[C1]))   + geom_point() 
  b <- b + geom_segment(aes(x=0,y=100,xend=100,yend=0),linetype=2)
 # b <- b + geom_segment(aes_string(x=Inter$Ext, y=TLData[,2], xend=sort(InterR, decreasing = TRUE), yend=sort(TLData[,1],decreasing = TRUE)),linetype=2)
  b
}