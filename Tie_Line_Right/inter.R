# Interpolation for Right Triangle
# Gives the premise of how this will be implemented because shiny
library(ggplot2)
source("rightTL.R")
#Read Data
myData <- read.csv("EQA.csv");
TLData <- read.csv("TLA.csv");
# Ranges
R <- 1:6;  E <- 7:12

conv <- ifelse((sum(myData>1) > 4), 100, 1)

x <- 2
y <- 1
s <- 1
Raf <- 2

plot <- ggplot(data=myData,aes(x=myData[,x],y=myData[,y])) +
        geom_point() + 
        geom_segment(x=0,y=conv,xend=conv,yend=0,linetype=2) +
        xlim(0, conv) + ylim(0, conv)

TL <- rightTL(myData , TLData , x , y , s , R , E, Raf)

plot <- plot + 
        geom_segment(data = TL, aes(x=TL[,1],y=TL[,2],xend=TL[,3],yend=TL[,4]),linetype=2)
plot