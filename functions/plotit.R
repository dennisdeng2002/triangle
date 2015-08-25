plotit <- function(myEQData, TLData, myData, hit) { 
 
  a <- ggtern(data = myEQData()) + 
    geom_point() + stat_smooth_tern(se=0, method="loess") + 
    theme(axis.tern.showarrows = TRUE, axis.tern.showlabels = TRUE) +
    theme(axis.tern.showtitles = FALSE) 

  a <- a + switch(hit,  
    aes_string(x=colnames(myEQData())[2], y=colnames(myEQData())[1], z=colnames(myEQData())[3]),
    aes_string(x=colnames(myEQData())[1], y=colnames(myEQData())[2], z=colnames(myEQData())[3]),
    aes_string(x=colnames(myEQData())[1], y=colnames(myEQData())[3], z=colnames(myEQData())[2]),
    aes_string(x=colnames(myEQData())[2], y=colnames(myEQData())[3], z=colnames(myEQData())[1]),
    aes_string(x=colnames(myEQData())[3], y=colnames(myEQData())[2], z=colnames(myEQData())[1]),
    aes_string(x=colnames(myEQData())[3], y=colnames(myEQData())[1], z=colnames(myEQData())[2])
  )

  a <- a + switch(hit,
    geom_segment(data = TLData, aes_string(x=colnames(TLData)[2], y=colnames(TLData)[1], z=colnames(TLData)[3], xend=colnames(TLData)[5], yend=colnames(TLData)[4], zend=colnames(TLData)[6])),
    geom_segment(data = TLData, aes_string(x=colnames(TLData)[1], y=colnames(TLData)[2], z=colnames(TLData)[3], xend=colnames(TLData)[4], yend=colnames(TLData)[5], zend=colnames(TLData)[6])),
    geom_segment(data = TLData, aes_string(x=colnames(TLData)[1], y=colnames(TLData)[3], z=colnames(TLData)[2], xend=colnames(TLData)[4], yend=colnames(TLData)[6], zend=colnames(TLData)[5])),
    geom_segment(data = TLData, aes_string(x=colnames(TLData)[2], y=colnames(TLData)[3], z=colnames(TLData)[1], xend=colnames(TLData)[5], yend=colnames(TLData)[6], zend=colnames(TLData)[4])),
    geom_segment(data = TLData, aes_string(x=colnames(TLData)[3], y=colnames(TLData)[2], z=colnames(TLData)[1], xend=colnames(TLData)[6], yend=colnames(TLData)[5], zend=colnames(TLData)[4])),
    geom_segment(data = TLData, aes_string(x=colnames(TLData)[3], y=colnames(TLData)[1], z=colnames(TLData)[2], xend=colnames(TLData)[6], yend=colnames(TLData)[4], zend=colnames(TLData)[5]))
    ) 
  
  # Add additional data points
  a <- a + switch(hit,
    geom_point(data = myData(), aes_string(x=colnames(myData())[2], y=colnames(myData())[1], z=colnames(myData())[3]), color = "blue"),
    geom_point(data = myData(), aes_string(x=colnames(myData())[1], y=colnames(myData())[2], z=colnames(myData())[3]), color = "blue"),
    geom_point(data = myData(), aes_string(x=colnames(myData())[1], y=colnames(myData())[3], z=colnames(myData())[2]), color = "blue"),
    geom_point(data = myData(), aes_string(x=colnames(myData())[2], y=colnames(myData())[3], z=colnames(myData())[1]), color = "blue"),
    geom_point(data = myData(), aes_string(x=colnames(myData())[3], y=colnames(myData())[2], z=colnames(myData())[1]), color = "blue"),
    geom_point(data = myData(), aes_string(x=colnames(myData())[3], y=colnames(myData())[1], z=colnames(myData())[2]), color = "blue")
  ) 
  # Add labels
  a <- a + geom_text(data = myData(), aes_string(label=colnames(myData()[4])))

}