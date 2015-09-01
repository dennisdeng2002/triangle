plotit <- function(myEQData, TLData, myData, hit, myTheme) {
  # Set initial graph
  a <- ggtern(data = myEQData) + 
    geom_point(size = myTheme[[1]]) +
    theme(axis.tern.showarrows = TRUE, axis.tern.showlabels = TRUE) +
    theme(axis.tern.showtitles = FALSE)
    # stat_smooth_tern(se=0, method="loess") + 

  # Set theme
  a <- a + switch(myTheme[[3]],
                  "Gray" = {theme_gray()},
                  "B/W" = {theme_bw()},
                  "RGB" = {theme_rgb()}
  )
  
  # Toggle equilibrium data
  a <- a + switch(hit,  
    aes_string(x=colnames(myEQData)[2], y=colnames(myEQData)[1], z=colnames(myEQData)[3]),
    aes_string(x=colnames(myEQData)[1], y=colnames(myEQData)[2], z=colnames(myEQData)[3]),
    aes_string(x=colnames(myEQData)[1], y=colnames(myEQData)[3], z=colnames(myEQData)[2]),
    aes_string(x=colnames(myEQData)[2], y=colnames(myEQData)[3], z=colnames(myEQData)[1]),
    aes_string(x=colnames(myEQData)[3], y=colnames(myEQData)[2], z=colnames(myEQData)[1]),
    aes_string(x=colnames(myEQData)[3], y=colnames(myEQData)[1], z=colnames(myEQData)[2])
  )
  
  # Toggle tie-line data
  a <- a + switch(hit,
    geom_segment(data = TLData, aes_string(x=colnames(TLData)[2], y=colnames(TLData)[1], z=colnames(TLData)[3], xend=colnames(TLData)[5], yend=colnames(TLData)[4], zend=colnames(TLData)[6]), size = myTheme[[2]], linetype="dotted"),
    geom_segment(data = TLData, aes_string(x=colnames(TLData)[1], y=colnames(TLData)[2], z=colnames(TLData)[3], xend=colnames(TLData)[4], yend=colnames(TLData)[5], zend=colnames(TLData)[6]), size = myTheme[[2]], linetype="dotted"),
    geom_segment(data = TLData, aes_string(x=colnames(TLData)[1], y=colnames(TLData)[3], z=colnames(TLData)[2], xend=colnames(TLData)[4], yend=colnames(TLData)[6], zend=colnames(TLData)[5]), size = myTheme[[2]], linetype="dotted"),
    geom_segment(data = TLData, aes_string(x=colnames(TLData)[2], y=colnames(TLData)[3], z=colnames(TLData)[1], xend=colnames(TLData)[5], yend=colnames(TLData)[6], zend=colnames(TLData)[4]), size = myTheme[[2]], linetype="dotted"),
    geom_segment(data = TLData, aes_string(x=colnames(TLData)[3], y=colnames(TLData)[2], z=colnames(TLData)[1], xend=colnames(TLData)[6], yend=colnames(TLData)[5], zend=colnames(TLData)[4]), size = myTheme[[2]], linetype="dotted"),
    geom_segment(data = TLData, aes_string(x=colnames(TLData)[3], y=colnames(TLData)[1], z=colnames(TLData)[2], xend=colnames(TLData)[6], yend=colnames(TLData)[4], zend=colnames(TLData)[5]), size = myTheme[[2]], linetype="dotted")
    ) 
  
  # Toggle tie-line endpoints
  a <- a + switch(hit,
    geom_point(data = TLData, aes_string(x=colnames(TLData)[2], y=colnames(TLData)[1], z=colnames(TLData)[3]), size = myTheme[[1]]),
    geom_point(data = TLData, aes_string(x=colnames(TLData)[1], y=colnames(TLData)[2], z=colnames(TLData)[3]), size = myTheme[[1]]),
    geom_point(data = TLData, aes_string(x=colnames(TLData)[1], y=colnames(TLData)[3], z=colnames(TLData)[2]), size = myTheme[[1]]),
    geom_point(data = TLData, aes_string(x=colnames(TLData)[2], y=colnames(TLData)[3], z=colnames(TLData)[1]), size = myTheme[[1]]),
    geom_point(data = TLData, aes_string(x=colnames(TLData)[3], y=colnames(TLData)[2], z=colnames(TLData)[1]), size = myTheme[[1]]),
    geom_point(data = TLData, aes_string(x=colnames(TLData)[3], y=colnames(TLData)[1], z=colnames(TLData)[2]), size = myTheme[[1]])
  ) 
  
  # Toggle tie-line endpoints
  a <- a + switch(hit,
    geom_point(data = TLData, aes_string(x=colnames(TLData)[5], y=colnames(TLData)[4], z=colnames(TLData)[6]), size = myTheme[[1]]),
    geom_point(data = TLData, aes_string(x=colnames(TLData)[4], y=colnames(TLData)[5], z=colnames(TLData)[6]), size = myTheme[[1]]),
    geom_point(data = TLData, aes_string(x=colnames(TLData)[4], y=colnames(TLData)[6], z=colnames(TLData)[5]), size = myTheme[[1]]),
    geom_point(data = TLData, aes_string(x=colnames(TLData)[5], y=colnames(TLData)[6], z=colnames(TLData)[4]), size = myTheme[[1]]),
    geom_point(data = TLData, aes_string(x=colnames(TLData)[6], y=colnames(TLData)[5], z=colnames(TLData)[4]), size = myTheme[[1]]),
    geom_point(data = TLData, aes_string(x=colnames(TLData)[6], y=colnames(TLData)[4], z=colnames(TLData)[5]), size = myTheme[[1]])
  ) 
  
  # Add additional data points (toggle to ensure correct orientation)
  a <- a + switch(hit,
    geom_point(data = myData, aes_string(x=colnames(myData)[2], y=colnames(myData)[1], z=colnames(myData)[3]), size = myTheme[[1]]),
    geom_point(data = myData, aes_string(x=colnames(myData)[1], y=colnames(myData)[2], z=colnames(myData)[3]), size = myTheme[[1]]),
    geom_point(data = myData, aes_string(x=colnames(myData)[1], y=colnames(myData)[3], z=colnames(myData)[2]), size = myTheme[[1]]),
    geom_point(data = myData, aes_string(x=colnames(myData)[2], y=colnames(myData)[3], z=colnames(myData)[1]), size = myTheme[[1]]),
    geom_point(data = myData, aes_string(x=colnames(myData)[3], y=colnames(myData)[2], z=colnames(myData)[1]), size = myTheme[[1]]),
    geom_point(data = myData, aes_string(x=colnames(myData)[3], y=colnames(myData)[1], z=colnames(myData)[2]), size = myTheme[[1]])
  ) 
  
  # Add labels
  a <- a + geom_text(data = myData, aes_string(label=colnames(myData[4])), hjust=0, vjust=-0.5)
}

