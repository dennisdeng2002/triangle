plotitk <- function(myData,ranges) {
  ggtern(data = myData, aes_string(x=colnames(myData)[1], y=colnames(myData)[2], z=colnames(myData)[3])) +
    geom_point() + 
    coord_tern(T = getOption("tern.default.T"), L = getOption("tern.default.L"),
               R = getOption("tern.default.R"), xlim = ranges$x, ylim = ranges$y,
               Tlim = NULL, Llim = NULL, Rlim = NULL)
}