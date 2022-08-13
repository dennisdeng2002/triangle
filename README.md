#### Synopsis
Shiny web application for multistage counter-current extraction. Consists of a ternary plot used for visualizing component compositions, CSV/and Excel table used for data input and tie line interpolation. 

Important R Packages:

* [Rstudio+Shiny](https://www.rstudio.com/)
* [ggplot2](http://ggplot2.org/)
* [ggtern](www.ggtern.com)
* [rhandsontable](https://github.com/jrowen/rhandsontable)

#### Installation
install.packages(c("shiny", "shinydashboard", "rhandsontable", "tools", "shinyBS", "svgPanZoom", "shinyjs", "V8"))

install.packages(c("ggtern", "gridSVG", "xlsx", "ggthemes", "R.utils"))

install.packages("devtools")
library(devtools)
install_github("duncantl/SVGAnnotation")
