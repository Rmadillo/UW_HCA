## R code and data for Ryan and Jeff's class

Dwight Barry and Bryan Nice  
February 25, 2016  

Packages to install:  
```
install.packages(c("ggplot2", "dplyr", "dygraphs", "DT", "googleVis", "qcc", "tidyr", "scales","knitr", "strucchange", "devtools", "lubridate", "htmlTable", "htmlwidgets","VIM","quantreg","GGally","psych"))

devtools::install_github("hrbrmstr/taucharts")

# Note that to run R markdown (.Rmd) files at a UW PCE workstation, you have to download the data first

download.file("http://raw.githubusercontent.com/Rmadillo/UW_HCA/master/data/clabsi.csv", "clabsi.csv")

# Then to access it, you'll need to change the path in the .Rmd file to R's working directory

getwd()

```
