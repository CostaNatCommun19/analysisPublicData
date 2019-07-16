

install.packages(c("ProjectTemplate", "plotrix", "reshape2", "survival", 
                   "ggplot2", "xtable", "GGally", "plyr", "dplyr", "corrplot", 
                   "grid")) # This will install some needed R packages
source("https://bioconductor.org/biocLite.R")
biocLite(c("Biobase", "genefu")) # This is to install Bioconductor packages
download.file(url="http://www.cbs.dtu.dk/~eklund/survplot/survplot_0.0.7.tar.gz", 
              destfile="survplot_0.0.7.tar.gz") 
install.packages(file.path("survplot_0.0.7.tar.gz"), repos=NULL) ## This will install a package to plot survival curves (from the author's website because the package is not available anywhere else)
