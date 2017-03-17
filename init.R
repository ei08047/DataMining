init <- function(){
packages <- c("rmarkdown","knitr" , "readr", "Matrix", "grid", "arules", "arulesViz","cluster")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(rmarkdown)
library(knitr)
library(Matrix)
library(grid)
library(arules)
library(arulesViz)
library(cluster)
summary(installed.packages())
}

init()