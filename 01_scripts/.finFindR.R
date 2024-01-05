install.packages("remotes")
install.packages("drat", repos="https://cran.rstudio.com")
install.packages("imager")

drat:::addRepo("dmlc")
install.packages("mxnet")

remotes::install_github("haimeh/finFindR")
install.packages("finFindR")

  remotes::install_github("apache/mxnet")
library(finFindR)
library(imager)
mxnet::