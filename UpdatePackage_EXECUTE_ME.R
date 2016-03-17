install.packages("devtools")
library("devtools")
install.packages("roxygen2")
library(roxygen2)


setwd('/Users/felix/Documents/GSE/Term 2/15D012 Advanced Computational Methods/Competition/Riding-free-on-KAGGLE/Package/')
devtools::document()
devtools::check()
devtools::document()
if (!require("devtools")) install.packages("devtools"); library(devtools)
if (!require("roxygen2")) install.packages("roxygen2"); library(roxygen2)
