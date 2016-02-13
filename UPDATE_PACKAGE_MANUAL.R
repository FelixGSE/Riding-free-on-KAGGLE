################################################################################
####################     Update Package Manual 'pravetz'    ####################
################################################################################

# Author:       FDenitsa Panova, Felix Gutmann, Thomas Vincente
# Programm:     Barcelona graduate school of economics - M.S. Data Science 
# Course:       15D012 - Advanced Computational Methods (Term 2)
# Type:         Classification competition

# Description:	Run this file to update package manual

################################################################################
### Preamble
################################################################################

### Load packages
library("devtools")
library("roxygen2")

##ä Got to package folder
setwd("/home/felix/GSE/Term 2/Kaggle Competition/Riding-free-on-KAGGLE/Package")

### Update Package Manual 
devtools::document("pravetz")

################################################################################

################################################################################