################################################################################
####################       F13. Support Vector Machines     ####################
################################################################################

# Author:       Felix Gutmann
# Programm:     Barcelona graduate school of economics - M.S. Data Science 
# Course:       15D012 - Advanced Computational Methods (Term 2)
# Type:         Classification competition

# Description:	

################################################################################
### Preamble
################################################################################

### Clear workspace
rm(list = ls())

### Load packages
if (!require("e1071")) 	      install.packages("e1071"); 	      library(e1071)
if (!require("caret"))        install.packages("caret");        library(caret)

### Set working directory
setwd("/home/felix/Dropbox/GSE/DS_GROUP_BOX/15D012 - Advanced Computational Methods/Work/R-Scripts/Felix")

### Load data
source("F01_DataConfig_1.R")

################################################################################
# Meassuring some computation time
################################################################################

# Compute Weights
S01 <- sampInfo(as.numeric(tr01$popularity) )/100
 
# Get system time for SVM
system.time(svm( popularity ~ ., data = tr01 , kernel = "radial" , cost = 1, scale = FALSE, gamma = 0.1,
      class.weights = c(A = S01[1] ,B = S01[2] , C = S01[3] , D = S01[4], E = S01[5])))


################################################################################
# Model 1: Without changes
################################################################################

# Compute Weights
S01 <- sampInfo(as.numeric(tr01$popularity) )/100

# Compute Model
m01   <- svm( popularity ~ ., data = tr01 , kernel = "radial" , cost = 15 ,scale = FALSE)

# Predict Values
pre01 <- predict( m01 , te01 , decision.values = TRUE)

################################################################################
# Save predictions
################################################################################


################################################################################

################################################################################