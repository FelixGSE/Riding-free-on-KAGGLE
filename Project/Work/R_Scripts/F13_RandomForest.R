################################################################################
####################            F13. Random Forest          ####################
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
if (!require("randomForest")) install.packages("randomForest"); library(randomForest)

### Set working directory
setwd("/home/felix/Dropbox/GSE/DS_GROUP_BOX/15D012 - Advanced Computational Methods/Work/R-Scripts/Felix")

### Load data
source("F01_DataConfig_1.R")

################################################################################
# Model 1: Without changes
################################################################################

m02 <- randomForest(popularity~., data=tr06, ntree=10 )

m03 <- randomForest(popularity~., data=tr06, ntree=1000 )
m04 <- randomForest(popularity~., data=tr06, ntree=1500 )

ahuu <- acc( useForest ,  tr07 , k = 5)

################################################################################

################################################################################