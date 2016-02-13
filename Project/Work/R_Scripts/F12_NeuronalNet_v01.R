###################################################################################################

###################################################################################################

# Author:       Felix Gutmann
# Course:       Financial Econometrics
# Last update:  20.01.2016
# Type:         Classification competition
# Description:	In this file we inspect some first properties of the data set and play around with
#				some methods like neuronal networks and random forrests

###################################################################################################
### Praeamble
###################################################################################################

### Clear workspace
rm(list = ls())

### Load Packages 
if (!require("neuralnet")) install.packages("neuralnet"); library(neuralnet)

### Set working directory
setwd("/home/felix/Dropbox/GSE/DS_GROUP_BOX/15D012 - Advanced Computational Methods/Data")

### Load and re-order Data
	
	# Training data (3000 x 62)
	training <- read.csv("news_popularity_training.csv")
	# Test data (9644 x 61)
	test 	 <- read.csv("news_popularity_test.csv")  
	# Example of a data submission set (9644 x 2)
	sample 	 <- read.csv("news_popularity_sample.csv")   

###################################################################################################
### First model - Lasso GLM - Internal training
###################################################################################################



















###################################################################################################
###### End CODE # End CODE # End CODE # End CODE # End CODE # End CODE # End CODE # End CODE ######
###################################################################################################