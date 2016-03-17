################################################################################
####################           F15. Super Learner           ####################
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

### Clear workspace
rm( list = ls () )

### Set working directory for input and ourput data
setwd('/Users/felix/Dropbox/GSE/DS_GROUP_BOX/15D012 - Advanced Computational Methods/Data')

### Load packages
if (!require("SuperLearner")) install.packages("SuperLearner"); library(SuperLearner)
if (!require("polspline")) install.packages("polspline"); library(polspline)
if (!require("gbm")) install.packages("gbm"); library(gbm)
if (!require("glmnet")) install.packages("glmnet"); library(glmnet)
if (!require("xgboost")) install.packages("xgboost"); library(xgboost)
if (!require("randomForest")) install.packages("randomForest"); library(randomForest)
if (!require("igraph")) install.packages("igraph"); library(igraph)

#Initialize function 
labConvert <- function( y , i ){
  newVec <- ifelse( y == i , 1 , 0 )
  return( newVec )
} 

# Training data (30000 x 62)
full.training <- read.csv("news_popularity_training.csv")
# Test data (9644 x 61)
full.test    <- read.csv("news_popularity_test.csv")  
# Example of a data submission set (9644 x 2)
sample   <- read.csv("news_popularity_sample.csv") 

################################################################################

################################################################################

training <- full.training[ ,!( names(full.training) %in% c( 'id', 'url','popularity' ) )]
test     <- full.test[ ,!( names(full.test) %in% c( 'id', 'url' ) )]
Y        <- full.training$popularity

y01 <- labConvert( Y , 1 )
y02 <- labConvert( Y , 2 )
y03 <- labConvert( Y , 3 )
y04 <- labConvert( Y , 4 )
y05 <- labConvert( Y , 5 )

SL.library <- c("SL.gbm",
                "SL.randomForest",
                "SL.glmnet"
                )

# least squares loss function
fit01 <- SuperLearner(Y = y01, X = training, newX = test, SL.library = SL.library, verbose = TRUE, method = "method.NNLS", family = binomial(), cvControl = list(V = 2))
fit02 <- SuperLearner(Y = y02, X = training, newX = test, SL.library = SL.library, verbose = TRUE, method = "method.NNLS", family = binomial(), cvControl = list(V = 2))
fit03 <- SuperLearner(Y = y03, X = training, newX = test, SL.library = SL.library, verbose = TRUE, method = "method.NNLS", family = binomial(), cvControl = list(V = 2))
fit04 <- SuperLearner(Y = y04, X = training, newX = test, SL.library = SL.library, verbose = TRUE, method = "method.NNLS", family = binomial(), cvControl = list(V = 2))
fit05 <- SuperLearner(Y = y05, X = training, newX = test, SL.library = SL.library, verbose = TRUE, method = "method.NNLS", family = binomial(), cvControl = list(V = 2))

pred01 = fit01$SL.predict[, 1]
pred02 = fit02$SL.predict[, 1]
pred03 = fit03$SL.predict[, 1]
pred04 = fit04$SL.predict[, 1]
pred05 = fit05$SL.predict[, 1]

write.csv(pred02,'class02.csv')
write.csv(pred01,'class01.csv')
write.csv(pred03,'class03.csv')
write.csv(pred04,'class04.csv')
write.csv(pred05,'class05.csv')

predictions <- data.frame( pred01 = fit01$SL.predict[, 1], 
                           pred02 = fit02$SL.predict[, 1], 
                           pred03 = fit03$SL.predict[, 1])
                           pred04 = fit04$SL.predict[, 1],
                           pred05 = fit05$SL.predict[, 1]
                          )

classification <- apply(predictions, 1, function( i ) c(1,2,3,4,5)[unname(which.max( i ))])

################################################################################

################################################################################