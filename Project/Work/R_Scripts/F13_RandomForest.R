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

# Compute model without weights
m00 <- randomForest( popularity ~. , data = TR ,ntree = 1000 )

# Compute model with weights
m01 <- randomForest( popularity ~. , data = TR ,ntree = 1000 , classwt = c( A = 0.31, B = 0.45, C = 0.19, D = 0.03, E = 0.01))

################################################################################
# Try different mtry
################################################################################

# Using own implementation 

# Initialize empty list
meta   <- list()
# Initialize levels for mtry
levels <- seq(4,16,1) 

for( i in 1:10){

run02 <- validateForest( X , Y , k = 5 , nt = 1000, mt = levels[i])
print(run02$Accuracy)
meta[[i]] <- run02

}

# Using package implementation

tuneRF(X, Y, mtryStart = 4 , ntreeTry = 50, stepFactor = 1.5 , improve=0.0005,
       trace=TRUE, plot=TRUE, doBest=FALSE)

################################################################################
# Extended data  
################################################################################

m02 <- randomForest( popularity ~. , data = temp2 ,ntree = 1500)

pre <- predict(m01, newdata = test2 , type ='class' )

toSubmission( pre , test , id = NA , name = "prediction" , save = FALSE )
test2 <- test[,!(names(test) %in% c("id","url") ) ]

dat <- as.data.frame( cbind( id = idTE , popularity = as.numeric(pre) ) 

write.table(dat, file = paste0("RFA2",Sys.Date(),".csv") ,
            row.names = FALSE , 
            col.names = TRUE, 
                  sep = "," )

################################################################################

################################################################################