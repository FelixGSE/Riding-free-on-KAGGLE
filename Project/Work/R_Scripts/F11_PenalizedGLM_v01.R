################################################################################
############### F11. Multinomial regularization regression  ####################
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

### Load Packages 
if (!require("glmnet")) install.packages("glmnet"); library(glmnet)

### Set working directory and output path
setwd("~/Dropbox/GSE/DS_GROUP_BOX/15D012 - Advanced Computational Methods/Work/R-Scripts/Felix")

path <-"~/Dropbox/GSE/DS_GROUP_BOX/15D012 - Advanced Computational Methods/Work/Output/OutputData/Felix/"

### Source related files
	
	# Source data configuration
	source("F01_DataConfig_1.R")
	
################################################################################
# Lasso regularization
################################################################################

# Convert data to glmnet input format
X01  <- as.matrix(training[,-1])
X02  <- as.matrix(test) 
Y01  <- as.matrix(training$popularity)

# Compute model - Type Lasso (alpha=1) - for full training set
m01 <- cv.glmnet( X01,Y01, family = "multinomial" , alpha = 1 ,standardize =FALSE )

# Predict outcome and adjust output
pre01 <- as.data.frame( predict( m01,X02, type = "response", s = "lambda.min"))
colnames(pre01) <- c("target1","target2","target3","target4","target5")

# classify data and create corresponding labels
result  <- pre01 == apply( pre01 , 1, max )
predict <- rep(NA,nrow(result))

for(i in 1:nrow(result)){
  pos <- as.numeric(which(result[i,] == TRUE))
  predict[i] <- pos
}

# Prepare output for export
predictM01 <- as.data.frame(cbind(id=idTE,popularity=predict))

# Export predictions
file01 <- paste0("pred_F01_M01_LASSO_",config,"_",Sys.Date(),".csv")
write.table(predictM01, file = paste0(path,file01) ,row.names=FALSE, na="",col.names=TRUE, sep=";")

################################################################################
# Ridge regularization
################################################################################

# Compute model - Type Lasso (alpha=0) - for full training set
m02 <- cv.glmnet( X01,Y01, family = "multinomial" , alpha = 0 )

# Predict outcome and adjust output
pre02 <- as.data.frame( predict( m02 , X02, type = "response", s = "lambda.min"))
colnames(pre02) <- c("target1","target2","target3","target4","target5")



# classify data and create corresponding labels
result  <- pre02 == apply( pre02 , 1, max )
predict <- rep(NA,nrow(result))

for(i in 1:nrow(result)){
  pos <- as.numeric(which(result[i,] == TRUE))
  if(length(pos)==0){ pos <- sample(5,1,replace=FALSE)}
  predict[i] <- pos
}

# Prepare output for export
predictM02 <- as.data.frame( cbind( id = idTE , popularity = predict ) )

# Export predictions
file02 <- paste0("pred_F01_M02_RIDGE_",config,"_",Sys.Date(),".csv")
write.table(predictM02, file = paste0(path,file02) ,row.names=FALSE, na="",col.names=TRUE, sep=";")

################################################################################
# Elastic  net regularization
################################################################################

# Compute model - Type Lasso (alpha=0.5) - for full training set
m03 <- cv.glmnet( X01,Y01, family = "multinomial" , alpha = 0.5 )

# Predict outcome and adjust output
pre03 <- as.data.frame( predict( m03 , X02, type = "response", s = "lambda.min"))
colnames(pre03) <- c("target1","target2","target3","target4","target5")

# classify data and create corresponding labels
result  <- pre03 == apply( pre03 , 1, max )
predict <- rep(NA,nrow(result))

for(i in 1:nrow(result)){
  pos <- as.numeric(which(result[i,] == TRUE))
  predict[i] <- pos
}

# Prepare output for export
predictM03 <- as.data.frame( cbind( id = idTE , popularity = predict ) )

# Export predictions
file03 <- paste0("pred_F01_M03_ELANET_",config,"_",Sys.Date(),".csv")
write.table(predictM03, file = paste0(path,file03) ,row.names=FALSE, na="",col.names=TRUE, sep=",")

###############################################################################

###############################################################################