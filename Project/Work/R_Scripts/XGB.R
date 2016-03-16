library(xgboost) ; library(caret)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')
train <- read.csv('trainS.csv') 
rows <- scan('outliers_id')
train <- as.matrix(train[-rows,-c(1,2)])

y <- as.integer(train[,60])
X <- train[,-60]

classes = 6
# xgboost parameters
param <- list("objective" = "multi:softprob",    # multiclass classification 
              "num_class" = classes,    # number of classes 
              "eval_metric" = "merror",    # evaluation metric 
              "nthread" = 4,   # number of threads to be used 
              "max_depth" = 5,    # maximum depth of tree 
              "eta" = .005,    # step size shrinkage 
              "gamma" = .2,    # minimum loss reduction 
              #"subsample" = .05,    # part of data instances to grow tree 
              #"colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              "lambda"=2
              #"min_child_weight" = 1  # minimum sum of instance weight needed in a child 
              )

bst.cv <- xgb.cv(param=param, data=X, label=y, nfold=3, nrounds=1500, prediction=T)
#pred.cv = matrix(bst.cv$pred, nrow=length(bst.cv$pred)/classes, ncol=classes)
#pred.cv = max.col(pred.cv, "last")
#pred.cv <- factor(pred.cv, levels=levels(factor(y+1)))
#confusionMatrix(factor(y+1), pred.cv)