#CHECK PAPER OPTIMIZE GBM
options( java.parameters = "-Xmx4g" ) ; library(RWeka)
library(readr) ; library(h2oEnsemble) ; library(randomForest) ; library(caret) ; library(nnet) ; library(extraTrees) ; library(xgboost)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')

data <- read.csv('news_popularity_training.csv') 
data <- as.data.frame(data[sample.int(nrow(data)), ])
rows <- scan('outliers_id')
data <- as.matrix(data[-rows,-c(1,2)])
train <- as.data.frame(data[c(1:24000),])
cv <- as.data.frame(data[c(24001:nrow(data)),])

#XTRA TREES
X <- train[,-60] ; y <- as.factor(train[,60])
Xcv<- cv[,-60] ; ycv <- as.factor(cv[,60])
xtra1 <- extraTrees(X, y, ntree=200)
PRED <<- data.frame(predict(xtra1, data[,-60]))
xtra2 <- extraTrees(X, y, ntree=200)
PRED <<- data.frame(PRED, predict(xtra2, data[,-60]))

#XGBOOST
X <- as.matrix(X) ; y <- as.factor(y)
Xcv<- as.matrix(Xcv) ; ycv <- as.factor(ycv)
dtrain <- xgb.DMatrix(data = X, label = y)
dcv <- xgb.DMatrix(data = Xcv, label = ycv)
param <- list("objective" = "multi:softprob",    # multiclass classification 
              "num_class" = 6,    # number of classes 
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
bst <- xgboost(param=param, data=dtrain, nrounds=300, prediction=T)
PRED <<- data.frame(PRED, as.factor(as.matrix(apply(matrix(predict(bst, as.matrix(data[,-60])), ncol = 6, byrow = T), 1, function(x) which(x == max(x))))-1))

#H2O RF & GBM
h2o.init(nthreads = -1, max_mem_size = "12g")
y <<- "popularity"
x <<- setdiff(names(train), y)
write.csv(data, 'traintestH2O.csv', row.names=F)
traintest <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/traintestH2O.csv')
write.csv(cv, 'testH2O.csv', row.names=F)
test <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/testH2O.csv')
write.csv(train, 'trainH2O.csv', row.names=F)
train <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/trainH2O.csv')
train[,y] <- as.factor(train[,y])

bindpred <- function(fit) PRED <<- data.frame(PRED, as.matrix(predict(fit, traintest)$predict))

#MODELS
forest1 <- h2o.randomForest(x, y, training_frame=train, ntrees = 500) ; bindpred(forest1) 
#forest2 <- h2o.randomForest(x, y, training_frame=train, ntrees = 150, sample_rate = 0.75) ; bindpred(forest2)
#forest3 <- h2o.randomForest(x, y, training_frame=train, ntrees = 250, nbins = 50, balance_classes = T) ; bindpred(forest3)
#forest4 <- h2o.randomForest(x, y, training_frame=train, ntrees = 150, sample_rate = 0.85)
gbm1 <- h2o.gbm(x, y, training_frame=train, ntrees = 500) ; bindpred(gbm1)
#gbm2 <- h2o.gbm(x, y, training_frame=train, ntrees = 150) ; bindpred(gbm2)
#gbm3 <- h2o.gbm(x, y, training_frame=train, ntrees = 50, max_depth = 10) ; bindpred(gbm3)
#gbm4 <- h2o.gbm(x, y, training_frame=train, ntrees = 150, col_sample_rate = 0.7) ; bindpred(gbm4)
#gbm5 <- h2o.gbm(x, y, training_frame=train, ntrees = 150)
#gbm6 <- h2o.gbm(x, y, training_frame=train, ntrees = 150, col_sample_rate = 0.6)
#gbm7 <- h2o.gbm(x, y, training_frame=train, ntrees = 150, max_depth = 3)
#gbm8 <- h2o.gbm(x, y, training_frame=train, ntrees = 150, col_sample_rate = 0.8)
#deep1 <- h2o.deeplearning(x, y, training_frame=train, hidden = c(200,200,200))
#deep2 <- h2o.deeplearning(x, y, training_frame=train, hidden = c(35, 17))

train <- as.data.frame(data[c(1:24000),])
PREDtrain <- data.frame(PRED[1:24000,], train)
PREDtrain$popularity <- as.factor(PREDtrain$popularity)
cv <- as.data.frame(data[c(24001:nrow(data)),])
PREDtest <- data.frame(PRED[24001:nrow(PRED),], cv)
PREDtest$popularity <- as.factor(PREDtest$popularity)
PREDtest[1:ncol(PRED),] <- lapply(PREDtest[1:ncol(PRED),], factor, levels=c(1,2,3,4,5))

meta <- randomForest(popularity~., data=PREDtrain[,c(1:ncol(PRED),ncol(PREDtrain))], do.trace=T,ntree=800, mtry=2)
meta <- nnet(popularity~., data=PREDtrain[,c(1:ncol(PRED),ncol(PREDtrain))], size = 5, rang = 0.1, decay = 5e-4, maxit = 1500)
meta <- multinom(popularity~., data=PREDtrain[,c(1:ncol(PRED),ncol(PREDtrain))])
prediction <- predict(meta, PREDtest[,-ncol(PREDtest)], type="class")

label <- as.factor(cv[,60])
prediction <- as.factor(prediction)

confusionMatrix(prediction, label)

#id <- read.csv('news_popularity_test.csv')$id
#submit <- as.matrix(cbind(id, prediction))
#colnames(submit)[2] <- 'popularity'
#head(submit)

#submit <- data.frame(id = submit[,1], popularity = submit[,2])

#write.csv(submit, '/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Work/Output/OutputData/Thomas/manualEnsembleMany190200-2.csv', row.names=F)