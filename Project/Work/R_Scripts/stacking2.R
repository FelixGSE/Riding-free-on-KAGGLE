#CHECK PAPER OPTIMIZE GBM

library(readr) ; library(h2oEnsemble) ; library(randomForest) ; library(caret) ; library(nnet) ; library(extraTrees)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')

localH2O <-  h2o.init(nthreads = -1, max_mem_size = "12g")  
train <- read.csv('news_popularity_training.csv') 
rows <- scan('outliers_id')
train <- train[-rows,-c(1,2)]
y <<- "popularity"
x <<- setdiff(names(train), y)

train <- as.data.frame(train[sample.int(nrow(train)), ])
write.csv(train[,1:59], 'traintestH2O.csv', row.names=F)
traintest <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/traintestH2O.csv')
test0 <- train[(24001:nrow(train)),]
labels <- as.data.frame(test0[,y])
write.csv(test0, 'testH2O.csv', row.names=F)
test <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/testH2O.csv')
train0 <- train[(1:24000),]
write.csv(train0, 'trainH2O.csv', row.names=F)
train <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/trainH2O.csv')
train[,y] <- as.factor(train[,y])

PRED <- matrix(,nrow(traintest),1) ; bindpred <- function(fit) PRED <<- data.frame(PRED, as.matrix(predict(fit, traintest)$predict))

#MODELS
forest1 <- h2o.randomForest(x, y, training_frame=train, ntrees = 500) ; bindpred(forest1) ; PRED <- PRED[,-1]
#forest2 <- h2o.randomForest(x, y, training_frame=train, ntrees = 150, sample_rate = 0.75) ; bindpred(forest2)
#forest3 <- h2o.randomForest(x, y, training_frame=train, ntrees = 250, nbins = 50, balance_classes = T) ; bindpred(forest3)
#forest4 <- h2o.randomForest(x, y, training_frame=train, ntrees = 150, sample_rate = 0.85)
gbm1 <- h2o.gbm(x, y, training_frame=train, ntrees = 500) ; bindpred(gbm1)
#gbm2 <- h2o.gbm(x, y, training_frame=train, ntrees = 150, nbins = 50)
#gbm3 <- h2o.gbm(x, y, training_frame=train, ntrees = 50, max_depth = 10) ; bindpred(gbm3)
#gbm4 <- h2o.gbm(x, y, training_frame=train, ntrees = 150, col_sample_rate = 0.7) ; bindpred(gbm4)
#gbm5 <- h2o.gbm(x, y, training_frame=train, ntrees = 150)
#gbm6 <- h2o.gbm(x, y, training_frame=train, ntrees = 150, col_sample_rate = 0.6)
#gbm7 <- h2o.gbm(x, y, training_frame=train, ntrees = 150, max_depth = 3)
#gbm8 <- h2o.gbm(x, y, training_frame=train, ntrees = 150, col_sample_rate = 0.8)
data <- read.csv('news_popularity_training.csv') 
rows <- scan('outliers_id')
data <- as.matrix(data[-rows,-c(1,2)])
train <- as.data.frame(data[c(1:24000),])
cv <- as.data.frame(data[c(24001:nrow(data)),])
X <- train[,-60] ; y <- as.factor(train[,60])
Xcv<- cv[,-60] ; ycv <- as.factor(cv[,60])
xtra1 <- extraTrees(X, y, ntree=200)
PRED <<- data.frame(PRED, predict(xtra1, traintest))
xtra2 <- extraTrees(X, y, ntree=200)
PRED <<- data.frame(PRED, predict(xtra2, traintest))
#deep1 <- h2o.deeplearning(x, y, training_frame=train, hidden = c(200,200,200))
#deep2 <- h2o.deeplearning(x, y, training_frame=train, hidden = c(35, 17))

PREDtrain <- data.frame(PRED[1:24000,], train0)
PREDtrain$popularity <- as.factor(PREDtrain$popularity)
PREDtest <- data.frame(PRED[24001:nrow(PRED),], test0)
PREDtest$popularity <- as.factor(PREDtest$popularity)
PREDtest[1:ncol(PRED),] <- lapply(PREDtest[1:ncol(PRED),], factor, levels=c(1,2,3,4,5))

#meta <- randomForest(popularity~., data=PREDtrain[,1:65], do.trace=T,ntree=500)
meta <- nnet(popularity~., data=PREDtrain[,c(1:ncol(PRED),ncol(PREDtrain))], size = 2, rang = 0.1, decay = 5e-4, maxit = 1500)
meta <- multinom(popularity~., data=PREDtrain[,c(1:ncol(PRED),ncol(PREDtrain))])
prediction <- predict(meta, PREDtest[,-ncol(PREDtest)], type="class")

label <- as.factor(as.matrix(labels))
prediction <- as.factor(prediction)

confusionMatrix(prediction, label)

#id <- read.csv('news_popularity_test.csv')$id
#submit <- as.matrix(cbind(id, prediction))
#colnames(submit)[2] <- 'popularity'
#head(submit)

#submit <- data.frame(id = submit[,1], popularity = submit[,2])

#write.csv(submit, '/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Work/Output/OutputData/Thomas/manualEnsembleMany190200-2.csv', row.names=F)