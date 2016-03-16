




#XGBOOST





llibrary(readr) ; library(h2oEnsemble) 
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')

localH2O <-  h2o.init(nthreads = -1) 

train <- read.csv('news_popularity_training.csv') 

rows <- scan('outliers_id')
train <- train[-rows,-c(1,2)]
y <<- "popularity"
x <<- setdiff(names(train), y)

#FOR ENSEMBLE ACCURACY
train <- as.data.frame(train[sample.int(nrow(train)), ])
test <- train[(24001:nrow(train)),]
labels <- as.data.frame(test[,y])
write.csv(test, 'testH2O.csv', row.names=F)

traintest <- rbind(train,test)
write.csv(test, 'testH2O.csv', row.names=F)

test <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/testH2O.csv')

traintest <- rbind(train,test)

train0 <- train[(1:24000),]














http://www.slideshare.net/0xdata/h2o-world-ensembles-with-erin-ledell


library(readr) ; library(h2oEnsemble) ; library(extraTrees)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')
train <- read.csv('news_popularity_training.csv') ; train$popularity <- as.factor(train$popularity)
rows <- scan('outliers_id')
train <- train[-rows,-c(1,2)]
localH2O <-  h2o.init(nthreads = -1)  

train <- as.data.frame(train[sample.int(nrow(train)), ])

write.csv(train[(1:24000),], 'trainAllH2O.csv', row.names=F)
trainAll <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/trainAllH2O.csv')
write.csv(train[(1:12000),], 'train1H2O.csv', row.names=F)
train1 <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/train1H2O.csv')
write.csv(train[(12001:24000),], 'train2H2O.csv', row.names=F)
train2 <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/train2H2O.csv')
write.csv(train[(24001:29912),], 'testH2O.csv', row.names=F)
test <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/testH2O.csv')

y <- "popularity"
x <- setdiff(names(train), y)
trainAll[,y] <- as.factor(trainAll[,y])  
train1[,y] <- as.factor(train1[,y])  
train2[,y] <- as.factor(train2[,y])  
test[,y] <- as.factor(test[,y])  

forest <- h2o.randomForest(x, y, training_frame=trainAll, ntrees=400)
predforest <- as.data.frame(predict(forest, trainAll)$predict)[,1]
gbm <- h2o.gbm(x, y, training_frame=trainAll, ntrees=400)
predgbm <- as.data.frame(predict(gbm, trainAll)$predict)[,1]

train <- read.csv('trainAllH2O.csv')
trainPred <- data.frame(train, predgbm, predforest)
write.csv(trainPred, 'trainPredH2O.csv', row.names=F)
trainPred <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/trainPredH2O.csv') ; trainPred$popularity <- as.factor(trainPred$popularity) ; trainPred$predforest <- as.factor(trainPred$predforest) ; trainPred$predgbm <- as.factor(trainPred$predgbm)

meta <- h2o.deeplearning(x, y, training_frame=trainPred)
prediction <- as.data.frame(predict(meta, test)$predict)[,1]

labels <- as.data.frame(test[,c(y)])[,1]
length(which(prediction==labels))/length(labels)