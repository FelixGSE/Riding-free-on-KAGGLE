library(nnet); library(caret); library(DMwR); library(pmlr); library(rusboost); library(adabag); library(lattice); library(stats); library(probsvm); library(randomForest); library(gbm)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')
train <- read.csv('news_popularity_training.csv')
popularity <- train$popularity
test  <- read.csv('news_popularity_test.csv')  
sample  <- read.csv('news_popularity_sample.csv') 
train <- train[, !(names(train) %in% c('id','url'))]
test <- test[, !(names(test) %in% c('id','url'))]
test <- cbind(test,1)
train$popularity <- as.factor(train$popularity)

all <- formula(popularity~.)

gboost <- gbm(all, n.trees = 1500, data=train)
apply(predict(gboost, type='response', n.trees = 100, newdata=test), 1, which.max)
out <- as.data.frame(cbind(test$id, as.matrix(as.numeric(pred))))
colnames(out) <- c('id','popularity')
write.csv(out, paste0('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Work/Output/OutputData/Thomas/predGboost', substr(Sys.time(),1,10), '.csv'), row.names=F)