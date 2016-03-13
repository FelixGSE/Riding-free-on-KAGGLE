rm(list=ls(all=T)); library(nnet); library(caret); library(DMwR); library(pmlr); library(rusboost); library(adabag); library(lattice); library(stats); library(probsvm); library(randomForest)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')
train <- read.csv('news_popularity_training.csv')
popularity <- train$popularity
test  <- read.csv('news_popularity_test.csv')  
id <- test$id
sample  <- read.csv('news_popularity_sample.csv') 
train <- train[, !(names(train) %in% c('id','url'))]
test <- test[, !(names(test) %in% c('id','url'))]
test <- cbind(test,1)
train$popularity <- as.factor(train$popularity)

train <- train[,2:60]
chan <- as.matrix(train[,13:17])
day <- as.matrix(train[,31:37])
conti <- as.matrix(train[,c(1:11, 18:29, 38:58)])
dayconti <- c()
for (i in 1:ncol(day)){dayconti <- cbind(dayconti, day[,i]*conti)}
chanconti <- c()
for (i in 1:ncol(chan)){chanconti <- cbind(chanconti, chan[,i]*conti)}
train <- cbind(conti, day, chan, dayconti, chanconti, popularity)
colnames(train) <- c(paste0('V', seq(1,ncol(train),1)))
train <- as.data.frame(train)
train[,585] <- as.factor(train[,585])

forest <- randomForest(V585~., data=train)

test <- test[,2:60]
chan <- as.matrix(test[,13:17])
day <- as.matrix(test[,31:37])
conti <- as.matrix(test[,c(1:11, 18:29, 38:58)])
dayconti <- c()
for (i in 1:ncol(day)){dayconti <- cbind(dayconti, day[,i]*conti)}
chanconti <- c()
for (i in 1:ncol(chan)){chanconti <- cbind(chanconti, chan[,i]*conti)}
test <- cbind(conti, day, chan, dayconti, chanconti, 1)
colnames(test) <- c(paste0('V', seq(1,ncol(test),1)))
pred <- predict(forest, test, 'class')

out <- as.data.frame(cbind(id, pred))
colnames(out) <- c('id','popularity')
write.csv(out, paste0('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Work/Output/OutputData/Thomas/predForestMoreVar', substr(Sys.time(),1,10), '.csv'), row.names=F)