library(doParallel); library(foreach); library(randomForest) ; registerDoParallel(cores=3)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')

data <- read.csv('trainS.csv') 
rows <- scan('outliers_id')
train <- data[-rows,-c(1,2)]
train[,60] <- as.factor(train[,60])

test <- read.csv('testS.csv') 

rf <- randomForest(popularity~., data=train, do.trace=T, ntree=2000, mtry=7, nodesize=5)

pred <- predict(rf, test)

submit <- data.frame(id=test$id,popularity=pred)
write.csv(submit, '/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Work/Output/OutputData/Thomas/lazyyy.csv', row.names=F)