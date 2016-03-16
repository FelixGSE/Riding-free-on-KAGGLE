setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')
T <- read.csv('searchesT.csv',header=F)
D <- read.csv('searchesD.csv',header=F)

T[is.na(T$V2),] <- D[is.na(T$V2),] 

T[is.na(T$V2),] <- 0

T <- T[-39645,]
T$V1 <- T$V1+1

train <- read.csv('news_popularity_training.csv')
trainS <- data.frame(train, search=T[1:30000,2])
write.csv(trainS, 'trainS.csv', row.names=F)

test <- read.csv('news_popularity_test.csv')
testS <- data.frame(test, search=T[30001:39644,2])
write.csv(testS, 'testS.csv', row.names=F)

library(randomForest)
trainS <- read.csv('trainS.csv')[,-c(1,2)]
rows <- scan('outliers_id')
trainS <- trainS[-rows,-c(1,2)]

trainS$popularity <- as.factor(trainS$popularity)
plot(trainS$popularity,log(trainS$search))
tapply(trainS$search, trainS$popularity, summary)

randomForest(x=trainS[,-60], y=trainS[,60],do.trace=T)