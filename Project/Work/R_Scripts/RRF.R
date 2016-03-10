library(RRF) ; library(dummies) ; library(caret)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')

#PREPARE DATA
data <- read.csv('news_popularity_training.csv') 
rows <- scan('outliers_id')
data <- data[-rows,-c(1,2)]
data <- as.data.frame(data[sample.int(nrow(data)), ])
X <- data[,-ncol(data)]
y <- data[,ncol(data)] 

rows <- 1:24000
Xtr <- X[rows,] ; ytr <- as.factor(y[rows])
Xte <- X[-rows,] ; labels <- as.factor(y[-rows])

rf <- RRF(Xtr, ytr, flagReg = 0, do.trace=T)
impRF <- rf$importance
impRF <- impRF[,"MeanDecreaseGini"]
imp <- impRF/(max(impRF))#normalize the importance score
gamma <- 0.3
coefReg <- (1-gamma)+gamma*imp #weighted average
grrf <- RRF(Xtr, ytr, coefReg=coefReg, flagReg=1, do.trace=T)
pred <- predict(rf, Xte)

confusionMatrix(pred, labels)