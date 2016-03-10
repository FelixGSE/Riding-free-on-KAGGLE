library(extraTrees) ; library(caret)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')
data <- read.csv('news_popularity_training.csv') 
rows <- scan('outliers_id')
data <- as.matrix(data[-rows,-c(1,2)])
train <- as.data.frame(data[c(1:24000),])
cv <- as.data.frame(data[c(24001:nrow(data)),])
X <- train[,-60] ; y <- as.factor(train[,60])
Xcv<- cv[,-60] ; ycv <- as.factor(cv[,60])

#fit <- extraTrees(X, y, ntree=400)
#pred<-predict(fit, Xcv)


#https://cran.r-project.org/web/packages/rFerns/rFerns.pdf

library(rFerns)
fit <- rFerns(X, y, ferns = 500)
pred<-predict(fit, Xcv)

length(which(pred==ycv))/length(ycv)
