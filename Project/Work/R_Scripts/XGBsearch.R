library(xgboost) ; library(randomForest)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')

train <- read.csv('trainS.csv') 
rows <- scan('outliers_id')
train <- train[-rows,-c(1,2)]

test <- read.csv('testS.csv') 
test <- test[,-c(1,2)]

dtrain <- xgb.DMatrix(data = as.matrix(train[,-60]), label = as.matrix(train[,60]))
param <- list("objective" = "multi:softprob", 
	   "num_class" = 6, 
	   "eval_metric" = "merror",    # evaluation metric 
              "nthread" = 4,   # number of threads to be used 
              "max_depth" = sample(seq(3,12),1),    # maximum depth of tree 
              "eta" = sample(seq(.01,.6,.001),1),    # step size shrinkage 
              "gamma" = sample(seq(.001,2,.001),1),    # minimum loss reduction 
              #"subsample" = .05,    # part of data instances to grow tree 
              #"colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              "lambda"=sample(c(1,2),1)
	   #"min_child_weight" = 1  # minimum sum of instance weight needed in a child 
	)
bst <- xgboost(param=param, data=dtrain, nrounds=300)
pred <- cbind(1:nrow(test), matrix(predict(bst, as.matrix(test)), ncol = 6, byrow = T)[,-1])

train$popularity <- as.factor(train$popularity)
rf <- randomForest(popularity~., data=train, do.trace=T, ntree=1000, mtry=7, nodesize=5)
pred <- rbind(pred, cbind(1:nrow(test), predict(rf, test, type='prob')))

for (i in 2:6) {assign(paste0('p',i-1), aggregate(pred[,i], by=list(Category=pred[,1]), FUN=sum))}
probs <- data.frame(p1=p1[,2],p2=p2[,2],p3=p3[,2],p4=p4[,2],p5=p5[,2])
prediction <- as.matrix(apply(probs[, (1:3)], 1, function(x) which(x == max(x))))
submit <- data.frame(id=test$id,popularity=prediction)
write.csv(submit, '/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Work/Output/OutputData/Thomas/lazyyy.csv', row.names=F)