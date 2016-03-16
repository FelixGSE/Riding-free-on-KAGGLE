rm(list=ls()) ; library(h2oEnsemble) ; library(extraTrees) ; library(xgboost) 
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')

h2o.init(nthreads = -1, max_mem_size = "12g")

#data <- read.csv('news_popularity_training.csv') 
data <- read.csv('trainS.csv') 

rows <- scan('outliers_id')
data <- data[-rows,-c(1,2)]

#dummy <- c(13:18,31:38)
#data[,-c(dummy,60)] <- scale(data[,-c(dummy,60)])

#for (c in 1:59) data[,c] <- ifelse(abs(data[,c]-mean(data[,c])) > 3*sd(data[,c]), 1, 0)

y <<- "popularity"
x <<- setdiff(names(data), y)

data <- as.data.frame(data[sample.int(nrow(data)), ])
train <- data[1:24000,]
train0 <- train[which(train$popularity<4),]
test <- data[(24001:nrow(data)),]
test0 <- test[,-60] ; labels <- test[,60]
write.csv(test0, 'testH2O.csv', row.names=F)
test <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/testH2O.csv')

cvSets <- function() {
	all <- as.data.frame(train0[sample.int(nrow(train0)), ])
	train1 <<- all[(1:floor(nrow(all)/1)),]
	write.csv(train1, 'trainH2O.csv', row.names=F)
	train <<- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/trainH2O.csv')
	train[,y] <<- as.factor(train[,y]) 
}
wProba <- function(fit, cv) {
	pred <- predict(fit, test)
	pred <- as.data.frame((cbind(as.matrix(pred$p1),as.matrix(pred$p2),as.matrix(pred$p3))))
	p <- cbind(seq(1,nrow(pred),1), pred)
	probas <<- rbind(probas, p)

	probas[,1] <- as.factor(probas[,1])
	for (i in 2:4) {assign(paste0('p',i-1), aggregate(probas[,i], by=list(Category=probas[,1]), FUN=sum))}
	probs <- data.frame(p1=p1[,2],p2=p2[,2],p3=p3[,2])
	prediction <<- as.matrix(apply(probs[, (1:3)], 1, function(x) which(x == max(x))))
	acc <<- length(which(prediction==labels))/length(labels)
	print(acc)
}

probas <- data.frame()

for (i in 1:50) {
	cvSets()
	forest <- h2o.randomForest(x, y, training_frame=train, ntrees = sample(50:500, 1), sample_rate = sample(seq(.5,.8,.05), 1), nbins=sample(10:60, 1), mtries=sample(5:10, 1), max_depth=sample(5:15, 1))
	wProba(forest)

	cvSets()
	gbm <- h2o.gbm(x, y, training_frame=train, ntrees = sample(50:400, 1), nbins=sample(10:60, 1), max_depth=sample(4:12, 1), learn_rate=sample(seq(.1,.7,.01), 1))
	wProba(gbm)

	cvSets() ; layer <- sample(seq(40,100),1)
	deep <- h2o.deeplearning(x, y, training_frame=train, hidden = c(layer,layer,layer,layer,layer), activation=(sample(c("Rectifier", "Tanh", "TanhWithDropout","RectifierWithDropout", "Maxout", "MaxoutWithDropout"),1)), epochs=sample(5:20,1))
	wProba(deep)

	cvSets()
	X <- train1[,-60] ; y0 <- as.factor(train1[,60]) 
	xtra <- extraTrees(X, y0, ntree=sample(seq(5,200,1),1), mtry=sample(seq(3,12,1),1), numThreads=4, numRandomCuts=sample(seq(1,3,1),1), evenCuts=T)
	pred <- data.frame(predict(xtra, test0, probability=T))
	p <- cbind(seq(1,nrow(pred),1), pred) ; colnames(p) <- c('seq(1, nrow(pred), 1)', 'p1', 'p2', 'p3')
	probas <<- rbind(probas, p)
	probas[,1] <- as.factor(probas[,1])
	for (i in 2:4) {assign(paste0('p',i-1), aggregate(probas[,i], by=list(Category=probas[,1]), FUN=sum))}
	probs <- data.frame(p1=p1[,2],p2=p2[,2],p3=p3[,2])
	prediction <- as.matrix(apply(probs[, (1:3)], 1, function(x) which(x == max(x))))
	acc <- length(which(prediction==labels))/length(labels)
	print(acc)

	cvSets()
	X <- as.matrix(train1[,-60]) ; y0 <- as.matrix((train1[,60]))
	dtrain <- xgb.DMatrix(data = X, label = y0)
	param <- list("objective" = "multi:softprob",    # multiclass classification 
	              "num_class" = 4,    # number of classes 
	              "eval_metric" = "merror",    # evaluation metric 
	              "nthread" = 4,   # number of threads to be used 
	              "max_depth" = sample(3:10,1),    # maximum depth of tree 
	              "eta" = sample(seq(.01,.6,.001),1),    # step size shrinkage 
	              "gamma" = sample(seq(.001,2,.001),1),    # minimum loss reduction 
	              #"subsample" = .05,    # part of data instances to grow tree 
	              #"colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
	              "lambda"=sample(1:2,1)
	              #"min_child_weight" = 1  # minimum sum of instance weight needed in a child 
	              )
	bst <- xgboost(param=param, data=dtrain, nrounds=sample(40:300,1), prediction=T)
	pred <- matrix(predict(bst, as.matrix(test0)), ncol = 4, byrow = T)[,-1]
	p <- cbind(seq(1,nrow(pred),1), pred) ; colnames(p) <- c('seq(1, nrow(pred), 1)', 'p1', 'p2', 'p3')
	probas <<- rbind(probas, p)
	probas[,1] <- as.factor(probas[,1])
	for (i in 2:4) {assign(paste0('p',i-1), aggregate(probas[,i], by=list(Category=probas[,1]), FUN=sum))}
	probs <- data.frame(p1=p1[,2],p2=p2[,2],p3=p3[,2])
	prediction <- as.matrix(apply(probs[, (1:3)], 1, function(x) which(x == max(x))))
	acc <- length(which(prediction==labels))/length(labels)
	print(acc)
}