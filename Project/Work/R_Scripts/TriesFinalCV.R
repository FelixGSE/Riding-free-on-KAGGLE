rm(list=ls()) ; library(h2oEnsemble) ; library(extraTrees) ; library(xgboost) ; library(plyr) ;library(C50)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')

#set a local H2O server
h2o.init(nthreads = 4, max_mem_size = "14g")

#load the training set, removing the outliers referenced in outliers_id
data <- read.csv('news_popularity_training.csv')[-scan('outliers_id'),]
#removing the columns ID and URL
data <- as.data.frame(data[sample.int(nrow(data)),-c(1,2)])

y <<- "popularity" ; x <<- setdiff(names(data), y)

#preparing the training set for cross validation (one fold because the model is very heavy, but tests with more folds are done afterwards)
train <- data[1:24000,]
#removing popularity levels 4 and 5. We think they might 'pollute' our inference
train0 <<- train[which(train$popularity<4),]

#test data with all popularity levels
test <- data[(24001:nrow(data)),]
test0 <- test[,-60] ; labels <- test[,60]
write.csv(test0, 'testH2O.csv', row.names=F)
#the H2O methods solely accept data resulting from the following operation
test <<- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/testH2O.csv')

#function used without argument to get a random subset of 60% to 75% of the training set. It should contribute to reducing the correlation between our different models.
cvSets <- function(subsample=sample(seq(.05,.2,.01),1)) {
	train1 <<- train0[sample(1:nrow(train0), floor(subsample*nrow(train0))),]
	write.csv(train1, 'trainH2O.csv', row.names=F)
	train <<- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/trainH2O.csv')
	train[,y] <<- as.factor(train[,y]) 
}

#function taking for argument a fitted model
probAcc <- function(fit) {
	#some methods have different prediction outputs than others
	if(deparse(substitute(fit))%in%c('forest','gbm','deep')) {
		pred <- predict(fit, test)
		pred <- as.data.frame((cbind(as.matrix(pred$p1),as.matrix(pred$p2),as.matrix(pred$p3))))
	} else { if (deparse(substitute(fit)) == 'xtra') {
		pred <- data.frame(predict(fit, test0, probability=T))
		pred <- as.data.frame((cbind(as.matrix(pred[,1]),as.matrix(pred[,2]),as.matrix(pred[,3]))))
	} else { if (deparse(substitute(fit)) == 'c5') {
		pred <- predict(fit, test0, type='prob')
		pred <- as.data.frame((cbind(as.matrix(pred[,1]),as.matrix(pred[,2]),as.matrix(pred[,3]))))
	} else {
		pred <- matrix(predict(fit, as.matrix(test0)), ncol = 4, byrow = T)[,-1]
		pred <- as.data.frame((cbind(as.matrix(pred[,1]),as.matrix(pred[,2]),as.matrix(pred[,3]))))
	}}}
	#computing the probability of each test data to belong to class 1, 2, or 3
	pred <- cbind(as.factor(1:nrow(pred)), pred) ; colnames(pred) <- c('id', 'p1','p2','p3')
	#collecting incrementally all the model's proabilities
	probas <<- rbind(probas, pred) 

	#compute the predictions emerging from the blending of the accumulated probabilities, and print the up-to-date accuracy
	for (i in 2:ncol(probas)) {assign(paste0('p',i-1), aggregate(probas[,i], by=list(Category=probas[,1]), FUN=sum))}
	probs <- data.frame(p1=p1[,2],p2=p2[,2],p3=p3[,2])
	prediction <<- as.matrix(apply(probs[,1:3], 1, function(x) which(x == max(x))))
	print(length(which(prediction==labels))/length(labels))
}

probas <- data.frame()
#the following models are tuned randomly within a reasonable range of values, according to multiple tries
for (i in 1:50) {
	#H2O's fast implementation of random forests, allowing more tuning
	cvSets()
	forest <- h2o.randomForest(x, y, training_frame=train, ntrees = sample(20:200, 1), sample_rate = sample(c(.65,.75,.85), 1), nbins=sample(seq(20,60,10), 1), mtries=sample(2:9, 1), max_depth=sample(1:15, 1))
	probAcc(forest)

	#H2O's implementation of gradient boosting machines
	cvSets()
	gbm <- h2o.gbm(x, y, training_frame=train, ntrees = sample(20:200, 1), nbins=sample(seq(20,60,10), 1), max_depth=sample(1:15, 1), learn_rate=sample(seq(.1,.7,.01), 1))
	probAcc(gbm)

	#H2O's implementation of neural networks, allowing notably to configure precisely the network's architecture
	cvSets() ; layer <- sample(seq(40,200,5),1)
	deep <- h2o.deeplearning(x, y, training_frame=train, hidden = c(layer,layer,layer,layer), activation=(sample(c("Rectifier", "Tanh", "TanhWithDropout","RectifierWithDropout", "Maxout", "MaxoutWithDropout"),1)), epochs=sample(seq(5,25,5),1))
	probAcc(deep)

	#extremely randomized trees
	cvSets() ; X <- train0[,-60] ; y0 <- as.factor(train0[,60]) 
	xtra <- extraTrees(X, y0, ntree=sample(20:200,1), mtry=sample(2:8,1), numThreads=4, numRandomCuts=sample(1:3,1), evenCuts=T)
	probAcc(xtra)

	#extreme gradient boosting
	cvSets() ; X <- as.matrix(train1[,-60]) ; y0 <- as.matrix((train1[,60])) ; dtrain <- xgb.DMatrix(data = X, label = y0)
	bst <- xgboost(data=dtrain, nrounds=sample(20:200,1), objective = "multi:softprob", num_class = 4, max_depth = sample(1:15,1), eta = sample(seq(.01,.6,.001),1), gamma = sample(seq(.001,2,.001),1), lambda=sample(1:2,1), prediction=T)
	probAcc(bst)

	#ensemble rules, C5 algorithm
	cvSets()
	c5 <- C5.0(as.factor(popularity)~., data=train1, trials=sample(30:80,1))
	probAcc(c5)
}