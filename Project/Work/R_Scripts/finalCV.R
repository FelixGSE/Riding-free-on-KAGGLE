rm(list=ls()) ; library(h2oEnsemble) ; library(extraTrees) ; library(xgboost) ; library(plyr)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')
h2o.init(nthreads = 4, max_mem_size = "14g")

data <- read.csv('news_popularity_training.csv')[-scan('outliers_id'),]
data <- as.data.frame(data[sample.int(nrow(data)),-c(1,2)])

y <<- "popularity" ; x <<- setdiff(names(data), y)

train <- data[1:24000,]
train0 <<- train[which(train$popularity<4),]

test <- data[(24001:nrow(data)),]
test0 <- test[,-60] ; labels <- test[,60]
write.csv(test0, 'testH2O.csv', row.names=F)
test <<- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/testH2O.csv')

cvSets <- function(subsample=.66) {
	train1 <<- train0[sample(1:nrow(train0), floor(subsample*nrow(train0))),]
	write.csv(train1, 'trainH2O.csv', row.names=F)
	train <<- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/trainH2O.csv')
	train[,y] <<- as.factor(train[,y]) 
}

probAcc <- function(fit) {
	if(deparse(substitute(fit))%in%c('forest','gbm','deep','glm')) {
		pred <- predict(fit, test)
		pred <- as.data.frame((cbind(as.matrix(pred$p1),as.matrix(pred$p2),as.matrix(pred$p3))))
	} else { if (deparse(substitute(fit)) == 'xtra') {
		pred <- data.frame(predict(fit, test0, probability=T))
		pred <- as.data.frame((cbind(as.matrix(pred[,1]),as.matrix(pred[,2]),as.matrix(pred[,3]))))
	} else {
		pred <- matrix(predict(fit, as.matrix(test0)), ncol = 4, byrow = T)[,-1]
		pred <- as.data.frame((cbind(as.matrix(pred[,1]),as.matrix(pred[,2]),as.matrix(pred[,3]))))
	}}
	pred <- cbind(as.factor(1:nrow(pred)), pred) ; colnames(pred) <- c('id', 'p1','p2','p3')
	probas <<- rbind(probas, pred)

	for (i in 2:ncol(probas)) {assign(paste0('p',i-1), aggregate(probas[,i], by=list(Category=probas[,1]), FUN=sum))}
	probs <- data.frame(p1=p1[,2],p2=p2[,2],p3=p3[,2])
	prediction <<- as.matrix(apply(probs[,1:3], 1, function(x) which(x == max(x))))
	print(length(which(prediction==labels))/length(labels))
}

probas <- data.frame()
for (i in 1:50) {
	cvSets()
	forest <- h2o.randomForest(x, y, training_frame=train, ntrees = sample(50:400, 1), sample_rate = sample(seq(.5,.8,.05), 1), nbins=sample(5:60, 1), mtries=sample(5:10, 1), max_depth=sample(5:15, 1))
	probAcc(forest)

	cvSets()
	gbm <- h2o.gbm(x, y, training_frame=train, ntrees = sample(50:400, 1), nbins=sample(5:60, 1), max_depth=sample(5:15, 1), learn_rate=sample(seq(.01,.7,.01), 1))
	probAcc(gbm)

	#cvSets()
	#glm <- h2o.glm(x, y, training_frame=train, max_iterations = sample(30:150, 1), beta_epsilon = 0, solver = sample(c("IRLSM", "L_BFGS"),1), standardize = TRUE, family = 'multinomial', alpha = sample(seq(.4,.6,.05), 1), lambda = sample(seq(5e-06, 3e-05, 1e-06),1), nlambdas = -1, lambda_min_ratio = -1, beta_constraints = NULL, offset_column = NULL, max_active_predictors = -1)
	#probAcc(glm)

	cvSets() ; layer <- sample(40:200,1)
	deep <- h2o.deeplearning(x, y, training_frame=train, hidden = c(layer,layer,layer,layer), activation=(sample(c("Rectifier", "Tanh", "TanhWithDropout","RectifierWithDropout", "Maxout", "MaxoutWithDropout"),1)), epochs=sample(5:30,1))
	probAcc(deep)

	cvSets() ; X <- train0[,-60] ; y0 <- as.factor(train0[,60]) 
	xtra <- extraTrees(X, y0, ntree=sample(5:200,1), mtry=sample(seq(3,12,1),1), numThreads=4, numRandomCuts=sample(1:3,1), evenCuts=T)
	probAcc(xtra)

	cvSets() ; X <- as.matrix(train1[,-60]) ; y0 <- as.matrix((train1[,60])) ; dtrain <- xgb.DMatrix(data = X, label = y0)
	bst <- xgboost(data=dtrain, nrounds=sample(5:200,1), objective = "multi:softprob", num_class = 4, max_depth = sample(3:12,1), eta = sample(seq(.01,.6,.001),1), gamma = sample(seq(.001,2,.001),1), lambda=sample(1:2,1), prediction=T)
	probAcc(bst)
}