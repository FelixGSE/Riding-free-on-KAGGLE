library(readr) ; library(h2oEnsemble) ; library(extraTrees) ; library(xgboost)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')

h2o.init(nthreads = -1, max_mem_size = "12g")

data <- read.csv('news_popularity_training.csv') 
rows <- scan('outliers_id')
data <- data[-rows,-c(1,2)]
y <<- "popularity"
x <<- setdiff(names(data), y)
data <- as.data.frame(data[sample.int(nrow(data)), ])
train0 <- data[which(data$popularity<4),]

test0 <- read.csv('news_popularity_test.csv') 
test0 <- test0[,-c(1,2)]
test <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/news_popularity_test.csv')

cvSets <- function() {
	all <- as.data.frame(train0[sample.int(nrow(train0)), ])
	train1 <<- all[(1:floor(nrow(all)/1.5)),]
	write.csv(train1, 'trainH2O.csv', row.names=F)
	train <<- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/trainH2O.csv')
	train[,y] <<- as.factor(train[,y]) 
}

wProba <- function(fit) {
	pred <- predict(fit, test)
	pred <- as.data.frame((cbind(as.matrix(pred$p1),as.matrix(pred$p2),as.matrix(pred$p3))))
	p <- cbind(seq(1,nrow(pred),1), pred)
	probas <<- rbind(probas, p)
}

probas <- data.frame()

sample_rates <- seq(.5,.8,.05)
ntrees <- seq(40,400)
nbins <- seq(5,60)
maxdepth <- seq(5,15)
mtries <- seq(5,10)
learn_rate <- seq(.01,.7,.01)

for (i in 1:50) {
	cvSets()
	forest <- h2o.randomForest(x, y, training_frame=train, ntrees = sample(ntrees, 1), sample_rate = sample(sample_rates, 1), nbins=sample(nbins, 1), mtries=sample(mtries, 1), max_depth=sample(maxdepth, 1))
	wProba(forest)

	cvSets()
	gbm <- h2o.gbm(x, y, training_frame=train, ntrees = sample(ntrees, 1), nbins=sample(nbins, 1), max_depth=sample(maxdepth, 1), learn_rate=sample(learn_rate, 1))
	wProba(gbm)

	cvSets() ; layer <- sample(seq(40,200),1)
	deep <- h2o.deeplearning(x, y, training_frame=train, hidden = c(layer,layer,layer,layer), activation=(sample(c("Rectifier", "Tanh", "TanhWithDropout","RectifierWithDropout", "Maxout", "MaxoutWithDropout"),1)), epochs=sample(seq(5,60,5),1))
	wProba(deep)

	#cvSets()
	#X <- as.matrix(train1[,-60]) ; y0 <- as.factor(train1[,60]) 
	#glm <- glmnet(X, y0, family="multinomial", offset=NULL, alpha = sample(c(0,.5,1,2),1), nlambda = sample(seq(30,300,10),1),lambda=NULL,standardize = TRUE, intercept=TRUE, thresh = 1e-07, type.multinomial=sample(c("ungrouped","grouped"),1))
	#pred <- predict(glm, as.matrix(test0), type="response")[,,1]
	#p <- cbind(seq(1,nrow(pred),1), pred) ; colnames(p) <- c('seq(1, nrow(pred), 1)', 'p1', 'p2', 'p3')
	#probas <<- rbind(probas, p)

	cvSets()
	X <- train1[,-60] ; y0 <- as.factor(train1[,60]) 
	xtra <- extraTrees(X, y0, ntree=sample(seq(5,200,1),1), mtry=sample(seq(3,12,1),1), numThreads=4, numRandomCuts=sample(seq(1,3,1),1), evenCuts=T)
	pred <- data.frame(predict(xtra, test0, probability=T))
	p <- cbind(seq(1,nrow(pred),1), pred) ; colnames(p) <- c('seq(1, nrow(pred), 1)', 'p1', 'p2', 'p3')
	probas <<- rbind(probas, p)

	cvSets()
	X <- as.matrix(train1[,-60]) ; y0 <- as.matrix((train1[,60]))
	dtrain <- xgb.DMatrix(data = X, label = y0)
	param <- list("objective" = "multi:softprob",    # multiclass classification 
	              "num_class" = 4,    # number of classes 
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
	bst <- xgboost(param=param, data=dtrain, nrounds=sample(seq(40,300,1),1), prediction=T)
	pred <- matrix(predict(bst, as.matrix(test0)), ncol = 4, byrow = T)[,-1]
	p <- cbind(seq(1,nrow(pred),1), pred) ; colnames(p) <- c('seq(1, nrow(pred), 1)', 'p1', 'p2', 'p3')
	probas <<- rbind(probas, p)

	probas[,1] <- as.factor(probas[,1])
	for (i in 2:4) {assign(paste0('p',i-1), aggregate(probas[,i], by=list(Category=probas[,1]), FUN=sum))}
	probs <- data.frame(p1=p1[,2],p2=p2[,2],p3=p3[,2])
	prediction <<- as.matrix(apply(probs[, (1:3)], 1, function(x) which(x == max(x))))

	print(i)

	Sys.sleep(30)
}

id <- read.csv('news_popularity_test.csv')$id
submit <- as.matrix(cbind(id, prediction))
colnames(submit)[2] <- 'popularity'
head(submit)

submit <- data.frame(id = submit[,1], popularity = submit[,2])

write.csv(submit, '/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Work/Output/OutputData/Thomas/manualEnsembleMany0903.csv', row.names=F)

#REGRESS OVER PROBA VECTOR

write.csv(probas, '/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Work/Output/OutputData/Thomas/probas0903.csv', row.names=F)