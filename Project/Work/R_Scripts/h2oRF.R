




#check https://github.com/h2oai/h2o-2/blob/master/R/examples/Kaggle/TradeShift.R









library(readr) ; library(h2oEnsemble) 
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')
train <- read.csv('news_popularity_training.csv') ; train$popularity <- as.factor(train$popularity)
rows <- scan('outliers_id')
train <- train[-rows,-c(1,2)]

localH2O <-  h2o.init(nthreads = -1)  # Start an H2O cluster with nthreads = num cores on your machine

train <- as.data.frame(train[sample.int(nrow(train)), ])
write.csv(train[(1:24000),], 'trainH2O.csv', row.names=F)
train <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/trainH2O.csv')
write.csv(train[(24001:29912),], 'testH2O.csv', row.names=F)
test <- h2o.importFile('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/testH2O.csv')

y <- "popularity"
x <- setdiff(names(train), y)

train[,y] <- as.factor(train[,y])  
test[,y] <- as.factor(test[,y])  

#gbm <- h2o.gbm(x, y, training_frame=train)
err <- function(fit) {fit@model$scoring_history$training_classification_error[length(fit@model$scoring_history$training_classification_error)]}
#err(gbm)

wProba <- function(fit,test) {
	predictions <- as.data.frame(predict(fit, test)$predict)[,1]
	labels <- as.data.frame(test[,c(y)])[,1]
	predictions <- factor(predictions, levels=levels(labels))
	#acc <- length(which(predictions==labels))/length(labels)
	#DO ACCURACY PER CLASS THEN IN MATRIX
	p <- cbind(as.matrix(pred$p1),as.matrix(pred$p2),as.matrix(pred$p3),as.matrix(pred$p4),as.matrix(pred$p5))/(1-3*acc) #weighting
	probas <<- rbind(probas, cbind(row(probas)[,1], p))
}

trees <- seq(100,800,1)
rows <- function() {
	r <- seq(1:nrow(train))
	tr <-sample(r, 20000)
	list(CV=tr, cv=which(!(r%in%tr)))
}
probas <- data.frame()
for (i in 1:2) {
	r <- rows() ; CV <- train[r$CV,] ; cv <- train[r$cv,]
	forest <- h2o.randomForest(x, y, training_frame=CV, ntrees=sample(trees, 1))
	wProba(forest,cv)
}
for (i in 1:4) {
	r <- rows() ; CV <- train[r[1],] ; cv <- train[r[2],]
	gbm <- h2o.gbm(x, y, training_frame=CV, ntrees=sample(trees, 1))
	wProba(gbm,cv)
}
r <- rows() ; CV <- train[r[1],] ; cv <- train[r[2],]
deep <- h2o.deeplearning(x, y, training_frame=CV, hidden = c(35,17))
wProba(deep,cv)

for (i in 1:5) {assign(paste0('p',i), aggregate(probas[,i], by=list(Category=probas$id), FUN=sum))}
probs <- data.frame(p1=p1[,2],p2=p2[,2],p3=p3[,2],p4=p4[,2],p5=p5[,2])
prediction <- apply(probs[, (2:5)], 1, function(x) which(x == max(x)))

labels <- as.data.frame(test[,c(y)])[,1]
length(which(prediction==labels))/length(labels)