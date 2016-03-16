#accuracy by K-fold validation

setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Work/Output/OutputData/Thomas/')

acc <- function(method, model, k=5) {
	n <- nrow(model)
	model <- as.data.frame(model[sample.int(n), ]) #shuffle
	names(model)[names(model)=='V1'] <- 'popularity'
	acc <- c()
	conf <- list()
	sapply(1:k, function(i) {
		s <- n*(i-1)/k+1 ; e <- n*i/k
		CV <- model[-(s:e),] ; cv <- model[(s:e),]
		CV$popularity  <- as.factor(CV$popularity) ; cv$popularity <- factor(cv$popularity, levels=levels(CV$popularity))
		pred <- predict(method(CV), newdata=cv[,!(names(cv)=='popularity')], type='class')
		acc <<- c(acc, length(which(pred==cv$popularity))/nrow(cv))
		conf <<- list(conf, caret::confusionMatrix(pred,cv$popularity))
	})
	write.table(t(c(as.character(as.list(match.call())$method), as.character(as.list(match.call())$model), mean(acc))), 'accuracies.csv', append=T, col.names=F, row.names=F)
	list(conf, mean(acc))
}