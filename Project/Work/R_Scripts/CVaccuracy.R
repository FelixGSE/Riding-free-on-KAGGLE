#5-fold cross validation, outputing accuracy rate

ACC <- function(method, model) {

	#method<-mnet ; model <- trivial

	n <- nrow(model)
	train <- as.data.frame(model[sample.int(n), ]) #shuffle
	s <- split(1:n, ceiling(1:5))
 	accuracy <- c()
 	#CONF <- c()
 	for(i in 1:5) {

 		#i <- 1

 		t <- s[[i]]
 		cv <- train[t,]
 		CV <- train[setdiff(unlist(s), t),] ; names(CV)[names(CV)=='V1'] <- 'popularity'
 		cv$pred <- predict(method(CV), type='class', newdata=cv)
 		accuracy <- c(accuracy, length(which(cv$pred==cv$V1))/nrow(cv))
 		#conf <- caret::confusionMatrix(cv$pred, cv$popularity)
 		#CONF <- list(CONF, conf$table)
 	}
	#list(CONF, mean_acc = mean(accuracy))
	mean(accuracy)
}