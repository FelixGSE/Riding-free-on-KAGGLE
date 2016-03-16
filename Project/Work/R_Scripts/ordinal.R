#http://www.ats.ucla.edu/stat/r/dae/ologit.htm

library(doParallel); library(foreach); library(randomForest) ; registerDoParallel(cores=3)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')

data <- read.csv('trainS.csv')[-scan('outliers_id'),]
data$search <- sqrt(data$search)

data$ones <- 1
data$day <- substr(data$url, 21, 30)
artday <- rowsum(data$ones, data$day) ; artday <- data.frame(day=rownames(artday), artday)
data <- plyr::join(data, artday, by = 'day', type = "left", match = "all")
data$revprop <- data$ones/data$artday

searchday <- rowsum(data$search, data$day) ; searchday <- data.frame(day=rownames(searchday), searchday)
data <- plyr::join(data, searchday, by = 'day', type = "left", match = "all")
data$rel_search <- data$search/data$searchday

data <- as.data.frame(data[sample.int(nrow(data)),-c(1,2,64,65)])
#data[,60] <- as.factor(data[,60])

train <- data[1:24000,] ; test <- data[24001:nrow(data),]
train$popularity <- as.factor(train$popularity)

(m <- MASS::polr(popularity~search+searchday+revprop, data = train, Hess=TRUE))
m <- MCMCpack::MCMCoprobit(popularity~., data=train)

pred <- predict(m, test[,-60])
prediction <<- as.matrix(apply(pred$fit, 1, function(x) which(x == max(x))))
length(which(prediction==test[,60]))/length(test[,60])