library(C50)

setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')
data <- read.csv('news_popularity_training.csv')[-scan('outliers_id'),]
data <- as.data.frame(data[sample.int(nrow(data)),-c(1,2)])
train <- data[1:24000,]
train$popularity <- as.factor(train$popularity)
test <- data[(24001:nrow(data)),-60]
labels <- data[(24001:nrow(data)),60]

(fit <- C5.0(popularity~., data=train, trials=50))
predictions <- predict(fit, test)

length(which(predictions==labels))/length(labels)