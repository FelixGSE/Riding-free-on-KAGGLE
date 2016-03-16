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
data[,60] <- factor(data[,60])

train <- data[1:24000,] ; test <- data[24001:nrow(data),]
#labels <- as.factor(test[,60])

#test <- read.csv('testS.csv') 

#rf <- foreach(ntree=rep(450, 3), .combine=combine, .packages='randomForest') %dopar%
#rf <- randomForest(popularity~., data=train, do.trace=T, mtry=7)
rpord <- rpartScore::rpartScore(popularity ~ ., data = train)
summary(glm <- MASS::glm.nb(popularity ~ ., data=train))

pred <- predict(rpord, test, type='response')
#print(length(which(pred==test[,60]))/length(test[,60]))

tapply(exp(pred)/3-.3, test[,60], summary)
plot(pred, test[,60])

pred <- round(floor((pred+1)^3),0)
print(length(which(pred==test[,60]))/length(test[,60]))
hist(train$popularity)

#submit <- data.frame(id=test$id,popularity=pred)
#write.csv(submit, '/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Work/Output/OutputData/Thomas/lazyyy.csv', row.names=F)