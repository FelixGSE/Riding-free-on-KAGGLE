library(mclust) ; library(fastcluster) ; library(plyr)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data')
test  <- read.csv('news_popularity_test.csv')  
test <- test[,-(1:4)]

d <- dist(test)
hc <- hclust(d)

mc <- Mclust(test, G=5)
mccla <- mc$classification
mccla <- mapvalues(mccla, from = c("4","2","5","1","3"), to = c("5","4","1","3","2"))
testmc <- cbind(mccla, test)
aggregate(x = testmc, by = list(testmc$mccla), FUN = "mean")
submit <- cbind(read.csv('news_popularity_test.csv')$id, mccla)
colnames(submit) <- c('id', 'popularity')
write.csv(submit, paste0('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Work/Output/OutputData/Thomas/predCluster', substr(Sys.time(),1,10), '.csv'), row.names=F)