library(astroFns)
setwd('/Users/Thomas/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/')
trainURLs <- as.character(read.csv('news_popularity_training.csv')$url)
testURLs <- as.character(read.csv('news_popularity_test.csv')$url)

URLs <- c(trainURLs, testURLs) ; n <- length(URLs)

publish <- as.Date(substr(URLs,21,30), "%Y/%m/%d")
addAweek <- publish+32
publish <- as.character(publish) ; addAweek <- as.character(addAweek)

jpublish <- round(ymd2jd(as.numeric(substr(publish,1,4)),as.numeric(substr(publish,6,7)),as.numeric(substr(publish,9,10))),0) ; jaddAweek <- round(ymd2jd(as.numeric(substr(addAweek,1,4)),as.numeric(substr(addAweek,6,7)),as.numeric(substr(addAweek,9,10))),0)

#q=justin+bieber+daterange:2456944-2456944&tbm=nws

#gPublish <- paste0(substr(publish,6,7),'%2F',substr(publish,9,10),'%2F',substr(publish,1,4))
#gAddAweek <- paste0(substr(addAweek,6,7),'%2F',substr(addAweek,9,10),'%2F',substr(addAweek,1,4))
title <- substr(URLs,32,nchar(URLs)-1) ; title <- gsub('-', '+', title)

searches <- paste0('https://www.google.es/search?q=',title,'+daterange:',jpublish,'-',jaddAweek,'&gws_rd=cr&ei=XWvHVs-ZBMm8swHkhI_IDQ#q=',title,'+daterange:',jpublish,'-',jaddAweek,'&tbm=nws')

results <- matrix(,length(searches),1)
for (i in 10000:11000) {
	source <- tryCatch(readLines(searches[i], warn=F), error=function(e) NA) #flexible for lost connection
	begin <- as.numeric(regexpr('\"resultStats\">Aproximadamente ', source))
	results[i] <- substr(source,begin+30,begin+70)
	Sys.sleep(1)
}
end <- as.numeric(regexpr(' resultados</div><div', results))
results <- substr(results,1,end-1)
results <- as.numeric(gsub('[.]', '', results))

write.csv(results, 'Rsearches.csv')
  
test<-cbind(log(results),read.csv('news_popularity_training.csv')[(20000:25000),62])
test <- cbind(test[(20000:25000),],seq(1,5001,1))

test <- as.data.frame(test) ; test[,2] <- as.factor(test[,2])

require(ggplot2)

ggplot(test, aes(test[,3], test[,1]), alpha=.02, size=2) + geom_point(aes(colour = factor(test[,2])))
qplot(factor=test[,2], test[,1], alpha=.001, size=3)
cor(as.numeric(test[,2]), test[,1],use="complete")
plot(test[,2],test[,1])

searches[20000]
xyplot(Sepal.Width ~ Sepal.Length, iris, groups = iris$Species, pch= 20)
ggplot(test, aes (x = test[,1], y=row(test[,1]), colour = test[,2]))
