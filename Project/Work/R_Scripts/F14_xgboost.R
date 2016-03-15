#packages
install.packages("xgboost")
library("xgboost")

#dataset 
train<- read.csv("/home/didi/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/news_popularity_training.csv",header = TRUE )
test<- read.csv("/home/didi/Dropbox/DS_GROUP/15D012 - Advanced Computational Methods/Data/news_popularity_test.csv", header=TRUE)
#drop the id and url
train<-train[,-1]
train<-train[,-1]
test<-test[,-1]
test<-test[,-1]
#separate the label colimns
label<-train[,"popularity"]
#make it binary 1=1, rest=0
label1<-rep(0,length(label))
for (i in 1:length(label1)) {
  if (label[i] == 1){
    label1[i] = 1
  } 
}

#remove label from the set 
tain<- train[,-ncol(train)]

#transform into marix
train<-as.matrix(train)

dtrain <- xgb.DMatrix(data = train, label = label1)

#use the package
bst <- xgboost(data = dtrain, max.depth = 5000, eta = 1, nthread = 2, 
               nround = 1000, objective = "binary:logistic", verbose = 0)
#prediction part
#probability prediction
pred <- predict(bst, as.matrix(test))
#transform into labels
prediction <- as.numeric(pred > 0.5)

#another approach 
#https://www.kaggle.com/tqchen/otto-group-product-classification-challenge/understanding-xgboost-model-on-otto-data/notebook
numberOfClasses <- max(label)+1
param <- list("objective" = "multi:softprob", #objective objective function
              "eval_metric" = "mlogloss",    
              "num_class" = numberOfClasses,
              "eta"=0.01,
              "max_depth"=16,
              "sub_sample"=0.9)
#the max number of iterations
cv.nround <- 5
#the original dataset is randomly partitioned into nfold equal size subsamples.
cv.nfold <- 100


bst.cv = xgb.cv(param=param, data = train, label = label, 
                nfold = cv.nfold, nrounds = cv.nround)
nround = 200
bst = xgboost(param=param, data = train, label = label, nrounds=nround)
bst.cv <- xgb.cv(param=param, data=train, label=label, 
                 nfold=cv.nfold, nrounds=cv.nround, prediction=TRUE, verbose=FALSE)
# get CV's prediction decoding
pred.cv = matrix(bst.cv$pred, nrow=length(bst.cv$pred)/numberOfClasses, ncol=numberOfClasses)
pred.cv = max.col(pred.cv, "last")
# confusion matrix
confusionMatrix(factor(label+1), factor(pred.cv))


#prediction class
pred<-predict(bst, as.matrix(test))
# decode prediction
pred = matrix(pred, nrow=numberOfClasses, ncol=length(pred)/numberOfClasses)
pred = t(pred)
pred = max.col(pred, "last")


#ZeroVariance of the features
require(caret)
require(corrplot)
require(Rtsne)
require(xgboost)
require(stats)
require(knitr)
require(ggplot2)
zero.var = nearZeroVar(train, saveMetrics=TRUE)
zero.var

#We can get rid of kw_min_min 
#Idea:Based on the principal component analysis PCA,
#it is important that features have maximum variance for maximum uniqueness, so that 
#each feature is as distant as possible (as orthogonal as possible) from the other features.

#tSNE plot - 2d plot of multi-dim classes
tsne = Rtsne(as.matrix(train), check_duplicates=FALSE, pca=TRUE, 
             perplexity=30, theta=0.5, dims=2)
