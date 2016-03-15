setwd("/home/didi/BGSE/semester2/adcomp/Kaggle")
data <- read.csv('news_popularity_training.csv',header=TRUE,sep=',')
y <- data[,"popularity"]; 
x <- data[,-ncol(data)]; 
x <- x[,-1]; 
x <- x[,-1];
x <- x[,-1];
#x<-as.data.frame(x)
#in total 58 variables without id, url, timedelta 

#caret package
#source: http://topepo.github.io/caret/rfe.html
library("caret")
library("Hmisc")
library("randomForest")


# #Identify binary variables
# binary<-as.matrix(apply(x,2,function(x) { all(x %in% 0:1) }))
# binary<- subset(binary, binary[,1]==TRUE)
# without<- c("data_channel_is_lifestyle","data_channel_is_entertainment", "data_channel_is_bus",
#             "data_channel_is_socmed", "data_channel_is_tech", "data_channel_is_world", "weekday_is_monday",
#             "weekday_is_tuesday", "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday",
#             "weekday_is_saturday","weekday_is_sunday", "is_weekend")
# 
# normx<-x[-c("data_channel_is_lifestyle","data_channel_is_entertainment", "data_channel_is_bus",
#              "data_channel_is_socmed", "data_channel_is_tech", "data_channel_is_world", "weekday_is_monday",
#              "weekday_is_tuesday", "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday",
#              "weekday_is_saturday","weekday_is_sunday", "is_weekend")]
# 
# new<- subset(x, select=without)
# bla<- subset(x, select=-without)
# 
# normx<-x[, !(colnames(x) %in% without)]
# #predictors are centered and scaled 
 normalization <- preProcess(x, method = "scale") #estimates the necessary parameters to scale and center
 x <- predict(normalization, x) #actually scale and center them 
# #note we can estimate the center and scale estimates on training set and 
# #then apply it to ANY set: http://topepo.github.io/caret/preprocess.html
x <- as.data.frame(x)
subsets <- c(1:5, 10, 15, 20, 25) #numbers of predictors 

#we do resampling based on the second algorithm 
# meaning that we reevaluate the significance of each classifier 
#after each action


ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv", #resampling 
                   repeats = 5,
                   verbose = FALSE) # prevents from copious amounts of output from being produced

lmProfile <- rfe(x, y,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile
