################################################################################
####################       FXX. Auxilliary Functions        ####################
################################################################################

# Author:       Denitsa Panova, Felix Gutmann, Thomas Vincente
# Programm:     Barcelona graduate school of economics - M.S. Data Science 
# Course:       15D012 - Advanced Computational Methods 
# Type:         Classification competition

# Content:		This file stores some functions that might be used in context of
#				this project

################################################################################
# Very basic function to partition data in two parts
################################################################################

dataSplit <- function(data,size){
# Define dimensions of data split
N 	<- nrow(data) 
S   <- 1:N
M 	<- ceiling( size * N )
D 	<- N - M 
# Sample the rows according to define dimensions
dimTR <- sample(N,M,replace=FALSE)
dimTE <- setdiff(S,dimTR)
# Define Training set and test set
trainingSet <- data[dimTR,]
testSet 	<- data[dimTE,]
# Define output list and return it
output <- list( TrainingSet=trainingSet, TestSet=testSet )
# Return output
return(output)
}

################################################################################
# Sample data according to share specifiation for each class
################################################################################

cutSet <- function( data, size = c( 9478, 13764, 5712 , 999 , 47) ){
# Initialize empty data frame - Keep column format
balanceDF <- data[FALSE,]
# Sample rows for each class according to input specifiation
for( i in 1:5 ){
	# Subset data for class i
	setTemp   <- data[ as.numeric( data$popularity ) == i , ]
	# Compute number of rows
	N 		  <- nrow(setTemp)
	# Sample a permutation of the rows 
	permut 	  <- sample(N , size[i], replace = FALSE )
	# Subset them and append to output data frame
	append 	  <- setTemp[permut,]
	balanceDF <- rbind(balanceDF,append)

}
# Shuffle the data
N 		  <- sum(size)
balanceDF <- as.data.frame(balanceDF[sample.int(N), ]) 
# Return data frame
return(balanceDF)
}

################################################################################
# Convert a matrix of Vector into factor format
################################################################################

toFactor <- function(data,lab = colnames(data) ){
# Check class of input data - OP1: Vector
if( is.vector(data) == TRUE ){
	# Convert vector to factor variable
	newVec <- factor(data, labels = lab)
	# Return new vector
	return(newVec)
# Check class of input data - OP2: DF / Matrix
} else {
	# Convert input data to factor column
	newMat <- factor(apply(data, 1, function(x) which(x == 1)), labels = lab )
	# Return new factor variable
	return(newMat)
}}

################################################################################
# Scale matrix to 0 and 1
################################################################################

toOne <- function(data){
# Compute column mins and max
max    <- apply(data,2,max)
min    <- apply(data,2,min)
# Scale the matrix
output <- scale(data, center = min, scale = max - min )
# Convert to data.frame
output <- as.data.frame(output)
# Rename out data frame
colnames(output)<- names(data)
# Return output
return(output)
}

################################################################################
# Convert input data to binary data with reference to the median
################################################################################

toMedian <- function(data){
# Compute dimensions of input data
N02   <- ncol(data)
N03   <- nrow(data)
# Compute the medians by feature
med   <- sapply(1:N02,function(j) median(data[,j]) )
# Intialize transformation matrix and rename it
Qtransform 	 <- matrix(NA, nrow = N03 , ncol =N02 )
colnames(Qtransform)<- names(data)
# Transform each column to median matrix
for(i in 1:N02){
	TC01 <- data[,i]
	Qtransform[,i]  <-ifelse( TC01 <= med[i] , 0 , 1 )
}
# Return output
return(Qtransform)
}

################################################################################
# Convert input data to specified quantile value
################################################################################

toQuantile <- function(data, 
					   quant = seq( 0, 1, 0.01 ), 
					   adj.l = 0.1**10,  
					   adj.u = 0.2**10 )
{
# Get Dimension of input data
N    <- nrow(data)
M    <- ncol(data)
# Add some tiny random noise to the data to avoid quantile overlapping
data <- data + runif(N , adj.l, )
# Set quantiles
q    <- quant
# Compute the quantiles for each feature
quant.matrix <- apply( data , 2 , function(j){ quantile( j , q) } )
# Intialize and rename output matrix
Qtransform   <- matrix( NA , nrow = N , ncol = M )
colnames(Qtransform) <- names( data )
# Transform each column to quantile values
for( i in 1:M ){
   TC01 <- data[,i]
   TC02 <- quant.matrix[,i]
   Qtransform[,i]  <- as.numeric(cut(TC01, TC02, include.lowest = TRUE)
   )
}
# Return final output
return(Qtransform)
}

################################################################################
# Design submission
################################################################################

toSubmission <- function( model , test , id = NA , name = "prediction" , save = FALSE ){
# Compute predictions
prediction <- as.numeric( predict( model , newx = test , decision.values = TRUE) )
# Design output
output     <- as.data.frame( cbind( id = id , popularity = prediction ) )
# Save to harddrive if specified
if( save == TRUE ){ write.table(output, 
                                file = paste0(name,Sys.Date(),".csv") ,
                           row.names = FALSE , 
                           col.names = TRUE, 
                                 sep = "," )}

# Return output
return(output)
}

################################################################################
# Compute a confusion matrix, comparing pedictions vs. truth
################################################################################

confusionMatrix <- function(truth,prediction){
	# Create confusion matrix based on input 
	confusion.matrix <- prop.table(table(truth, prediction),1)
	# Return matrix
	return(confusion.matrix)
}

################################################################################
# Run crossvalidation on input data only with random forest - 
# Use acc function for other methods and more arguments
################################################################################

validateForest <- function( data , response , k = 5 , nt = 500 ){
# Compute number of rows
N  	  	<- nrow(data)
# Shuffle row indices
shuffle <- sample.int(N)
# Split the rows in k-different parts	
split   <- split( shuffle , ceiling( 1:N /( N / k) ) )
# Initialize accuracy vecotr
acc 	<- rep(NA,k)
# Initialize confusion matrix
class 	<- length( unique( as.numeric(response) ) )
con     <- matrix(0, nrow = class, ncol = class )
# Run cross validation
for(i in 1:k){
	# Compute the training data
    X  <- data[setdiff(unlist(split),split[[i]]),]
 	Y  <- response[setdiff(unlist(split),split[[i]])]
 	D  <- data.frame( X , Y )
	# Compute the test sample
    x  <- data[split[[i]],]
    y  <- response[split[[i]]]
    # Compute model
    mod <- randomForest( Y ~. , data = D ,ntree = nt )
    # Cpmpute prediction 
  	pre <- predict(mod, newdata = x , type ='class' )
	# Compute accuracy of the model
	acc[i] <- length( which( pre == y ) ) / nrow(x) 
	# Compute confusion matrix
    con    <- con + confusionMatrix( y, pre )
    }
# Design final output 1: Compute mean confusion
con <- round ( con / k , 3 )
# Design final output 2: Compute mean accuracy
acc <- mean(acc)
# Combine results
output <- list( confusionMatrix = con , Accuracy = acc )
# Return output
return(output)
}

################################################################################
# More general function to validate different kind of methods
################################################################################

acc <- function( method , model , k = 5 , save = FALSE, file = NULL ) {
# Get number of observations
n 	  <- nrow(model)
# Shuffle input data
model <- as.data.frame(model[sample.int(n), ]) 
# Initialize accuracy vector 
acc   <- c()
# Initialize list 
conf  <- list()
# Compute accuracy and configuration using cross validation
sapply(1:k, function(i) {
	# Partition data set
	s  <- n * (i-1) / k+1  
	e  <- n *  i/k
	# Big sample
	CV <- model[-(s:e),] 
	# Small sample
	cv <- model[(s:e),]
	# Predict model output
	pred <- predict(method(CV), newdata = cv[,!(names(cv) %in% "popularity") ], type ='class')
	# Compute accuracy of the model and
	acc  <<- c(acc, length(which(pred==cv$popularity))/nrow(cv))
	conf <<- list(conf, confusionMatrix(pred,cv[,1]))
	})
# Optionally save result to hard drive
if ( save == TRUE ){ 
write.table(t(c(as.character(as.list(match.call())$method), 
	 			as.character(as.list(match.call())$model ), 
	 			mean(acc))), 
				file, 
				append = TRUE, 
				col.names = FALSE, 
				row.names = FALSE
			)
}
# Design final output
output <- list( ConfusionMatrix = conf, Accuracy = mean(acc) )
# Return output
return(output)
}

################################################################################
# Some methods that can be used within acc
################################################################################

useForest <- function(data) {randomForest(popularity ~. , data=data, ntree=1000 )}
boost 	  <- function(data) {boosting(popularity~., data=data)}
pmlr 	  <- function(data) {pmlr(popularity~., data=data)}
bag 	  <- function(data) {bagging(popularity~., data=data)}
forest 	  <- function(data) {randomForest(popularity~., data=data)}
mnet 	  <- function(data) {multinom(popularity~., data=data, trace=F)}
forestTr100   	  <- function(data) {randomForest(popularity~., data=data, ntree=100)}
forestMtry50 	  <- function(data) {randomForest(popularity~., data=data, mtry=50)}
forestTr100Mtry50 <- function(data) {randomForest(popularity~., data=data, ntree=100, mtry=50)}
forestMtry5 	  <- function(data) {randomForest(popularity~., data=data, mtry=50)}
forestBal 		  <- function(data) {randomForest(popularity~., data=data, replace=T, sampsize=c(10, 10, 10, 10, 10))}
wsrf 			  <- function(data) {wsrf(popularity~., data=data)}
forest2000 		  <- function(data) {randomForest(popularity~., data=data, ntree=2000, mtry=30)}
foT1000repFnod20  <- function(data) {randomForest(popularity~., data=data, ntree=1000, replace=F, nodes=20)}
foOptMtryNt1500   <- function(data) { mtry <- tuneRF(data[,-1], data[,1], ntreeTry=1501, trace=F)
	randomForest(popularity~., data=data, mtry=as.numeric(as.matrix(mtry[order(mtry[,2]),])[1,1]), ntree=1501)
}
foOptMtry 		 <- function(data) {
	mtry <- tuneRF(data[,-1], data[,1], trace=F)
	randomForest(popularity~., data=data, mtry=as.numeric(as.matrix(mtry[order(mtry[,2]),])[1,1]))
}
foOptMtryStart30step1p5 <- function(data) {
	mtry <- tuneRF(data[,-1], data[,1], trace=F, stepFactor=1.5, mtryStart=30)
	randomForest(popularity~., data=data, mtry=as.numeric(as.matrix(mtry[order(mtry[,2]),])[1,1]))
}

################################################################################
# Some useful ploting automatisation
################################################################################

# The Function gets two columns of a data freame or a matrix and plots the data 
# points with different colours for each class

# Function Arguements:
# data = data.frame or matrix
#    i = column number
#    j = column number

plotFunc <- function( data , i , j ){
  library(ggplot2)
  ggplot( data = data ) + 
  geom_point( data = data, 
         aes( x = data[,i], 
         	  y = data[,j], 
         	  z = data[,"popularity"], 
         colour = as.factor(data[,"popularity"])
         	))
}

# The Function gets a column of a data freame or a matrix and plots its densities 
# with different colours for each class

# Function Arguements:
# data = data.frame or matrix
#    i = column number

plotDens <- function(data,i){
  library(ggplot2)
  qplot( data[,i], 
  		 data = data, 
  		 geom = "density", 
  		 fill = factor(data[,"popularity"]), 
        alpha = I(.5), 
         ylab = "Density")  
}

################################################################################

################################################################################