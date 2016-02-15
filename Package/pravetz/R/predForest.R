# ----------------------------------------------------------------------
#  predictForest 
# ----------------------------------------------------------------------
#' Performs a random forest model based on input specifications 
#' 
#' This functions reproduces in some ways the result achieved in the
#' in the Kaggle competition so far. The function performing based on the 
#' arguments a random forest model to predict the test data.

#' @param training A data frame containing both the predicators and response
#' @param Nresponse Character defining the name of the response varaible in training
#' @param test A data frame containing the test data
#' @param drops A vector containing column names that should be droped from the training set
#' @param nt An integer specifying how many trees should be used in random 
#' @return A data frame containing predicted labels for each id in the \code{test} data
#' @export
#' @examples
#' # Create a simple training data set similiar to the competition set
#' N  <- 10
#' M  <- 10
#' # Transform to quantile matrix
#' predictForest( training = trSet, test =  )

# ----------------------------------------------------------------------
#  Begin code 
# ----------------------------------------------------------------------

predictForest <- function( training = NULL , Nresponse = "popularity", test = NULL, 
						     drops = c("id","url"), nt = 1000 )

{

################################################################################
# Check input for correct format
################################################################################
# Check for correct format of training
if( is.data.frame(training) == FALSE ){
stop("Error: training must be an object of class data frame")
} 
# Check for correct format of test
if( is.data.frame(test) == FALSE ){ 
stop("Error: The argument test must be an object of class data frame")
}
# Check if data are empty - test
if( ncol(training) == 0 | nrow(training) == 0 ){
stop("Error: The argument training can't be empty")
} 
# Check if data are empty - training
if( ncol(test) == 0 | nrow(test) == 0 ){
stop("Error: The argument test can't be empty")
} 
# Convert response variable if necessary and check if empty
response <- training[,Nresponse]
n 		 <- length(response)
if( n == 0  ){ 
stop("Error: No response specified")
} else if ( is.factor( response ) == FALSE ) { 
response <- as.factor( response )
}
################################################################################
# Run prediction
################################################################################

# Adjust data frame for the model
feature <- training[,!(names(training) %in% c(drops,Nresponse))]
# Compute the model 
m01  <- randomForest( x = feature , y = response , ntree = nt )

# Adjust the data frame for prediction
testPredict <- test[,!(names(test) %in% c(drops))] 
# Compute predictions
pred <- predict( m01 , newdata = testPredict , type ='class')
# Convert to prediction to numeric variable
pred <- as.numeric(pred)

################################################################################
# Design final output
################################################################################

# Look up test id
idTest <- test[,"id"]
# Check if id is existing
if( length(idTest) == 0){
# Compute placeholder id in case there is no adequate id in test set
idTest <- 1:nrow(test)
# Print corresponding warning
warning("No proper id found in test data set")
}

# Combine output
output <- data.frame( id = idTest  , popularity = pred )
# Return output
return(output)

}

# ----------------------------------------------------------------------
#  End Code
# ----------------------------------------------------------------------