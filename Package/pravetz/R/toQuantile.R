# ----------------------------------------------------------------------
#  toQuantile 
# ----------------------------------------------------------------------
#' Transforms values of matrix or data frame to corresponding quantiles. 
#' 
#' This functions transforms each value in a column to the quantile it
#' it belongs to. Notice that if quantiles are overlapping the function
#' produces an error. To fix that there will be added a tiny random noise
#' (uniform distributed) to each value to avoid this circumstance. 

#' @param data A numeric matrix or data frame. 
#' @param quant A vector between 0 and 1 with quantiles, percentiles, etc. By default set to percentiles
#' @param adj.l A number, which is used as the lower bound of the random noise added to the values in each column. By default is set to 0.1**10
#' @param adj.u A number, which is used as the upper bound of the random noise added to the values in each column. By default is set to 0.1**10 0.2**10.
#' @return A matrix with transformed values of \code{data}.
#' @export
#' @examples
#' # create a matrix
#'mat <- matrix(rnorm(30),ncol=5, nrow= 6)
#' 
#' # Transform to quantile matrix
#' toQuantile(mat)

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