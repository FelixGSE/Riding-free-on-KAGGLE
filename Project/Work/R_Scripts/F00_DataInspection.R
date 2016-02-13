################################################################################
####################          F00. Data inspection          ####################
################################################################################

# Author:       Felix Gutmann
# Programm:     Barcelona graduate school of economics - M.S. Data Science 
# Course:       15D012 - Advanced Computational Methods (Term 2)
# Type:         Classification competition
# Last update:  

# Description:	

################################################################################
### Preamble
################################################################################

### Clear workspace
rm(list = ls())

### Load Packages 

### Set working directory
setwd("/home/felix/Dropbox/GSE/DS_GROUP_BOX/15D012 - Advanced Computational Methods/Data")

### Load and re-order Data
	
	# Training data (30000 x 62)
	training <- read.csv("news_popularity_training.csv")
	# Test data (9644 x 61)
	test 	 <- read.csv("news_popularity_test.csv")  
	# Example of a data submission set (9644 x 2)
	sample 	 <- read.csv("news_popularity_sample.csv")   

################################################################################

################################################################################

# True categories
pop <- setdiff(names(training),names(test))

# Number of observations
N 	<- nrow(training)

# Number of classes
cla 	<- length(unique(training$popularity))

# Number of missing values in the class column
claNA 	<- sum(is.na(training$popularity))

# Check for oversampling / Create storage data frame
sampling.info <- as.data.frame(cbind(class=1:cla,count=NA,percent=NA))

# Compute sampling values
for(i in 1:cla){
	# Compute the number of class in the training set
	temp <- length(which(training$popularity==i))
	# Save absolute number of class i in data frame
	sampling.info[i,2] <- temp
	# Save the percentage share of each class in data frame
	sampling.info[i,3] <- round((temp/N)*100,2)
}

################################################################################

################################################################################

# Count missing values in training data set 
missings01 <- rep(NA,ncol(training))

for( i in 1:ncol(training) ){
	# Count missings by variable
	missings01[i] <- sum(is.na(training[,i]))
}

# Total number of missings in training data set
totNA01 <- sum(missings01)

# Count missing values in training data set 
missings02 <- rep(NA,ncol(test))

for( i in 1:ncol(test) ){
	# Count missings by variable
	missings02[i] <- sum(is.na(training[,i]))
}

# Total number of missings in test data set
totNA02 <- sum(missings02)

# Summary 

################################################################################

################################################################################

sum.tab <- matrix(NA, nrow = ( ncol(training) - 3 ), ncol = 6 )

for(i in 1:( ncol( training )- 3 ) ) {

temp 	 	 <- training[,(i+3)]
sum.tab[i,]  <- t( as.matrix ( summary( temp ) ) )

}

rownames(sum.tab) <- names(training)[ 4 : ncol(training) ]
colnames(sum.tab) <- c( "Min.", "1st Qu.", "Median", "Mean" , "3rd Qu." , "Max." )   

#write.csv(sum.tab, file = "Variable_Summary02.csv", row.names = TRUE, 
#			  col.names = TRUE, sep = ",")

################################################################################

################################################################################

tr <- training[-rows,]

sum.tab <- matrix(NA, nrow = ( ncol(tr) - 3 ), ncol = 6 )

for(i in 1:( ncol( tr )- 3 ) ) {

temp 	 	 <- tr[,(i+3)]
sum.tab[i,]  <- t( as.matrix ( summary( temp ) ) )

}

rownames(sum.tab) <- names(tr)[ 4 : ncol(tr) ]
colnames(sum.tab) <- c( "Min.", "1st Qu.", "Median", "Mean" , "3rd Qu." , "Max." )   

write.csv(sum.tab, file = "Variable_Summary03.csv", row.names = TRUE, 
			  col.names = TRUE, sep = ",")


################################################################################

################################################################################
