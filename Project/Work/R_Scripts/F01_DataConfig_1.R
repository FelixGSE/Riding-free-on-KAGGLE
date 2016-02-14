################################################################################
####################        F01. Data Configuration 1       ####################
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

### Load Packages 
if (!require("DMwR")) install.packages("DMwR"); library(DMwR)

### Set working directory for other R-Scripts
setwd("/home/felix/Dropbox/GSE/DS_GROUP_BOX/15D012 - Advanced Computational Methods/Work/R-Scripts/Felix")

### Load functions
source("FXX_AuxilliaryFunctions.R")
source("F02_Variable_Details.R")

### Set working directory for input and ourput data
setwd("/home/felix/Dropbox/GSE/DS_GROUP_BOX/15D012 - Advanced Computational Methods/Data")

### Load and re-order Data
	
	# Training data (30000 x 62)
	training <- read.csv("news_popularity_training.csv")
	# Test data (9644 x 61)
	test 	 <- read.csv("news_popularity_test.csv")  
	# Example of a data submission set (9644 x 2)
	sample 	 <- read.csv("news_popularity_sample.csv")   

### Save configurations to harddrive ?
saveData = FALSE

################################################################################
# Basic
################################################################################

# Subset ids
idTR <- training$id
idTE <- test$id

# Dimensions of training
N00 <- nrow(training)
M00 <- ncol(training)
# Dimensions of test

N01 <- nrow(test)
M01 <- ncol(test)

# Convert dummies to factors (Colnames in Variable script) TRAINING
wd    <- training[,weekday]
we 	  <- training[,weekend]
ch 	  <- training[,channel]  
other <- ifelse( rowSums(ch) == 0, 1 , 0 ) 
ch 	  <- cbind( ch , data_channel_is_other = other   )

FA01  <- toFactor(wd, lab = c("MO","TU","WE","TH","FR","SA","SU"))
FA02  <- toFactor(we, lab = c("WD","WE"))
FA03  <- toFactor(ch, lab = c("LS","ET","BU","SO","TE","WO","OT"))

dum   <- data.frame( weekday = FA01 , weekend = FA02 , channel = FA03 )

# Convert outcome variable to factor
Y00   <- factor(training$popularity , labels = c("A", "B","C","D","E"))

# Convert dummies to factors (Colnames in Variable script) TEST
wdTE    <- test[,weekday]
weTE 	<- test[,weekend]
chTE 	<- test[,channel]  
otherTE <- ifelse( rowSums(chTE) == 0, 1 , 0 ) 
chTE 	<- cbind( chTE , data_channel_is_other = otherTE   )

FA04  <- toFactor(wdTE, lab = c("MO","TU","WE","TH","FR","SA","SU"))
FA05  <- toFactor(weTE, lab = c("WD","WE"))
FA06  <- toFactor(chTE, lab = c("LS","ET","BU","SO","TE","WO","OT"))

dumTE <- data.frame( weekday = FA04 , weekend = FA05 , channel = FA06 )

################################################################################
# Basic config - No adjustments
################################################################################

c0  <- c( np , weekday , weekend , channel , outcome ) 
TR  <- cbind( training[, !(names(training) %in% c0) ], dum , popularity = Y00 )
TE  <- cbind( test[, !(names(test) %in% c0) ] , dumTE )

################################################################################
# Config 0
################################################################################

# Standardized / dummies as factors 

# Training
c00  <- c( np , weekday , weekend , channel , outcome ) 
temp <- scale(training[ , !(names(training) %in% c00) ],center = TRUE , scale = TRUE)
tr00 <- cbind( temp , dum , popularity = Y00)

# Test
temp <- scale(test[ , !(names(test) %in% c00) ],center = TRUE , scale = TRUE)
te00 <- cbind( temp , dumTE )

################################################################################
# Config 1
################################################################################

# Standardized / dummies as factors / lda normal

# Training
c01  <- c( np , weekday , weekend , channel , lda , outcome ) 
temp <- scale(training[ , !(names(training) %in% c01) ],center = TRUE , scale = TRUE)
tr01 <- cbind( temp , dum , training[,lda], popularity = Y00)

# Test
temp <- scale(test[ , !(names(test) %in% c01) ],center = TRUE , scale = TRUE)
te01 <- cbind( temp , dumTE , test[,lda])

################################################################################
# Config 2 - Median transformation
################################################################################

# Training
c02  <- c( np , weekday , weekend , channel , lda , outcome ) 
temp <- toMedian( training[ ,!(names(training) %in% c02) ] )
tr02 <- cbind( temp , dum , training[,lda], popularity = Y00  )

# Test 
temp <- toMedian(test[ , !(names(test) %in% c02) ])
te02 <- cbind( temp , dumTE , test[,lda] )

################################################################################
# Config 3 - Percentile transformation
################################################################################

c03  <- c( np , weekday , weekend , channel , lda , outcome )
temp <- toQuantile( training[ ,!(names(training) %in% c03 ) ] )
tr03 <- cbind( temp , dum , training[,lda], popularity = Y00 )

temp <- toQuantile(test[ , !(names(test) %in% c03) ])
te03 <- cbind(temp , dumTE , test[,lda] )

################################################################################
# Config 4 - Decile transformation
################################################################################

c04  <- c( np , weekday , weekend , channel , lda , outcome )
temp <- toQuantile( training[ ,!(names(training) %in% c03 ) ], quant = seq( 0, 1, 0.1 )  )
tr04 <- cbind( temp , dum , training[,lda], popularity = Y00 )

temp <- toQuantile(test[ , !(names(test) %in% c03) ])
te04 <- cbind(temp , dumTE , test[,lda] )

################################################################################
# Config 5 - Drop that shit
################################################################################

# Clean rows
cleanTR <- training[-rows,]

# Create interactions
int01 <- cleanTR$data_channel_is_world * cleanTR$num_imgs
int02 <- cleanTR$data_channel_is_world * cleanTR$num_videos
int03 <- cleanTR$data_channel_is_world * cleanTR$global_subjectivity
int04 <- cleanTR$data_channel_is_world * cleanTR$rate_negative_words
int05 <- cleanTR$data_channel_is_entertainment * cleanTR$num_imgs
int06 <- cleanTR$data_channel_is_entertainment * cleanTR$num_videos
int07 <- cleanTR$data_channel_is_entertainment * cleanTR$n_tokens_title
int08 <- cleanTR$data_channel_is_tech * cleanTR$num_self_hrefs

int_df <- data.frame( it_WO_NIM = int01 , it_WO_NVI = int02,  it_WO_GS  = int03, 
					  it_WO_NW  = int04,  it_ET_NIM = int05 , it_ET_NVI = int06, 
					  it_WO_NTT = int07, it_WO_NSR = int08 )

# Combine sets 

tr05 <- cbind( toOne(cleanTR[,scaleD]) ,  cleanTR[,ok] , int_df, 
				weekend = FA02[-rows], channel = FA03[-rows], popularity = Y00[-rows]) 

# Test Set

# Create interactions
int11 <- test$data_channel_is_world * test$num_imgs
int12 <- test$data_channel_is_world * test$num_videos
int13 <- test$data_channel_is_world * test$global_subjectivity
int14 <- test$data_channel_is_world * test$rate_negative_words
int15 <- test$data_channel_is_entertainment * test$num_imgs
int16 <- test$data_channel_is_entertainment * test$num_videos
int17 <- test$data_channel_is_entertainment * test$n_tokens_title
int18 <- test$data_channel_is_tech * test$num_self_hrefs

int_df2 <- data.frame( it_WO_NIM = int11 , it_WO_NVI = int12,  it_WO_GS  = int13, 
					   it_WO_NW  = int14,  it_ET_NIM = int15 , it_ET_NVI = int16, 
					   it_WO_NTT = int17,  it_WO_NSR = int18 )

te05 <- cbind( toOne(test[,scaleD]) ,  test[,ok] , int_df2, 
			   weekend = FA05, channel = FA06 ) 

################################################################################
# Config 6 - Drop that shit
################################################################################

tr06 <- cutSet(TR, size = c( 8000, 10000, 5712, 999, 47 ) )

################################################################################
# Config 7 
################################################################################

tr08 <- cbind( timedelta = training$timedelta , tr03 )

################################################################################
# Config 8
################################################################################

temp  <- training
temp  <- temp[,!(names(temp) %in% c("id","url")  )]
sub09 <- dataSplit(temp,0.9)
subTR <- sub09$TrainingSet

te 			  <- subTR[!subTR$popularity ==4 & !subTR$popularity == 5 ,]
te$popularity <- as.factor(as.character(te$popularity))

samp 	  <- autoSmote(te, p = 100 , k = 10, 3 )

subTR <- shuffleDF(rbind(subTR,samp))
subTR$popularity <- as.factor(subTR$popularity )
subTE <- sub09$TestSet
subTE$popularity <- as.factor(subTE$popularity )

################################################################################
# Config 9 - I am desperate
################################################################################

set10 <- cutSet( training, size = floor( 0.1 * c( 9478, 13764, 5712 , 999 , 47) ) )

diff  <- complementDF(training,set10)
subTR <- diff

te 			  <- subTR[!subTR$popularity ==4 & !subTR$popularity == 5 ,]
te$popularity <- as.factor(as.character(te$popularity))

samp 	  <- autoSmote(te, p = 100 , k = 10, 3 )

subTR <- shuffleDF(rbind(subTR,samp))

subTR$popularity <- as.factor(as.numeric(subTR$popularity))

subTE <- set10
subTE$popularity <- as.factor(subTE$popularity )

################################################################################
# Config 10: Extended data
################################################################################

temp <- training[-rows,!(names(training) %in% c("id","url") ) ]
# temp$popularity <- as.factor(as.character(temp$popularity))
te 	 <- temp[!temp$popularity ==4 & !temp$popularity == 5 ,]
te$popularity <- as.factor(as.character(te$popularity))

samp 	  <- autoSmote(te, p = 50 , k = 10, 3 )

temp2 <- shuffleDF(rbind(temp,samp))
temp2[temp2$popularity == 5,] <- 4
temp2$popularity <- as.factor(as.character(temp2$popularity))

################################################################################
# Option: Save data
################################################################################

if( saveData == TRUE ){ 

### Set working directory for input and ourput data
setwd("/home/felix/Dropbox/GSE/DS_GROUP_BOX/15D012 - Advanced Computational Methods/Data/Configurations")

# Config 0
saveRDS(tr00, file = "C0_Training_st_daf.Rda")
saveRDS(te01, file = "C0_Test_st_daf.Rda")

# Config 1
saveRDS(tr01, file = "C1_Training_st_daf_ldaN.Rda")
saveRDS(te02, file = "C1_Test_st_daf_ldaN.Rda")

# Config 2
saveRDS(tr02, file = "C2_Training_MD_daf_ldaN.Rda")
saveRDS(te02, file = "C2_Test_MD_daf_ldaN.Rda")

# Config 3
saveRDS(tr03, file = "C3_Training_PE_daf_ldaN.Rda")
saveRDS(te03, file = "C3_Test_PE_daf_ldaN.Rda")

# Config 4
saveRDS(tr04, file = "C4_Training_DE_daf_ldaN.Rda")
saveRDS(te04, file = "C4_Test_DE_daf_ldaN.Rda")

# Config 5
saveRDS(tr05, file = "C5_Training_clean.Rda")
saveRDS(te05, file = "C5_Test_clean.Rda")

# Report status
print("Data saved to hard drive and loaded to environment")

} else { 

# Report status
print("Data sets loaded to environment") 

}

################################################################################

################################################################################