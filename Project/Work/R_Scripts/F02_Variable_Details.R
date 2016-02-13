################################################################################
####################          F02. Variable_Details         ####################
################################################################################

# Author:       Denitsa Panova, Felix Gutmann, Thomas Vincente
# Programm:     Barcelona graduate school of economics - M.S. Data Science 
# Course:       15D012 - Advanced Computational Methods (Term 2)
# Type:         Classification competition

# Description:	

################################################################################
### Preamble
################################################################################

### Load data
rows <- scan("/home/felix/Dropbox/GSE/DS_GROUP_BOX/15D012 - Advanced Computational Methods/Data/outliers_id" , quiet = TRUE )

################################################################################
# Variables by category
################################################################################

np 		<- c("id", "url", "timedelta")

np2 	<- c("id", "url")

outcome <- c("popularity")

tokens  <- c("n_tokens_title", 
			 "n_tokens_content", 
			 "n_unique_tokens",
			 "n_non_stop_words",
			 "n_non_stop_unique_tokens")       

notsure <- c("average_token_length")

num 	<- c("num_hrefs",
			 "num_self_hrefs", 
			 "num_imgs",
			 "num_videos") 

kw 		<- c("kw_min_min",
			 "kw_max_min", 
			 "kw_avg_min",
			 "kw_min_max",
			 "kw_max_max",
			 "kw_avg_max", 
			 "kw_min_avg",
			 "kw_max_avg",
			 "kw_avg_avg")

self 	<- c("self_reference_min_shares",
			 "self_reference_max_shares",
			 "self_reference_avg_sharess")   

weekday <- c("weekday_is_monday", 
			 "weekday_is_tuesday",
		     "weekday_is_wednesday",
		     "weekday_is_thursday", 
		     "weekday_is_friday",
		     "weekday_is_saturday", 
		     "weekday_is_sunday")

weekend <- c("is_weekend")

lda 	<- c("LDA_00",
			 "LDA_01",
			 "LDA_02",
			 "LDA_03",
			 "LDA_04")  

global 	<- c("global_subjectivity", 
	         "global_sentiment_polarity",
			 "global_rate_positive_words", 
			 "global_rate_negative_words")

rate 	<-  c("rate_positive_words",
		      "rate_negative_words")

channel <-  c("data_channel_is_lifestyle", 
			  "data_channel_is_entertainment",
			  "data_channel_is_bus", 
			  "data_channel_is_socmed" , 
			  "data_channel_is_tech",
			  "data_channel_is_world")

polarity <- c("avg_positive_polarity", 
			  "min_positive_polarity", 
			  "max_positive_polarity", 
			  "avg_negative_polarity",
	 		  "min_negative_polarity", 
	 		  "max_negative_polarity")

title 	 <- c("title_subjectivity",
			  "title_sentiment_polarity",
			  "abs_title_subjectivity", 
			  "abs_title_sentiment_polarity")

################################################################################
# Rows to be droped
################################################################################

ok	 <- c("global_subjectivity",
		  "rate_negative_words")

scaleD <- c("n_tokens_title",
			"self_reference_max_shares",
			"n_unique_tokens",
			"n_unique_tokens", 
      	  	"n_non_stop_words", 
      	  	"n_non_stop_unique_tokens", 
      	  	"num_self_hrefs",
      	    "num_imgs", 
      	    "num_videos",
      	    "average_token_length",
      	  	"num_keywords",
      	  	"kw_max_max",
      	    "kw_min_min")

################################################################################

################################################################################