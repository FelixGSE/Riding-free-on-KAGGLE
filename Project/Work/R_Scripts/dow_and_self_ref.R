###################################################
##### DOW,CHANNEL AND SELF_REFERENCE VARIABLES
##################################################


####################
# Import data set
####################
data<-read.csv("/home/didi/Dropbox/DS_GROUP/
               15D012 - Advanced Computational Methods/
               Data/data_no_outliers")

## Include variables which have been 
## indicated as relevant in the previous
## analysis

ok<-c("id","global_subjectivity","rate_negative_words",
      "n_tokens_title","abs_title_sentiment_polarity", 
      "self_reference_min_shares","self_reference_max_shares",
      "self_reference_avg_sharess","weekday_is_monday",
      "weekday_is_tuesday", "weekday_is_wednesday","weekday_is_thursday",
      "weekday_is_friday","weekday_is_saturday",
      "n_unique_tokens", "n_non_stop_words", "n_non_stop_unique_tokens", 
      "num_self_hrefs","num_imgs", "num_videos", 
      "average_token_length","num_keywords","is_weekend","weekday_is_sunday",                 
      "data_channel_is_lifestyle","data_channel_is_entertainment",
      "data_channel_is_bus", "data_channel_is_socmed",       
      "data_channel_is_tech","data_channel_is_world",
      "kw_max_max","kw_min_min","popularity" )

#Construct the dataset with 
# only the relevant variables
relevant<-data[,(colnames(data) %in% ok) ]

#NOTE: The variables has decresed from 60 to 31 

# Collect all DOW and CHANNEL in one variable
dow<-relevant[,c("weekday_is_monday","weekday_is_tuesday", 
                 "is_weekend","weekday_is_sunday",
                 "weekday_is_wednesday","weekday_is_thursday",
                 "weekday_is_friday","weekday_is_saturday")]
channel<-relevant[,c("data_channel_is_lifestyle",
                     "data_channel_is_entertainment",
                     "data_channel_is_bus", 
                     "data_channel_is_socmed",       
                     "data_channel_is_tech",
                     "data_channel_is_world")]


##Correlation analysis between DOW and 
## the DATA
d<-cor(dow,relevant)
library(corrplot)
corrplot(d,method = "number", type="upper")
#OUTCOME - no correlation


##Correlation analysis between CHANNEL and 
## the DATA
c<-cor(channel,relevant)
corrplot(c,method = "number", type="upper")
#OUTCOME - Worth investigating for 
# the purpose of interaction dummies


## INTERAVTION DUMMIES of CHANNEL
c1<-cor(channel[length(channel)],relevant)
corrplot(c1,method = "number", type="upper")
#data_channel_world interaction with 
#ref/imag/videos/global/negative 

c2<-cor(channel[2],relevant)
corrplot(c2,method = "number", type="upper")
#data_channel_entertainment interact with
# imag/video/n_token_title

c3<-cor(channel[5],relevant)
corrplot(c3,method = "number", type="upper")
# data_channel_tech intect with
#num_ref



## Collect all SELF in one variable
self<-relevant[,c("self_reference_min_shares",
                  "self_reference_max_shares",
                  "self_reference_avg_sharess")]
## Correlation between SELFs 
s<-cor(self)
corrplot(s,method="number",type="upper")
#OUTCOME - all of them are correlated

