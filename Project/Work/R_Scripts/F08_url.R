###################################################
##### TRANSFORMATION OF THE URL VARIABLE
##################################################


####################
# Import data set
####################
data<-read.csv("/home/didi/Dropbox/DS_GROUP/15D012 - 
               Advanced Computational Methods/
               Data/news_popularity_training.csv")
url<- data[,2] #separate the variable under inspection

####################
# Divide URL in 
# title and Date
##################

#divide the content of the url in parts
content<-strsplit(url, "/")


## DATE


#create year column
year<- rep(NA,length(content))
for(i in 1:(length(content))){
  #take the 4th element of each row
  year[i]<-content[[i]][4] 
}
year<-as.numeric(year) #remoove quotes

#create month column
month<- rep(NA,length(content))
for(i in 1:(length(content))){
  #take the 5th element of each row
  month[i]<-content[[i]][5]
}
month<-as.numeric(month)#remove quotes

#create day column
day<- rep(NA,length(content))
for(i in 1:(length(content))){
  #take the 6th element of each row
  day[i]<-content[[i]][6]
}
day<-as.numeric(day)#remove quotes



### CREATE NEW DATA, appending 
### the new columns
new_data<- cbind(data,year,month,day)

### Create variable which represents the whole date
new_data$date <- paste0(new_data[,"year"],"/",
                        new_data[,"month"],"/",
                        new_data[,"day"])
#convert to date format
new_data$date <- as.Date(new_data$date, "%Y/%m/%d")


## TITLE

title<-rep(NA,length(content))
for(i in 1:(length(content))){
  # take the 7th element of each row
  title[i]<-content[[i]][7]
}

