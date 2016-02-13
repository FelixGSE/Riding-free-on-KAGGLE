################################################################
### OUTLIER DETECTION, USING PLOTS: CLASS5 
###############################################################


##################
# Read the data
##################

# 1. We read the already cleaned data

data<-read.csv("cleaned_class4_data.csv")

# 2. subset for popularity class 5
class5<- subset(data, data[,ncol(data)] == 5)

##################
# MEDIAN COEFF
##################

# We use coefficient to the median to look 
# for outliers
# advantages: smaller scale and if it is INF 
# than either the median=0 or there is extreme 
# value 


## FIND MEDIAN
coeftomed5<-matrix(NA,nrow(class5),(ncol(class5)-1))

med5<- sapply(2:(ncol(class5)-1), function(x) median(class5[,x]))

for( i in 2:(ncol(class5)-1)){
  for(j in 1:nrow(class5)){
    coeftomed5[j,i] <- abs(class5[j,i]/med5[i])
  }
}
#use the same names as data
colnames(coeftomed5)<-names(data)[-46]

##################
# PLOT 4x4 
##################

## Try eyebolling

#we separate the pictures into 4  

old.par <- par(mfrow=c(2, 2))
plot(coeftomed5[,2]) 
plot(coeftomed5[,3]) 
plot(coeftomed5[,4])
plot(coeftomed5[,5])
par(old.par) #reset 

old.par <- par(mfrow=c(2, 2))
plot(coeftomed5[,6])
plot(coeftomed5[,7]) 
plot(coeftomed5[,8]) 
plot(coeftomed5[,9]) 
par(old.par) #reset 

old.par <- par(mfrow=c(2, 2))
plot(coeftomed5[,10]) 
plot(coeftomed5[,11])  
plot(coeftomed5[,12]) 
plot(coeftomed5[,13]) 
par(old.par) #reset 


old.par <- par(mfrow=c(2, 2))
plot(coeftomed5[,14]) 
plot(coeftomed5[,15])  
plot(coeftomed5[,16]) 
plot(coeftomed5[,17]) 
par(old.par) #reset



old.par <- par(mfrow=c(2, 2))
plot(coeftomed5[,18])  
plot(coeftomed5[,19]) 
plot(coeftomed5[,20]) 
plot(coeftomed5[,21]) 
par(old.par) #reset



old.par <- par(mfrow=c(2, 2))
plot(coeftomed5[,22])  
plot(coeftomed5[,23])  
plot(coeftomed5[,24])  
plot(coeftomed5[,25]) 
par(old.par) #reset


old.par <- par(mfrow=c(2, 2))
plot(coeftomed5[,26]) 
plot(coeftomed5[,27]) 
plot(coeftomed5[,28]) 
plot(coeftomed5[,29]) 
par(old.par) #reset


old.par <- par(mfrow=c(2, 2))
plot(coeftomed5[,30]) 
plot(coeftomed5[,31])  
plot(coeftomed5[,32]) 
plot(coeftomed5[,33]) 
par(old.par) #reset


old.par <- par(mfrow=c(2, 2))
plot(coeftomed5[,34])  
plot(coeftomed5[,35])  
plot(coeftomed5[,36]) 
plot(coeftomed5[,37]) 
par(old.par) #reset

old.par <- par(mfrow=c(2, 2))
plot(coeftomed5[,38])  
plot(coeftomed5[,39])  
plot(coeftomed5[,40]) 
plot(coeftomed5[,41]) 
par(old.par) #reset

old.par <- par(mfrow=c(2, 2))
plot(coeftomed5[,41])  
plot(coeftomed5[,42]) 
plot(coeftomed5[,43]) 
plot(coeftomed5[,44]) 
par(old.par) #reset


##################
# OUTLIERS 
##################

# Identify outliers' id's

o1<-order(data[,3],decreasing=T)[1:3]
o2<-order(data[,7],decreasing=T)[1:2]
o3<-order(data[,8],decreasing=T)[1]
o4<-order(data[,9],decreasing=T)[1]
o5<-order(data[,10],decreasing=T)[1:3]
o6<-order(data[,13],decreasing=T)[1:3]
o7<-order(data[,14],decreasing=T)[1:2]
o8<-order(data[,15],decreasing=T)[1:2]
o9<-order(data[,16],decreasing=T)[1:2]
o10<-order(data[,17],decreasing=F)[1:2]
o11<-order(data[,20],decreasing=T)[1:2]
o12<-order(data[,21],decreasing=T)[1:2]
o13<-order(data[,22],decreasing=T)[1]
o14<-order(data[,23],decreasing=T)[1:2]
o15<-order(data[,24],decreasing=T)[1]
o16<-order(data[,26],decreasing=T)[1:2]
o17<-order(data[,27],decreasing=T)[1]
o18<-order(data[,39],decreasing=T)[1]
o19<-order(data[,41],decreasing=T)[1]

## Outliers' id's
withoutclasss5<-c(o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,
                  o11,o12,o13,o14,o15,o16,o17,o18,o19)


##################
# CLEAN DATA
##################

#The data without class5 outliers
data<-data[!(data[,1] %in% withoutclasss5), ]
write.csv(data,"cleaned_class5_data.csv", row.names = FALSE)

#id's of all outliers
outliers_id<-unique(c(withoutclasss1, withoutclasss2, 
                      withoutclasss3, withoutclasss4, withoutclasss5))
#write csv with the id of all outliers 
write.csv(outliers_id, "outliers_id", row.names = FALSE)

#Write csv with the data without all outliers
data<-data[!(data[,1] %in% outliers_id), ]
write.csv(data,"data_no_outliers.csv", row.names = FALSE)
