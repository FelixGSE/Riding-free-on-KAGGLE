################################################################
### OUTLIER DETECTION, USING PLOTS: CLASS1 
###############################################################


##################
# Read the data
##################
data<-read.csv("/home/didi/Dropbox/DS_GROUP/
               15D012 - Advanced Computational Methods/Data/
               news_popularity_training.csv")

# 1.clean data from url and delta time
data<-data[,-c(2,3)]

# 2. Only continuos variables
# (without dow and channel)
data<-data[,-c(seq(31,38), seq(13,18))]

# 3. Subset for popularity class 1
class1<- subset(data, data[,ncol(data)] == 1)


##################
# MEDIAN COEFF
##################

# We use coefficient to the median to look 
# for outliers
# advantages: smaller scale and if it is INF 
# than either the median=0 or there is extreme 
# value 


## FIND MEDIAN
coeftomed1<-matrix(NA,nrow(class1),(ncol(class1)-1))
med1<- sapply(2:(ncol(class1)-1), function(x) median(class1[,x]))

for( i in 2:(ncol(class1)-1)){
  for(j in 1:nrow(class1)){
    coeftomed1[j,i] <- abs(class1[j,i]/med1[i])
  }
}
#use the same names as data
colnames(coeftomed1)<-names(data)[-46]


##################
# PLOT 4x4 
##################
  
## Try eyebolling
old.par <- par(mfrow=c(2, 2))
plot(coeftomed1[,2])
plot(coeftomed1[,3]) #possible outlier 
#take the maximum value out
plot(coeftomed1[,4])
plot(coeftomed1[,5])
par(old.par) #reset 

old.par <- par(mfrow=c(2, 2))
plot(coeftomed1[,6])
plot(coeftomed1[,7]) #possible outlier 
plot(coeftomed1[,8]) #outlier 
plot(coeftomed1[,9]) #inf - num_images
#there is extremely big thing here
#plot(class1[,"num_imgs"]) 
#there are 3 probable outliers
par(old.par) #reset 

old.par <- par(mfrow=c(2, 2))
plot(coeftomed1[,10]) #two outliers 
plot(coeftomed1[,11]) #one outlier 
plot(coeftomed1[,12]) 
plot(coeftomed1[,13]) #two outliers
par(old.par) #reset 


old.par <- par(mfrow=c(2, 2))
plot(coeftomed1[,14]) # 4 outliers 
plot(coeftomed1[,15]) # 3 outliers 
plot(coeftomed1[,16]) 
plot(coeftomed1[,17]) 
par(old.par) #reset



old.par <- par(mfrow=c(2, 2))
plot(coeftomed1[,18])  
plot(coeftomed1[,19]) 
plot(coeftomed1[,20]) # 1 outlier
plot(coeftomed1[,21]) # 2 outliers
par(old.par) #reset



old.par <- par(mfrow=c(2, 2))
plot(coeftomed1[,22]) #3 outliers 
plot(coeftomed1[,23])  
plot(coeftomed1[,24]) # 3 
plot(coeftomed1[,25]) 
par(old.par) #reset


old.par <- par(mfrow=c(2, 2))
plot(coeftomed1[,26]) 
plot(coeftomed1[,27]) 
plot(coeftomed1[,28]) 
plot(coeftomed1[,29]) 
par(old.par) #reset


old.par <- par(mfrow=c(2, 2))
plot(coeftomed1[,30]) 
plot(coeftomed1[,31])  
plot(coeftomed1[,32]) 
plot(coeftomed1[,33]) 
par(old.par) #reset


old.par <- par(mfrow=c(2, 2))
plot(coeftomed1[,34])  
plot(coeftomed1[,35])  
plot(coeftomed1[,36]) 
plot(coeftomed1[,37]) #could be 3 outliers
par(old.par) #reset

old.par <- par(mfrow=c(2, 2))
plot(coeftomed1[,38])  
plot(coeftomed1[,39])  
plot(coeftomed1[,40]) 
plot(coeftomed1[,41]) 
par(old.par) #reset

old.par <- par(mfrow=c(2, 2))
plot(coeftomed1[,41])  
plot(coeftomed1[,42])  #inf-max pos popularity #no idea why there is inf??
plot(coeftomed1[,43]) 
plot(coeftomed1[,44]) #inf - abs title subjectivity #here median is zero!
par(old.par) #reset

#plot(class1[,42])

##################
# OUTLIERS 
##################

# Identify outliers' id's

order(data[,3],decreasing=T)[1]
#2960
order(data[,7],decreasing=T)[1]
#985
order(data[,8],decreasing=T)[1]
#5532
order(data[,9],decreasing=T)[1]
#4867 2960 12575
order(data[,10],decreasing=T)[2]
#12252 19826
order(data[,11],decreasing=T)[1]
#5963
order(data[,13],decreasing=T)[2]
#5406 13291
order(data[,14],decreasing=T)[1:4]
#16546 27619  3612 19100
order(data[,15],decreasing=T)[1:3]
#27619  3612  6621
order(data[,20],decreasing=T)[1]
#16546
order(data[,21],decreasing=T)[1:2]
#27619  3618
order(data[,22],decreasing=T)[1:3]
#11188  3326  4409
order(data[,24],decreasing=T)[1:3]
#11188  3326  4409
order(data[,37],decreasing=T)[1:3]
#477 1925 4287

## Outliers' id's
withoutclasss1<-c(2960,985,5532,4867,2960,12575,12252,
                  19826,5963,5406,13291,16546,27619,3612,
                  19100,27619, 3612, 6621,
                  16546,27619, 3618,11188,3326,4409,
                  477,1925,4287)

##################
# CLEAN DATA
##################

#The data without class1 outliers
data<-data[!(data[,1] %in% withoutclasss1), ]
write.csv(data,"cleaned_class1_data.csv", row.names = FALSE)
