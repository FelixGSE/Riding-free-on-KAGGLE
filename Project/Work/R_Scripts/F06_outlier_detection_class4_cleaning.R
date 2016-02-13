################################################################
### OUTLIER DETECTION, USING PLOTS: CLASS4 
###############################################################


##################
# Read the data
##################

# 1. We read the already cleaned data

data<-read.csv("cleaned_class3_data.csv")

# 2. subset for popularity class 4
class4<- subset(data, data[,ncol(data)] == 4)

##################
# MEDIAN COEFF
##################

# We use coefficient to the median to look 
# for outliers
# advantages: smaller scale and if it is INF 
# than either the median=0 or there is extreme 
# value 


## FIND MEDIAN
coeftomed4<-matrix(NA,nrow(class4),(ncol(class4)-1))

med4<- sapply(2:(ncol(class4)-1), function(x) median(class4[,x]))

for( i in 2:(ncol(class4)-1)){
  for(j in 1:nrow(class4)){
    coeftomed4[j,i] <- abs(class4[j,i]/med4[i])
  }
}
#use the same names as data
colnames(coeftomed4)<-names(data)[-46]

##################
# PLOT 4x4 
##################

## Try eyebolling

#we separate the pictures into 4  

old.par <- par(mfrow=c(2, 2))
plot(coeftomed4[,2]) 
plot(coeftomed4[,3]) 
plot(coeftomed4[,4])
plot(coeftomed4[,5])
par(old.par) #reset 

old.par <- par(mfrow=c(2, 2))
plot(coeftomed4[,6])
plot(coeftomed4[,7]) 
plot(coeftomed4[,8]) 
plot(coeftomed4[,9]) 
par(old.par) #reset 

old.par <- par(mfrow=c(2, 2))
plot(coeftomed4[,10]) 
plot(coeftomed4[,11])  
plot(coeftomed4[,12]) 
plot(coeftomed4[,13]) 
par(old.par) #reset 


old.par <- par(mfrow=c(2, 2))
plot(coeftomed4[,14]) 
plot(coeftomed4[,15])  
plot(coeftomed4[,16]) 
plot(coeftomed4[,17]) 
par(old.par) #reset



old.par <- par(mfrow=c(2, 2))
plot(coeftomed4[,18])  
plot(coeftomed4[,19]) 
plot(coeftomed4[,20]) 
plot(coeftomed4[,21]) 
par(old.par) #reset



old.par <- par(mfrow=c(2, 2))
plot(coeftomed4[,22])  
plot(coeftomed4[,23])  
plot(coeftomed4[,24])  
plot(coeftomed4[,25]) 
par(old.par) #reset


old.par <- par(mfrow=c(2, 2))
plot(coeftomed4[,26]) 
plot(coeftomed4[,27]) 
plot(coeftomed4[,28]) 
plot(coeftomed4[,29]) 
par(old.par) #reset


old.par <- par(mfrow=c(2, 2))
plot(coeftomed4[,30]) 
plot(coeftomed4[,31])  
plot(coeftomed4[,32]) 
plot(coeftomed4[,33]) 
par(old.par) #reset


old.par <- par(mfrow=c(2, 2))
plot(coeftomed4[,34])  
plot(coeftomed4[,35])  
plot(coeftomed4[,36]) 
plot(coeftomed4[,37]) 
par(old.par) #reset

old.par <- par(mfrow=c(2, 2))
plot(coeftomed4[,38])  
plot(coeftomed4[,39])  
plot(coeftomed4[,40]) 
plot(coeftomed4[,41]) 
par(old.par) #reset

old.par <- par(mfrow=c(2, 2))
plot(coeftomed4[,41])  
plot(coeftomed4[,42]) 
plot(coeftomed4[,43]) 
plot(coeftomed4[,44]) 
par(old.par) #reset

##################
# OUTLIERS 
##################

# Identify outliers' id's

o1<-order(data[,3],decreasing=T)[1:3]
o2<-order(data[,7],decreasing=T)[1]
o3<-order(data[,8],decreasing=T)[1]
o4<-order(data[,9],decreasing=T)[1]
o5<-order(data[,10],decreasing=T)[1:2]
o6<-order(data[,11],decreasing=T)[1:2]
o7<-order(data[,14],decreasing=T)[1:3]
o8<-order(data[,15],decreasing=T)[1:3]
o9<-order(data[,16],decreasing=T)[1:3]
o10<-order(data[,18],decreasing=T)[1:3]
o11<-order(data[,21],decreasing=T)[1:2]
o12<-order(data[,22],decreasing=T)[1:3]
o13<-order(data[,23],decreasing=T)[1:3]
o14<-order(data[,24],decreasing=T)[1]
o15<-order(data[,31],decreasing=T)[1:2]
o16<-order(data[,32],decreasing=T)[1:2]
o17<-order(data[,33],decreasing=T)[1]


## Outliers' id's
withoutclasss4<-c(o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,
                  o11,o12,o13,o14,o15,o16,o17)

##################
# CLEAN DATA
##################

#The data without class4 outliers

data<-data[!(data[,1] %in% withoutclasss4), ]
write.csv(data,"cleaned_class4_data.csv", row.names = FALSE)
