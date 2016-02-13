################################################################
### OUTLIER DETECTION, USING PLOTS: CLASS 
###############################################################


##################
# Read the data
##################

# 1. We read the already cleaned data

data<-read.csv("cleaned_class2_data.csv")

# 2. subset for popularity class 3
class3<- subset(data, data[,ncol(data)] == 3)

##################
# MEDIAN COEFF
##################

# We use coefficient to the median to look 
# for outliers
# advantages: smaller scale and if it is INF 
# than either the median=0 or there is extreme 
# value 


## FIND MEDIAN

coeftomed3<-matrix(NA,nrow(class3),(ncol(class3)-1))

med3<- sapply(2:(ncol(class3)-1), function(x) median(class3[,x]))

for( i in 2:(ncol(class3)-1)){
  for(j in 1:nrow(class3)){
    coeftomed3[j,i] <- abs(class3[j,i]/med3[i])
  }
}
#use the same names as data
colnames(coeftomed3)<-names(data)[-46]

##################
# PLOT 4x4 
##################

## Try eyebolling

old.par <- par(mfrow=c(2, 2))
plot(coeftomed3[,2]) 
plot(coeftomed3[,3]) 
plot(coeftomed3[,4])
plot(coeftomed3[,5])
par(old.par) #reset 

old.par <- par(mfrow=c(2, 2))
plot(coeftomed3[,6])
plot(coeftomed3[,7]) 
plot(coeftomed3[,8]) 
plot(coeftomed3[,9]) 
par(old.par) #reset 

old.par <- par(mfrow=c(2, 2))
plot(coeftomed3[,10]) 
plot(coeftomed3[,11])  
plot(coeftomed3[,12]) 
plot(coeftomed3[,13]) 
par(old.par) #reset 


old.par <- par(mfrow=c(2, 2))
plot(coeftomed3[,14]) 
plot(coeftomed3[,15])  
plot(coeftomed3[,16]) 
plot(coeftomed3[,17]) 
par(old.par) #reset



old.par <- par(mfrow=c(2, 2))
plot(coeftomed3[,18])  
plot(coeftomed3[,19]) 
plot(coeftomed3[,20]) 
plot(coeftomed3[,21]) 
par(old.par) #reset



old.par <- par(mfrow=c(2, 2))
plot(coeftomed3[,22])  
plot(coeftomed3[,23])  
plot(coeftomed3[,24])  
plot(coeftomed3[,25]) 
par(old.par) #reset


old.par <- par(mfrow=c(2, 2))
plot(coeftomed3[,26]) 
plot(coeftomed3[,27]) 
plot(coeftomed3[,28]) 
plot(coeftomed3[,29]) 
par(old.par) #reset


old.par <- par(mfrow=c(2, 2))
plot(coeftomed3[,30]) 
plot(coeftomed3[,31])  
plot(coeftomed3[,32]) 
plot(coeftomed3[,33]) 
par(old.par) #reset


old.par <- par(mfrow=c(2, 2))
plot(coeftomed3[,34])  
plot(coeftomed3[,35])  
plot(coeftomed3[,36]) 
plot(coeftomed3[,37]) 
par(old.par) #reset

old.par <- par(mfrow=c(2, 2))
plot(coeftomed3[,38])  
plot(coeftomed3[,39])  
plot(coeftomed3[,40]) 
plot(coeftomed3[,41]) 
par(old.par) #reset

old.par <- par(mfrow=c(2, 2))
plot(coeftomed3[,41])  
plot(coeftomed3[,42]) 
plot(coeftomed3[,43]) 
plot(coeftomed3[,44]) 
par(old.par) #reset


##################
# OUTLIERS 
##################

# Identify outliers' id's

o1<-order(data[,2],decreasing=T)[1]
o2<-order(data[,3],decreasing=T)[1:3]
o3<-order(data[,4],decreasing=T)[1]
o4<-order(data[,5],decreasing=T)[1]
o5<-order(data[,6],decreasing=T)[1]
o6<-order(data[,7],decreasing=T)[1:2]
o7<-class3[(class3[,13]> 50 & class3[,13]<200),1]
o8<-order(data[,14],decreasing=T)[1:3]
o9<-order(data[,15],decreasing=T)[1:3]
o10<-order(data[,20],decreasing=T)[1]
o11<-order(data[,21],decreasing=T)[1]
o12<-order(data[,22],decreasing=T)[1:6]
o13<-order(data[,23],decreasing=T)[1:4]
o14<-order(data[,30],decreasing=T)[1:2]
o15<-order(data[,31],decreasing=T)[1:2]
o16<-order(data[,33],decreasing=T)[1]
o17<-order(data[,37],decreasing=T)[1:3]


## Outliers' id's
withoutclasss3<-c(o1,o2,o3,o4,o5,o6,o7,o8,o9,
                  o10,o11,o12,o13,o14,o15,o16,o17)

##################
# CLEAN DATA
##################

#The data without class3 outliers

data<-data[!(data[,1] %in% withoutclasss3), ]
write.csv(data,"cleaned_class3_data.csv", row.names = FALSE)
