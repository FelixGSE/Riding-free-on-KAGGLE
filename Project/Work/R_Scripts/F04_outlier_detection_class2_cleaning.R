################################################################
### OUTLIER DETECTION, USING PLOTS: CLASS2 
###############################################################


##################
# Read the data
##################

# 1. We read the already cleaned data
data<-read.csv("cleaned_class1_data.csv")

# 2. subset for popularity class 2
class2<- subset(data, data[,ncol(data)] == 2)

##################
# MEDIAN COEFF
##################

# We use coefficient to the median to look 
# for outliers
# advantages: smaller scale and if it is INF 
# than either the median=0 or there is extreme 
# value 


## FIND MEDIAN
coeftomed2<-matrix(NA,nrow(class2),(ncol(class2)-1))
med2<- sapply(2:(ncol(class2)-1), function(x) median(class2[,x]))

for( i in 2:(ncol(class2)-1)){
  for(j in 1:nrow(class2)){
    coeftomed2[j,i] <- abs(class2[j,i]/med2[i])
  }
}
#use the same names as data
colnames(coeftomed2)<-names(data)[-46]


##################
# PLOT 4x4 
##################

## Try eyebolling

old.par <- par(mfrow=c(2, 2))
plot(coeftomed2[,2]) # possible 3 outliers 
plot(coeftomed2[,3]) 
plot(coeftomed2[,4])
plot(coeftomed2[,5])
par(old.par) #reset 

old.par <- par(mfrow=c(2, 2))
plot(coeftomed2[,6])
plot(coeftomed2[,7]) 
plot(coeftomed2[,8]) 
plot(coeftomed2[,9]) 
par(old.par) #reset 

old.par <- par(mfrow=c(2, 2))
plot(coeftomed2[,10]) 
plot(coeftomed2[,11])  
plot(coeftomed2[,12]) 
plot(coeftomed2[,13]) 
par(old.par) #reset 


old.par <- par(mfrow=c(2, 2))
plot(coeftomed2[,14]) 
plot(coeftomed2[,15])  
plot(coeftomed2[,16]) 
plot(coeftomed2[,17]) 
par(old.par) #reset



old.par <- par(mfrow=c(2, 2))
plot(coeftomed2[,18])  
plot(coeftomed2[,19]) 
plot(coeftomed2[,20]) 
plot(coeftomed2[,21]) 
par(old.par) #reset



old.par <- par(mfrow=c(2, 2))
plot(coeftomed2[,22])  
plot(coeftomed2[,23])  
plot(coeftomed2[,24])  
plot(coeftomed2[,25]) 
par(old.par) #reset


old.par <- par(mfrow=c(2, 2))
plot(coeftomed2[,26]) 
plot(coeftomed2[,27]) 
plot(coeftomed2[,28]) 
plot(coeftomed2[,29]) 
par(old.par) #reset


old.par <- par(mfrow=c(2, 2))
plot(coeftomed2[,30]) 
plot(coeftomed2[,31])  
plot(coeftomed2[,32]) 
plot(coeftomed2[,33]) 
par(old.par) #reset


old.par <- par(mfrow=c(2, 2))
plot(coeftomed2[,34])  
plot(coeftomed2[,35])  
plot(coeftomed2[,36]) 
plot(coeftomed2[,37]) 
par(old.par) #reset

old.par <- par(mfrow=c(2, 2))
plot(coeftomed2[,38])  
plot(coeftomed2[,39])  
plot(coeftomed2[,40]) 
plot(coeftomed2[,41]) 
par(old.par) #reset

old.par <- par(mfrow=c(2, 2))
plot(coeftomed2[,41])  
plot(coeftomed2[,42]) 
plot(coeftomed2[,43]) 
plot(coeftomed2[,44]) 
par(old.par) #reset

##################
# OUTLIERS 
##################

# Identify outliers' id's

o1<-order(data[,2],decreasing=T)[1:3]
#19341 17706 24164
o2<-order(data[,3],decreasing=T)[1:3]
#14361  4377 28164
o3<-order(data[,7],decreasing=T)[1:3]
#7280  5092 20903
o4<-order(data[,8],decreasing=T)[1:3]
#12927 14396 16369
o5<-order(data[,13],decreasing=T)[1]
#13937
o6<-order(data[,14],decreasing=T)[1:4]
#14881 11026 22896  1140
o7<-order(data[,15],decreasing=T)[1:4]
#14881 11026 20277  5029
o8<-order(data[,20],decreasing=T)[1]
#7034
o9<-order(data[,21],decreasing=T)[1:2]
#27619  3612  6621
o10<-order(data[,20],decreasing=T)[1]
#16546
o11<-order(data[,21],decreasing=T)[1:2]
#19290  7034
o12<-order(data[,23],decreasing=T)[1:3]
#13324 22635 29231
o13<-order(data[,32],decreasing=T)[1]
#11820
o14<-order(data[,33],decreasing=T)[1:3]
#22270 21196 16355
o15<-order(data[,37],decreasing=T)[1:3]
#8482 14895 20834


## Outliers' id's
withoutclasss2<-c(o1,o2,o3,o4,o5,o6,o7,o8,
                  o9,o10,o11,o12,o13,o14,o15)

##################
# CLEAN DATA
##################

#The data without class2 outliers

data<-data[!(data[,1] %in% withoutclasss2), ]
write.csv(data,"cleaned_class2_data.csv", row.names = FALSE)
