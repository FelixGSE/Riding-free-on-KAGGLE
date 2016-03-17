#***********************************************
#Exploration of KW variables 
#***********************************************

setwd("/home/didi/BGSE/semester2/adcomp/Kaggle")
library(ggplot2)
#data with ids
data <- read.csv('news_popularity_training.csv',header=TRUE,sep=',')
y <- data[,"popularity"]; 
x <- data[,-ncol(data)]; 
x <- x[,-2]; #removing the url 
x <- x[,-2]; #removing the delta

#Explore the correltion between unique_tokens and kw 
qplot(data$n_unique_tokens, data=data, geom="density", fill=factor(, alpha=I(.5), 
      main="kw_min_min", xlab="kw_min_min", 
      ylab="Density")

# create new variable for keywords 
kw<-x[,seq(19,27)]
#new df for the kw and y
new<-cbind(kw,y)
#target<-seq(0,nrow(new))
#separate only class 1= 1 and rest =0
dummy <- as.numeric(y>2)
new<-cbind(new,dummy)
#Investigate again using plot
qplot(new[,1], data=new, geom="density", fill=factor(new[,ncol(new)]), alpha=I(.5), 
      main="kw_min_min", xlab="kw_min_min", 
      ylab="Density")

qplot(new[,5], data=new, geom="density", fill=factor(new[,ncol(new)]), alpha=I(.5), 
      main="kw_max_max", xlab="max_max_max", 
      ylab="Density")
#NOTE: kw_max_max more shares in 1,2 pop than in 4,3,5!!!!

# Density plots

qplot(new[,c(2)], data=new, geom="density", fill=factor(new[,ncol(new)]), alpha=I(.5), 
      main="kw_max_min", xlab="max_max_min", 
      ylab="Density")
par(1)

#again higher for 1,2
qplot(new[,7], data=new, geom="density", fill=factor(new[,ncol(new)]), alpha=I(.5), 
      main="kw_max_max", xlab="max_max_max", 
      ylab="Density")


qplot(y, data=new, geom="freqpoly", group=cut, colour=cut, position="identity"

#data with binary labels
binarypop<-new[,-11]

p1 <- as.numeric(y==1)
p2 <- as.numeric(y==2)
p3 <- as.numeric(y==3)
p4 <- as.numeric(y==4)
p5 <- as.numeric(y==5)

banana<- cbind(new,p1,p2,p3,p4,p5)

#let's do some correlation
library(corrplot)
m<-cor(new)
m1<-cor(binarypop)
m2<- cor(x[,c(seq(13,18),seq(31,38))])

m_banana<- cor(banana)
corrplot(m, method="circle")
corrplot(m1, method="circle", type="up")
corrplot(m2, method="number")
corrplot(m_banana, method="circle")

dow_data3<- cbind(x[,c(1,seq(31,38))],y)


library(vcd)
l <- reshape(dow_data3, 
             varying = c("weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday",
                         "weekday_is_thursday", "weekday_is_friday","weekday_is_saturday","weekday_is_sunday"), 
             v.names = "id",
             timevar = "dow", 
             times = c("weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday",
                       "weekday_is_thursday", "weekday_is_friday","weekday_is_saturday","weekday_is_sunday"), 
             #new.row.names = 1:1000,
             direction = "long")

mosaic(as.data.frame(mat),  shade=TRUE, legend=TRUE)

which( abs(a) < 0.05, arr.ind=T )
