#different methods

boost <- function(data) {boosting(popularity~., data=data)}
pmlr <- function(data) {pmlr(popularity~., data=data)}
bag <- function(data) {bagging(popularity~., data=data)}
forest <- function(data) {randomForest(popularity~., data=data, do.trace=T)}
mnet <- function(data) {multinom(popularity~., data=data, trace=F)}
foT1000repFnod20 <- function(data) {randomForest(popularity~., data=data, ntree=1000, replace=F, nodes=20)}
foOptMtryNt1500 <- function(data) {
	mtry <- tuneRF(data[,-1], data[,1], ntreeTry=1501, trace=F)
	randomForest(popularity~., data=data, mtry=as.numeric(as.matrix(mtry[order(mtry[,2]),])[1,1]), ntree=1501)
}
foOptMtry <- function(data) {
	mtry <- tuneRF(data[,-1], data[,1], trace=F)
	randomForest(popularity~., data=data, mtry=as.numeric(as.matrix(mtry[order(mtry[,2]),])[1,1]))
}
foOptMtryStart30step1p5 <- function(data) {
	mtry <- tuneRF(data[,-1], data[,1], trace=F, stepFactor=1.5, mtryStart=30)
	randomForest(popularity~., data=data, mtry=as.numeric(as.matrix(mtry[order(mtry[,2]),])[1,1]))
}
forestMtry12Tr2000 <- function(data) {randomForest(popularity~., data=data, mtry=12, ntree=2000)}
forestMtry50 <- function(data) {randomForest(popularity~., data=data, mtry=50)}
forestMtry15Ntree3000 <- function(data) {randomForest(popularity~., data=data, mtry=15, ntree=3000)}
forestMtry5 <- function(data) {randomForest(popularity~., data=data, mtry=50)}
forestBal <- function(data) {randomForest(popularity~., data=data, replace=T, sampsize=c(10, 10, 10, 10, 10))}
wsrf <- function(data) {wsrf(popularity~., data=data)}
forest2000 <- function(data) {randomForest(popularity~., data=data, ntree=2000, mtry=30)}

