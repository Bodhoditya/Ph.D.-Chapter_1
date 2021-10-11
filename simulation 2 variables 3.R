################n=100##################
library(MASS)
library(ggplot2)
library(lattice)
library(caret)
options(max.print = 1000000)
beta0=c(1,2,3)
K=length(beta0)
nsim=10000
gam=0.5
N=100
N1=200
Sigma=matrix(c(1,0.3,0,0.3,1,0,0,0,1),3,3)
#############Estimates if Beta###################
beta=matrix(0,K,nsim)
for (i in 1:nsim) {
  R=mvrnorm(N,c(0,0,0),Sigma)
  X=cbind(matrix(1,N,1),R[,c(1,2)])
  eps=R[,3]
  y=X%*%beta0+eps
  beta[,i]=solve((t(X)%*%X),t(X)%*%y);
}
est=print(list("beta0"=beta0,"beta_ols (average)"=rowMeans(beta,2)))
est
######################Generate train and test date ################
misclaserror=rep(0,nsim)
for (i in 1:nsim) {
  R1=mvrnorm(N1,c(0,0,0),Sigma)
  X1=cbind(matrix(1,N1,1),R1[,c(1,2)])
  eps1=R1[,3]
  y1=X1%*%est$`beta_ols (average)`+eps1
  y11=as.matrix(y1,ncol=1,`colnames<-`(y))
  y_mean=mean(y1)
  data=cbind(y11,R1[,c(1,2)])
  X11=as.matrix(data, colnames=c("y","X1","X2"))
  colnames(X11) <- c("y", "x1", "x2")
  class = ifelse(y11[,1]>y_mean,1,0)
  data = cbind(X11,class)
  Data=as.data.frame(data)
  TD1=Data[,c(2,3)]
  PR=prcomp(TD1)
  summary(PR)
  PCV=PR$x
  Data1=cbind(PCV,class)
  Data2=as.data.frame(Data1)
  train_index <- createDataPartition(Data2$class, p=0.50, list=FALSE)
  testdata <- Data2[-train_index,]
  traindata <- Data2[train_index,]
  r1=lda(class~PC1+PC2,data=traindata)
  r2=predict(object=r1,testdata)
  T=table(testdata[,3],r2$class)
  T
  misclaserror[i]=1-sum(diag(T))/sum(T)
}
mean(misclaserror)
##########################n=500#####################

library(MASS)
library(ggplot2)
library(lattice)
library(caret)
options(max.print = 1000000)
beta0=c(1,2,3)
K=length(beta0)
nsim=10000
gam=0.5
N=500
N1=1000
Sigma=matrix(c(1,0.3,0,0.3,1,0,0,0,1),3,3)
#############Estimates if Beta###################
beta=matrix(0,K,nsim)
for (i in 1:nsim) {
  R=mvrnorm(N,c(0,0,0),Sigma)
  X=cbind(matrix(1,N,1),R[,c(1,2)])
  eps=R[,3]
  y=X%*%beta0+eps
  beta[,i]=solve((t(X)%*%X),t(X)%*%y);
}
est=print(list("beta0"=beta0,"beta_ols (average)"=rowMeans(beta,2)))
est
######################Generate train and test date ################
misclaserror=rep(0,nsim)
for (i in 1:nsim) {
  R1=mvrnorm(N1,c(0,0,0),Sigma)
  X1=cbind(matrix(1,N1,1),R1[,c(1,2)])
  eps1=R1[,3]
  y1=X1%*%est$`beta_ols (average)`+eps1
  y11=as.matrix(y1,ncol=1,`colnames<-`(y))
  y_mean=mean(y1)
  data=cbind(y11,R1[,c(1,2)])
  X11=as.matrix(data, colnames=c("y","X1","X2"))
  colnames(X11) <- c("y", "x1", "x2")
  class = ifelse(y11[,1]>y_mean,1,0)
  data = cbind(X11,class)
  Data=as.data.frame(data)
  TD1=Data[,c(2,3)]
  PR=prcomp(TD1)
  summary(PR)
  PCV=PR$x
  Data1=cbind(PCV,class)
  Data2=as.data.frame(Data1)
  train_index <- createDataPartition(Data2$class, p=0.50, list=FALSE)
  testdata <- Data2[-train_index,]
  traindata <- Data2[train_index,]
  r1=lda(class~PC1+PC2,data=traindata)
  r2=predict(object=r1,testdata)
  T=table(testdata[,3],r2$class)
  T
  misclaserror[i]=1-sum(diag(T))/sum(T)
}
mean(misclaserror)