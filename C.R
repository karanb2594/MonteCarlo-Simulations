#The libraries needed to run.
library(quantmod)
library(xts)
library(ggplot2)
library(tseries)
#library(truncnorm)

#The main function
myfunction<-function(tick1,tick2,tick3,tick4,tick5,wt1,wt2,wt3,wt4,wt5,initial,num)
{
ticker<-c(tick1,tick2,tick3,tick4,tick5)
weight<-c(wt1,wt2,wt3,wt4,wt5)
init<-initial
n<-num
real<-NULL
real<-xts(real)

#simd is used to simulate the stock prices of any number of tickers
simd<-matrix(data=NA,ncol=length(ticker),nrow=n)

#return2 will be used to calculae the returns of the 
return2<-matrix(data=NA,ncol=length(ticker),nrow=n)

#total will give the Value of Total Simulated Portfolio
total<-matrix(data=NA,ncol=1,nrow=10)


valueport<-matrix(data=NA,ncol=length(ticker),nrow=n)
lastpr<-matrix(data=NA,ncol=length(ticker),nrow=1)
finalresult<-matrix(data=NA,ncol=2,nrow=1)

#Calculates number of shares
number<-matrix(data=NA,nrow=1,ncol=length(ticker))

meanr<-NULL

#Getting the Historical data of the tickers
for(i in 1:length(ticker))
{
  prices<-getSymbols(ticker[i],from="2007-01-01",auto.assign=F)
  returns<-periodReturn(prices, period = "daily", type = "arithmetic")
  real<-merge.xts(real,returns)
  meanr<-colMeans(real,na.rm = F)
  meanr
  vol<-sd(real,na.rm = T)*sqrt(252)
  sdr<-vol^0.5
  lp<-last(prices[,ncol(prices)])
  lastpr[1,i]<-last(prices[nrow(prices),ncol(prices)])
  prices<-as.data.frame(prices)
  
    for(j in 1:n)
  {
    stock1<-lp*(1+meanr[i]*(1/252)+sdr*(1/252)^0.5*rnorm(1,0,1))
    simd[j,i]<-stock1
  }
}

colnames(simd)<-c(ticker)

#Calculating returns for simulated stock prices
for(i in 1:length(ticker))
{
  for(j in 1:(n-1))
  {
    return2[j+1,i]<-(simd[j+1,i]-simd[j,i])/simd[j,i]
  }
}
colnames(return2)<-c(ticker)

#Calculating the Means
mean2<-colMeans(return2,na.rm=T)

#Calculating the Covariance of simulated stocks
covar<-var(return2,na.rm=T)

weight<-as.matrix(weight/100)

finalresult[1,1]<-sum(weight*mean2)
sig<-t(weight)%*%covar%*%weight
finalresult[1,2]<-sqrt(sig)
colnames(finalresult)<-c("ExpectedReturn","StandardDeviation")

#Calculating the number of shares of each stock
for(i in 1:length(ticker))
{
  number[1,i]<-(weight[i,1]*init)/lastpr[1,i]
}
number<-floor(number)
colnames(number)<-c(ticker)
rownames(number)<-c("No_of_Shares")

for(i in 1:n)
{
  for(j in 1:length(ticker))
  {
    valueport[i,j]<-simd[i,j]*number[1,j]
  }
}
colnames(valueport)<-c(ticker)
total<-data.frame(rowSums(valueport))
colnames(total)<-c("SimulatedTotal")
num<-data.frame(seq(1:n))
total<-cbind(num,total)
colnames(total)<-c("No_of_days","SimulatedTotal")
total<-data.frame(total)

gg<-ggplot(data=total,aes(x=No_of_days,y=SimulatedTotal)) + geom_point(data=total,color="black",size=1) + ggtitle("Value of Portfolio over Days") + geom_line()

#Output
g<-list(gg,number,simd,valueport,total)
return(g)
}


