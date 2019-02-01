library(MASS)
library(tseries)
library(forecast)
getwd()
setwd("C:/Users/DHEERAJ/Documents/dataset")
mydata<-read.csv("SPX.csv")
View(mydata)
attach(mydata)
m1<-mydata$Adj.Close[1:500]
m1
logstock=diff(log(m1),lag=1)
logstock
plot(logstock,type='l',main='log returns plot')
difflnstock=diff(logstock)
difflnstock

#ACF,PACF and DIckey Fuller test

adf.test(logstock,alternative="stationary",k=0)
#adf.test(logstock,alternative = "explosive",k=0)

acf(logstock) # #estimate 
pacf(logstock,lag.max=20)

acf(difflnstock)
pacf(difflnstock)

arima(m1,order=c(1,0,0))
arima(m1,order=c(2,0,0))
arima(m1,order=c(2,0,2))
#least aic values and ar1 values should not be more or near to one
# arima model uing function
fit=arima(m1,order=c(2,0,2))
summary(fit)
tsdiag(fit)
fit
predicted=arimapredit<-predict(fit,n.ahead=100)
predicted
plot(m1)
plot(predicted$pred)
#table1=data.frame(mydata$Close[501:600],predicted$pred,predicted$se)
#lines(m1,predicted$pred,col="blue")
mdataset=predicted$pred
df=data.frame(mdataset,mydata$Close[501:600])
error=df$mdataset-df$mydata.Close.501.600
mdf=data.frame(mdataset[2:99],mdataset[1:98],error[2:99],error[1:98],mdataset[3:100])
names(mdf)=c("a","b","c","d","p")

