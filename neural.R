# Data
getwd()
setwd("C:/Users/DHEERAJ/Documents")
data <- read.csv("mdf.csv", header = TRUE)
data=data[2:6]
data
data1=data[1:2]
data1
data1=data.frame(data1,data$p)
names(data1)=c("a","b","p")
rdata1=data1
rdata=data
rdata
# Min-Max Normalization
data$a <- (data$a - min(data$a))/(max(data$a) - min(data$a))
data$b <- (data$b - min(data$b))/(max(data$b) - min(data$b))
data$c <- (data$c - min(data$c))/(max(data$c) - min(data$c))
data$d <- (data$d - min(data$d))/(max(data$d) -min(data$d))
data$p <- (data$p - min(data$p))/(max(data$p )-min(data$p))
data1$a <- (data1$a - min(data1$a))/(max(data1$a) - min(data1$a))
data1$b <- (data1$b - min(data1$b))/(max(data1$b) - min(data1$b))
data1$p <- (data1$p - min(data1$p))/(max(data1$p )-min(data1$p))

# Data Partition
set.seed(222)
training <- data[1:62,]
testing <- data[63:78,]
train=rdata[1:62,]
test=rdata[63:78,]
training1 <- data1[1:62,]
testing1<- data1[63:78,]
train1=rdata1[1:62,]
test1=rdata1[63:78,]

# Neural Networks
library(neuralnet)
set.seed(333)
softplus <- function(x) log(1+exp(x))
n <- neuralnet(p~a+b+c+d,act.fct = softplus,
               data = training,
               hidden =c(2,3),
            
               linear.output = FALSE)
n1 <- neuralnet(p~a+b,
               data = training1,
               hidden =c(2,3),
               
               linear.output = FALSE)

plot(n)
plot(n1)

# Prediction
pr.nn<- compute(n, testing[,c(1:4)])
pr.nn
pr.nn_=(pr.nn$net.result)*(max(rdata$p)-min(rdata$p))+min(rdata$p)


pr.nn1<- compute(n1, testing1[,c(1:2)])
pr.nn1
pr.nn1_=(pr.nn1$net.result)*(max(rdata1$p)-min(rdata1$p))+min(rdata1$p)

#summary(foutput)
#testing$p=(testing$p)*(max(rdata$p)-min(rdata$p))+min(rdata$p)
RMSE.nn = (sum((test$p -pr.nn_ )^2) / nrow(testing)) ^ 0.5
MSE.nn <- sum((test$p - pr.nn_)^2)/nrow(testing)
RMSE.nn1 = (sum((test1$p -pr.nn1_ )^2) / nrow(testing1)) ^ 0.5
MSE.nn1 <- sum((test1$p - pr.nn1_)^2)/nrow(testing1)

RMSE.nn_ = (sum((mydata$Close[463:478] -pr.nn_ )^2) / nrow(testing)) ^ 0.5
MSE.nn_ <- sum((mydata$Close[463:478] - pr.nn_)^2)/nrow(testing)
RMSE.nn1_ = (sum((mydata$Close[463:478] -pr.nn1_ )^2) / nrow(testing1)) ^ 0.5
MSE.nn1_ <- sum((mydata$Close[463:478] - pr.nn1_)^2)/nrow(testing1)











e1=test$p -pr.nn_
e2=test1$p -pr.nn1_
hist(data$p)
plot(e1)

