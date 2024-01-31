library(survey) 
library(sampling) 
library(car)
mydata <- read.csv("C:/Users/ASUS/Desktop/APPENC02.csv", header = FALSE)
# Import data into a data frame to form a list
mydata<-data.frame(mydata)
mydata$y<-(mydata$V10)/100000
mydata<-mydata[,-10]
mydata<-mydata[,-c(1:3)]
summary(mydata)
# a seed number for reproducibility purposes
#set.seed(1234)
# 396 samples were taken(sampling without replacement)
#mata<-sample(nrow(mydata), 396, replace = FALSE, prob = NULL)
#train_data=mydata[mata,]
#test_data=mydata[-mata,]
#train_data<-scale(train_data,center=T,scale=T)
#test_data<-scale(test_data,center=T,scale=T)
#cor(train_data)
#普通回归
mydata<-scale(mydata,center=T,scale=T)
mydata<-as.data.frame(mydata)
fit2<-lm(y~V13+V15+V16,data=mydata)

#fit2<-lm(y~V4+V6+V7+V11+V12+V13+V14+V15+V16,data=mydata)
summary(fit2)
#逐步回归
tstep<-step(fit2)
summary(tstep)
fit2.Y<-predict(fit2,test_data)
plot(fit2.Y,testnY,xlab="predicted",ylab="actual",main="ols regression")
sst <- sum((testnY- mean(testnY))^2)
sse <- sum((fit2.Y- testnY)^2)# R方
rsq <-1- sse / sst
rsq 
#检查多重共线性
train_data<-as.data.frame(train_data[,c(14,2,5,6,12)])
test_data<-as.data.frame(test_data[,c(14,2,5,6,12)])
fit<-lm(y~V5+V8+V9+V16,data=train_data)
summary(fit)
vif(fit)
mdata<-as.data.frame(train_data[,c(2,5,6,12)])
pairs(mydata)
#岭回归
library(tidyverse) 
library(broom) 
library(glmnet) 
mydata<-as.matrix(mydata)
test_data<-as.matrix(test_data)
Y<-mydata[,14]
X<-mydata[,-14]
X<-mydata[,c(1,3,4,7,8,9,10,11,12)]
nY<-test_data[,1]
nX<-test_data[,-1]
#交叉验证法确定s
cvfit<-cv.glmnet(X,Y,type.measure = "mse",nfolds = 5,alpha=0)
plot(cvfit)
#取lambda.min时均方误差最小
cvfit$lambda.min
#另一种方法确定s
ridge<-glmnet(X,Y,family = "gaussian",alpha=0)
ridge
plot(ridge,label = TRUE)
#岭迹图
plot(ridge,xvar="lambda",label = TRUE)
#
library(MASS)
lm.ridge(Y~V4+V13+V15+V16,mydata,lambda=seq(0,0.1,0.001))
plot(lm.ridge(Y~V13+V15+V16,mydata,lambda=seq(0,0.1,0.001))) 
#为了避免过拟合，所以s不选用0.08，而是大致的且常用的0.1
ridge.coef<-predict(ridge,s=0.1,type="coefficients")
ridge.coef
ridge.Y<-predict(ridge,newx=nX,type = "response",s=0.1)
plot(ridge.Y,nY,xlab="predicted",ylab="actual",main="ridge regression")
sst <- sum((nY- mean(nY))^2)
sse <- sum((ridge.Y- nY)^2)# R方
rsq <-1- sse / sst
rsq 
#随机森林
set.seed(1234)
# 396 samples were taken(sampling without replacement)
mata<-sample(nrow(mydata), 396, replace = FALSE, prob = NULL)
train_data=mydata[mata,]
test_data=mydata[-mata,]
train_data<-scale(train_data,center=T,scale=T)
test_data<-scale(test_data,center=T,scale=T)
rf<-randomForest(y~V13+V15+V16,data=train_data,importance=TRUE,ntree = 500)
print(rf)
importance(rf)
varImpPlot(rf)
pre_data <- predict(rf,newdata=test_data)
test_data<-data.frame(test_data)
obs_p_data = data.frame(prob=pre_data,obs=test_data$y)
library(ggplot2)  ### 导入ggplot2包
plot1 <- ggplot(data = test_data, aes(x = c(1:44))) + geom_line(aes(y = y, linetype = "实际值", colour = "实际值"), size = 0.8) ### 画实际值得曲线
plot2 <- plot1 + geom_line(aes(y = pre_data, linetype = "预测值", colour = "预测值"), size = 0.8) ### 画预测值曲线
plot2 + scale_linetype_manual(name = "", values = c("实际值" = "solid", "预测值" = "twodash")) +
  scale_colour_manual(name = "", values = c("实际值" = "red", "预测值" = "blue"))  ### 设置图例
ran_roc <- roc(test_data$y,as.numeric(pre_data))
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='随机森林模型ROC曲线,mtry=3,ntree=500')