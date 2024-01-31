#logistic regression

#1.从案例中建立直觉####

#汽车贷款违约的案例：逻辑回归构建初始信用评级
#数据accepts.csv

setwd("C:/Users/Lei Zhang/OneDrive - for personal/A_教学/互联网思维与数字化营销/02 glm")
accepts=read.csv("accepts.csv",stringsAsFactors = F)
str(accepts)
#数据解释见文件
table(accepts$bad_ind)
table(accepts$bankruptcy_ind)
cross=table(accepts[,'bankruptcy_ind'],accepts[,'bad_ind']) #生成列联表
cross
odd_n=cross[2,2]/cross[2,1]   #N组发生比
odd_y=cross[3,2]/cross[3,1]   #Y组发生比
odd_y/odd_n    #Y组对N组的优势比

accepts$bad_ind2=as.factor(accepts$bad_ind) # 
accepts$bankruptcy2_ind=as.factor(accepts$bankruptcy_ind)

##1.1 数据预处理####
library(dplyr) # 用这个包进行处理
accepts%>%summary()
accepts=na.omit(accepts)   #去除缺省的cases，去掉缺失值
base=table(accepts$bad_ind)
base %>% prop.table # 89%违约过

attach(accepts)
set.seed(100)

### 选择训练集和测试集
nrow(accepts)
length(accepts$application_id)*0.7  
select=sample(1:nrow(accepts),length(accepts$application_id)*0.7) ;select # sample取一部分的样本，数据来源，选多大
#70%的作为训练集
train=accepts[select,]  # select作为索引，，表示行，把行的索引，那一行的具体数据找出来作为训练集
test=accepts[-select,]    #测试集
#   train=as.data.frame(train)
#   GGally::ggpairs('train')

##1.2 第一次尝试####

glm1=glm(bad_ind ~fico_score+tot_derog,family=binomial(link='logit')) #  后面这个就是我要glm中的logit回归
summary(glm1)
b=coef(glm1);b

glm1=glm(bad_ind ~fico_score+age_oldest_tr,family=binomial(link='logit'))
summary(glm1)
b=coef(glm1);b

##看具体的数字和含义
#given x1=3,x2=4
logit=sum(b*c(1,696,2))
odd=exp(logit)
prob=odd/(1+odd)
c(logit=logit,odd=odd,pro=prob)

##预测概率 predicted probability(training)
pred=predict(glm1,type='response')
hist(pred)
abline(v=0.5,col='red')

##1.3 混淆矩阵和模型判断 ####
confmx=table(Acture=accepts$bad_ind, Predict=pred>0.5)
confmx

confmx=table(Acture=accepts$bad_ind, Predict=pred>0.4)
confmx

##模型准确性指标（accuracy metrics(training)）k代表小数点后几位，as numeric是做成一个具体的数
accuracymetrices=function(x,k=3) c(
  accuracy=sum(diag(x)/sum(x)),
  sensitivity=as.numeric(x[2,2]/rowSums(x)[2]),
  specificity=as.numeric(x[1,1]/rowSums(x)[1])
)%>% round(k)

accuracymetrices(confmx)

accuracymetrices2=function(x) c(
  accuracy=sum(diag(x)/sum(x)),
  sensitivity=as.numeric(x[2,2]/rowSums(x)[2]),
  specificity=as.numeric(x[1,1]/rowSums(x)[1])
)

accuracymetrices2(confmx)

## confusion matrix (testing)
pred2=predict(glm1,type='response',newdata=test)
confmx2=table(test$bad_ind,predict=pred2>0.5)
confmx2
sapply(list(Train=confmx,test=confmx2),accuracymetrices)


##1.4 第二次尝试####

lg=glm(bad_ind ~fico_score+bankruptcy_ind+tot_derog+age_oldest_tr+rev_util + 
            ltv+ veh_mileage,family=binomial(link='logit'))
summary(lg)

#画出第二次的ROC曲线

library(pROC)
attach(accepts)
p=predict(lg,train)
plot.roc(bad_ind~p,train,col="1")->r1
rocobjtr= roc(train$bad_ind, train$p);rocobjtr
legend(0.7,0.5,paste("Mode_Train-AUC:",round(auc(rocobjtr),3),sep=""))

p2=predict(lg,test)
plot.roc(bad_ind~p2,test,add=TRUE,col='2')->r2
rocobjte =roc(test$bad_ind, test$p);rocobjte
legend(0.7,0.3,paste("Mode_Test-AUC:",round(auc(rocobjte),3),sep=""))
##两条曲线是否存在差别
roc.test(r1,r2)

##1.5第三次尝试：逐步回归####

lg_ms=step(lg,direction = "both")
summary(lg_ms)


#2模型优化####

##2.1 删除veh_mileage####
lg=glm(bad_ind ~fico_score+bankruptcy_ind+age_oldest_tr+rev_util+ltv+tot_derog,
       family=binomial(link='logit'))
lg
summary(lg)

##2.2 删除tot_derog ####

lg=glm(bad_ind ~fico_score+bankruptcy_ind+age_oldest_tr+rev_util+ltv,
       family=binomial(link='logit'))
lg
summary(lg)
##通过方差膨胀系数VIF值判断多重共线性，该值需要小于10
library(car)
vif(lg)

####小练习，画出第三次的ROC曲线 ####

#3 绘制ROC-ROCR####

##3.1原理####
library(ggplot2)
library(ROCR) 
data(ROCR.simple) 
View(ROCR.simple)
ROCR.simple[["predictions"]]   #view，看图中数据：图中最右边的箭头点击
ROCR.simple[["labels"]]

pred = prediction(ROCR.simple$predictions, ROCR.simple$labels)  
perf = performance(pred,"tpr","fpr") #计算tpr,fpr
perf
fpr <- unlist(perf@x.values)    #把x.values提取出来,就是fpr
tpr <- unlist(perf@y.values)    #把y.values提取出来,就是tpr
plotdata <- data.frame(fpr,tpr) ;plotdata   #必须做成data.frame
names(plotdata) <- c("fpr", "tpr")
g <- ggplot(plotdata,aes(x = fpr, y = tpr, colour = fpr))+
  geom_point()+
  labs(x = "False positive rate", y = "True positive rate", title ="ROC Curves")
g 

#本例
test$p=predict(glm1,test)
pred_Te <- prediction(test$p, test$bad_ind)
perf_Te <- performance(pred_Te,"tpr","fpr")
perf_Te
fpr <- unlist(perf_Te@x.values)   #把x.values提取出来,就是fpr
tpr <- unlist(perf_Te@y.values)    #把y.values提取出来,就是tpr
plotdata <- data.frame(fpr,tpr) ;plotdata   #必须做成data.frame
names(plotdata) <- c("fpr", "tpr")
g <- ggplot(data=plotdata,aes(x = fpr, y = tpr, colour = fpr), size=1)+ 
  geom_point()+
  labs(x = "False positive rate", y = "True positive rate", title ="ROC Curves")
g

#把test和train的ROC放在一起
library(ROCR)
pred_Te <- prediction(test$p, test$bad_ind)
perf_Te <- performance(pred_Te,"tpr","fpr")

pred_Tr <- prediction(train$p, train$bad_ind)
perf_Tr <- performance(pred_Tr,"tpr","fpr")
plot(perf_Te, col='blue',lty=1);
plot(perf_Tr, col='black',lty=2,add=TRUE);
abline(0,1,lty=2,col='red')

##3.2绘制ROC-pROC####
##优点：将多条ROC曲线放在同一张图中
library(pROC)
data(aSAH)
View(aSAH)
roc1<-roc(aSAH$outcome,aSAH$age)
roc2<-roc(aSAH$outcome,aSAH$s100b)
plot(roc1,col='blue')
plot.roc(roc2,add=TRUE,col='red')  # plot(roc2,add=TRUE,col='red') it is ok 

library(pROC)
attach(accepts)
p=predict(glm1,train)
plot.roc(bad_ind~p,train,col="1")->r1
rocobjtr= roc(train$bad_ind, train$p);rocobjtr
legend(0.7,0.5,paste("Mode_Train-AUC:",round(auc(rocobjtr),3),sep=""))
p2=predict(glm1,test)
plot.roc(bad_ind~p2,test,add=TRUE,col='2')->r2
rocobjte =roc(test$bad_ind, test$p);rocobjte
legend(0.7,0.3,paste("Mode_Test-AUC:",round(auc(rocobjte),3),sep=""))
##两条曲线是否存在差别
roc.test(r1,r2)

##3.3 caTools画ROC####
par(mfrow=c(1,2),cex=0.8)
pred1=predict(glm1,train)
trAUC=caTools::colAUC(pred1,y=train$bad_ind,plotROC=T)

#caTools::colAUC(x,y,plotROC=T) x是预测值,y 实际值
pred2=predict(glm1,test)
tsAUC=caTools::colAUC(pred2,y=test$bad_ind,plotROC=T)


#4选做：课堂练习####

library(ggplot2)
library(cowplot)
## NOTE: The data used in this demo comes from 
## http://archive.ics.uci.edu/ml/index.php
## Specifically, this is the heart disease data set.
## http://archive.ics.uci.edu/ml/datasets/Heart+Disease

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

data <- read.csv(url, header=FALSE)

head(data) # you see data, but no column names

colnames(data) <- c(
  "age",
  "sex",# 0 = female, 1 = male
  "cp", # chest pain胸痛
  # 1 = typical angina,典型心绞痛
  # 2 = atypical angina,非典型心绞痛
  # 3 = non-anginal pain,非心绞痛性疼痛
  # 4 = asymptomatic无症状
  "trestbps", # resting blood pressure (in mm Hg)静息血压
  "chol", # serum cholestoral in mg/dl 血清胆固醇（mg/dl）
  "fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE空腹血糖低于120 mg/dl
  "restecg", # resting electrocardiographic results静息心电图结果
  # 1 = normal
  # 2 = having ST-T wave abnormality有ST-T波异常
  # 3 = showing probable or definite left ventricular hypertrophy显示可能的或明确的左心室肥大
  "thalach", # maximum heart rate achieved达到最大心率
  "exang",   # exercise induced angina, 1 = yes, 0 = no运动性心绞痛，1=是，0=否
  "oldpeak", # ST depression induced by exercise relative to rest运动相对休息诱发的ST段压低
  "slope", # the slope of the peak exercise ST segment山顶段的坡度
  # 1 = upsloping
  # 2 = flat
  # 3 = downsloping
  "ca", # number of major vessels (0-3) colored by fluoroscopy荧光透视着色的主要血管数量（0-3）
  "thal", # this is short of thalium heart scan 短铊心脏扫描
  # 3 = normal (no cold spots)
  # 6 = fixed defect (cold spots during rest and exercise)
  # 7 = reversible defect (when cold spots only appear during exercise)
  "hd" # (the predicted attribute) - diagnosis of heart disease（预测属性）-心脏病诊断
  # 0 if less than or equal to 50% diameter narrowing
  # 1 if greater than 50% diameter narrowing
)

head(data) # now we have data and column names

str(data) # this shows that we need to tell R which columns contain factors
# it also shows us that there are some missing values. There are "?"s
# in the dataset. These are in the "ca" and "thal" columns...

## First, convert "?"s to NAs...
data[data == "?"] <- NA

## Now add factors for variables that are factors and clean up the factors
## that had missing data...
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)

data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

data$ca <- as.integer(data$ca) # since this column had "?"s in it
# R thinks that the levels for the factor are strings, but
# we know they are integers, so first convert the strings to integers...
data$ca <- as.factor(data$ca)  # ...then convert the integers to factor levels

data$thal <- as.integer(data$thal) # "thal" also had "?"s in it.
data$thal <- as.factor(data$thal)

## This next line replaces 0 and 1 with "Healthy" and "Unhealthy"
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd) # Now convert to a factor

str(data) ## this shows that the correct columns are factors

## Now determine how many rows have "NA" (aka "Missing data"). If it's just
## a few, we can remove them from the dataset, otherwise we should consider
## imputing the values with a Random Forest or some other imputation method.
nrow(data[is.na(data$ca) | is.na(data$thal),])
data[is.na(data$ca) | is.na(data$thal),]
## so 6 of the 303 rows of data have missing values. This isn't a large
## percentage (2%), so we can just remove them from the dataset
## NOTE: This is different from when we did machine learning with
## Random Forests. When we did that, we imputed values.
nrow(data)
data <- data[!(is.na(data$ca) | is.na(data$thal)),] #去掉na的数据
nrow(data)

##
## Now we can do some quality control by making sure all of the factor
## levels are represented by people with and without heart disease (hd)
##
## NOTE: We also want to exclude variables that only have 1 or 2 samples in
## a category since +/- one or two samples can have a large effect on the
## odds/log(odds)
##descriptive analysis

xtabs(~ hd + sex, data=data)
xtabs(~ hd + cp, data=data)
xtabs(~ hd + fbs, data=data)
xtabs(~ hd + restecg, data=data)
xtabs(~ hd + exang, data=data)
xtabs(~ hd + slope, data=data)
xtabs(~ hd + ca, data=data)
xtabs(~ hd + thal, data=data)

##
## Now we are ready for some logistic regression. First we'll create a very
## simple model that uses sex to predict heart disease
##

## let's start super simple and see if sex (female/male) is a good
## predictor...
## First, let's just look at the raw data...
xtabs(~ hd + sex, data=data)
#           sex
# hd         F   M
# Healthy    71  89
# Unhealthy  25 112
## Most of the females are healthy and most of the males are unhealthy.
## Being female is likely to decrease the odds in being unhealthy.
##    In other words, if a sample is female, the odds are against it that it
##    will be unhealthy
## Being male is likely to increase the odds in being unhealthy...
##    In other words, if a sample is male, the odds are for it being unhealthy


## Now do the actual logistic regression


logistic <- glm(hd ~ sex, data=data, family="binomial")
summary(logistic)
## 数据解释
## (Intercept)  -1.0438     0.2326  -4.488 7.18e-06 ***
##   sexM        1.2737     0.2725   4.674 2.95e-06 ***

## Let's start by going through the first coefficient...
## (Intercept)  -1.0438     0.2326  -4.488 7.18e-06 ***
##
## The intercept is the log(odds) a female will be unhealthy. This is because
## female is the first factor in "sex" (the factors are ordered,
## alphabetically by default,"female", "male")
head(data)
xtabs(~ sex +hd, data=data)
table(data$sex,data$hd)

female.log.odds <- log(25 / 71)
female.log.odds

## Now let's look at the second coefficient...
##   sexM        1.2737     0.2725   4.674 2.95e-06 ***
##
## sexM is the log(odds ratio) that tells us that if a sample has sex=M, the
## odds of being unhealthy are, on a log scale, 1.27 times greater than if
## a sample has sex=F.
male.log.odds.ratio <- log((112 / 89) / (25/71))
male.log.odds.ratio

## Now calculate the overall "Pseudo R-squared" and its p-value

## NOTE: Since we are doing logistic regression...
## Null devaince = 2*(0 - LogLikelihood(null model))
##               = -2*LogLikihood(null model)
## Residual deviacne = 2*(0 - LogLikelihood(proposed model))
##                   = -2*LogLikelihood(proposed model)
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

## chi-square value = 2*(LL(Proposed) - LL(Null))
## p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2*(ll.proposed - ll.null), df=1)
1 - pchisq((logistic$null.deviance - logistic$deviance), df=1)

## Lastly, let's  see what this logistic regression predicts, given
## that a patient is either female or male (and no other data about them).
predicted.data <- data.frame(
  probability.of.hd=logistic$fitted.values,
  sex=data$sex)
predicted.data

predicted.data$p=predict(logistic,data)

## We can plot the data...
library(ggplot2)
ggplot(data=predicted.data, aes(x=sex, y=probability.of.hd)) +
  geom_point(aes(color=sex), size=5) +
  xlab("Sex") +
  ylab("Predicted probability of getting heart disease")

## Since there are only two probabilities (one for females and one for males),
## we can use a table to summarize the predicted probabilities.
xtabs(~ probability.of.hd + sex, data=predicted.data)


## Now we will use all of the data available to predict heart disease


logistic <- glm(hd ~ ., data=data, family="binomial")
summary(logistic)

## Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

## The p-value for the R^2
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))

## now we can plot the data
predicted.data <- data.frame(
  probability.of.hd=logistic$fitted.values,
  hd=data$hd)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.hd, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
ggplot(data=predicted.data, aes(x=rank, y=probability.of.hd)) +
  geom_point(aes(color=hd), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting heart disease")

ggsave("heart_disease_probabilities.pdf")

#5 作业####
##数据：数据：teleco数据
#解释变量：客户是否流失(churn)
#步骤：
#1、对数据作清洗，训练集70%；
#2、对训练集作glm；
#3、计算测试集的混淆矩阵；
#4、尝试两个模型，基于两种模型，画出test数据集的ROC曲线，并对模型进行解释。

