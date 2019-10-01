##### matching the place of starbucks #####

library(class)
dong = read.csv(file.choose(),header=T) # code matching
sb = read.csv(file.choose(),header=T) # sbsb
View(dong); View(sb)
dong = dong[,c(1,2,9,10)]
sb = sb[,c(1,2,9,10)]
names(dong); names(sb)
code = factor(dong[,1])
name = factor(dong[,2])
dong.code = knn1(dong[,3:4],sb[,3:4],code)
dong.name = knn1(dong[,3:4],sb[,3:4],name)
dong.code
dong.name
sb$dong.name = dong.name; sb$dong.code = dong.code
View(sb)
write.csv(sb,"c:/Users/JJ/Desktop/starbucks_dong.csv",row.names=F)

##### handling NA #####

raw.dat = read.csv(file.choose(),header=T)
View(raw.dat)
nrow(raw.dat)

library(Amelia)
missmap(raw.dat)

library("DMwR")
new = knnImputation(raw.dat,k=5)
View(new)

##### logistic reg. #####

raw.dat = read.csv(file.choose(),header=T)
names(raw.dat)

candidate = raw.dat[which(raw.dat$candidate==1),]
data = raw.dat[which(raw.dat$candidate==0),-c(1:3)] 
names(data)
data$starbucks = ifelse(data$starbucks!=0,1,0)
set.seed(0)
index = sample(1:nrow(data),nrow(data)*0.7)
train = data[index,]
test = data[-index,]

null = glm(starbucks~1,data=train,family="binomial")
full = glm(starbucks~.-lat-lon,data=train,family="binomial")
step(null,scope=list(lower=null,upper=full),direction="both")

fit = glm(formula = starbucks ~ drink.sales + cloth.num + theater + 
            apt.area + fwork + move50 + market + bank + high + mmove + 
            drink.num + drink.month, family = "binomial", data = train)
summary(fit)
library(lmtest)
lrtest(fit)
dwtest(fit)

pred.y = predict(fit,newdata = test,type="response"); plot(pred.y)

# roc curve

library(ROCR)
pred = prediction(pred.y,test$starbucks)
roc = performance(pred,measure='tpr',x.measure='fpr')
plot(roc,col='red')
legend('bottomright',c('base','logistic'),col=1:2,lty=2:1)
abline(a=0,b=1,lty='dotted')

auc = performance(pred,measure='auc')
auc = auc@y.values[[1]]
auc

sb.hat = ifelse(pred.y>=0.21,1,0)
table(sb.hat,test$starbucks)
sum(sb.hat==test$starbucks)/nrow(test)
table(sb.hat,test$starbucks)[2,2]/sum(test$starbucks)

final.sb = predict(fit,newdata=candidate,type="response")
final.sb = ifelse(final.sb>=0.9,1,0)
glm.result = candidate[which(final.sb==1&candidate$starbucks==0),"road.name"]
glm.result

##### support vector machine #####

library(e1071)

svm.model = svm(factor(starbucks)~.-lat-lon,data=train,kernel="linear",class.weights=c("0"=0.25))
summary(svm.model)

pred.y = predict(svm.model,newdata=test)
ct = table(pred.y,test$starbucks); ct
sum(diag(ct))/sum(ct)
sum(diag(ct)[2])/sum(ct[,2])

set.seed(0)
library(caret)
library(mlbench)

control = rfeControl(rfFuncs,method="cv",number=5)
results = rfe(train[,2:44],factor(train[,1]),sizes=1:43,rfeControl=control)
predictors(results) 
plot(results,type=c("g","o"))

sel.svm.model = svm(factor(starbucks)~market+move20+mwork+fwork+drink.sales+bank+
                      move30+theater,data=train,kernel="linear",class.weights=c("0"=0.25))
summary(sel.svm.model)

pred.y = predict(sel.svm.model,newdata=test)
ct = table(pred.y,test$starbucks); ct
sum(diag(ct))/sum(ct)
sum(diag(ct)[2])/sum(ct[,2])
pred = prediction(as.integer(pred.y)-1,test$starbucks)
auc = performance(pred,measure='auc')
auc = auc@y.values[[1]]
auc

pred.y = predict(svm.model,newdata=test)
pred = prediction(as.integer(pred.y)-1,test$starbucks)
auc = performance(pred,measure='auc')
auc = auc@y.values[[1]]
auc

final.sb = predict(svm.model,newdata=candidate)
svm.result = candidate[which(final.sb==1&candidate$starbucks==0),"road.name"]
svm.result

##### GAM #####

sb.dist = as.matrix(dist(cbind(raw.dat$lon,raw.dat$lat)))
sb.dist.inv = 1/sb.dist
diag(sb.dist.inv) = 0
sb.dist.inv[1:3,1:3]
library(ape)
Moran.I(raw.dat$starbucks,sb.dist.inv)

library(gam)
library(mgcv)

# full

names(train)
gam.full = gam(starbucks ~ drink.sales + cloth.num + theater + 
                 apt.area + fwork + move50 + market + bank + high + mmove + 
                 drink.num + drink.month + s(lat,lon)+
                 s(mlive10,bs="cr")+s(mlive20,bs="cr")+s(mlive30,bs="cr")+
                 s(mlive40,bs="cr")+s(mlive50,bs="cr")+s(mlive60,bs="cr")+
                 s(flive10,bs="cr")+s(flive20,bs="cr")+s(flive30,bs="cr")+
                 s(flive40,bs="cr")+s(flive50,bs="cr")+s(flive60,bs="cr")+
                 s(cos.month,bs="cr")+s(cos.sales,bs="cr")+s(cos.num,bs="cr")+
                 s(cloth.month,bs="cr")+s(cloth.sales,bs="cr")+s(apt.num,bs="cr")+
                 s(apt.price,bs="cr")+s(fmove,bs="cr")+s(move10,bs="cr")+
                 s(move20,bs="cr")+s(move30,bs="cr")+s(move40,bs="cr")+s(move60,bs="cr")+
                 s(mwork,bs="cr")+s(govern,bs="cr")+s(market,bs="cr")+s(subway,bs="cr")+
                 s(bus,bs="cr"),select=T,data=train,method="REML",family="binomial")
summary(gam.full)
plot(gam.full,se=T,pages=5,shade=T)

pred.y = predict.gam(gam.full,newdata=test,type="response")
par(mfrow=c(1,1))
plot(pred.y)

pred = prediction(as.vector(pred.y),test$starbucks)
roc = performance(pred,measure='tpr',x.measure='fpr')
plot(roc,col='red')
legend('bottomright',c('base','logistic'),col=1:2,lty=2:1)
abline(a=0,b=1,lty='dotted')

auc = performance(pred,measure='auc')
auc = auc@y.values[[1]]
auc

sb.hat = ifelse(pred.y>=0.21,1,0)
table(sb.hat,test$starbucks)
sum(sb.hat==test$starbucks)/nrow(test)
table(sb.hat,test$starbucks)[2,2]/sum(test$starbucks)

# significant var.

gam.sel = gam(starbucks ~ drink.sales + cloth.num + theater + apt.area + 
                fwork + move50 + market + bank + high + mmove + drink.num +
                drink.month + s(lat,lon)+
                s(flive20)+s(cloth.sales)+s(fmove)+s(move20)+s(move40)+
                s(govern)+s(subway),method="REML",select=T,data=train,family="binomial")
summary(gam.sel)
plot(gam.sel,se=T,shade=T)

pred.y = predict.gam(gam.sel,newdata=test,type="response")
par(mfrow=c(1,1))
plot(pred.y)

pred = prediction(as.vector(pred.y),test$starbucks)
roc = performance(pred,measure='tpr',x.measure='fpr')
plot(roc,col='red')
legend('bottomright',c('base','logistic'),col=1:2,lty=2:1)
abline(a=0,b=1,lty='dotted')

auc = performance(pred,measure='auc')
auc = auc@y.values[[1]]
auc

sb.hat = ifelse(pred.y>=0.21,1,0)
table(sb.hat,test$starbucks)
sum(sb.hat==test$starbucks)/nrow(test)
table(sb.hat,test$starbucks)[2,2]/sum(test$starbucks)

final.sb = predict(gam.sel,newdata=candidate,type="response")
final.sb = ifelse(final.sb>=0.9,1,0)
gam.result = candidate[which(final.sb==1&candidate$starbucks==0),"road.name"]
gam.result

##### final CV error checking #####

library(cvTools)

n = nrow(data)
K = 5

cvf = cvFolds(n, K)

library(MASS)

# glm

cvstat = numeric(K)
for (k in 1:K)
{
  index = cvf$subsets[cvf$which == k]
  tr = data[-index,]
  te = data[index,]
  fit = glm(formula = starbucks ~ drink.sales + cloth.num + theater + 
              apt.area + fwork + move50 + market + bank + high + mmove + 
              drink.num + drink.month, family = "binomial", data = tr)
  pred = predict(fit, newdata = te, type='response')
  yhat = ifelse(pred>=0.21,1,0)
  cvstat[k] = 1 - sum(te$starbucks == yhat)/nrow(te)
}
mean(cvstat)

# gam

cvstat = numeric(K)
for (k in 1:K)
{
  index = cvf$subsets[cvf$which == k]
  tr = data[-index,]
  te = data[index,]
  gam.sel = gam(starbucks ~ drink.sales + cloth.num + theater + apt.area + 
                  fwork + move50 + market + bank + high + mmove + drink.num +
                  drink.month + s(lat,lon)+
                  s(flive20)+s(cloth.sales)+s(fmove)+s(move20)+s(move40)+
                  s(govern)+s(subway),method="REML",select=T,data=tr,family="binomial")
  pred = predict(fit, newdata = te, type='response')
  yhat = ifelse(pred>=0.21,1,0)
  cvstat[k] = 1 - sum(te$starbucks == yhat)/nrow(te)
}
mean(cvstat)