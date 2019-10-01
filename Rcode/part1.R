# 스타벅스 위치
library(ggplot2)
library(ggmap)
starbucks<-read.csv(file.choose(),header=T,stringsAsFactors = F)
seoul <- get_map("Seoul, South Korea", zoom=11,maptype = "roadmap") #???????? ????
map<-ggmap(seoul) #?????? ǥ?? 

lonlan<-mutate_geocode(starbucks,Street.Address,source="google") #��???浵 ???? 
map+geom_point(data=starbucks,aes(x=lon,y=lat),color="red",alpha=0.7,size=3)+
  geom_text(aes(lon,lat,label=road.name),data=starbucks,color="black") #??�� ?ĺ??? ��????


##### regression #####

raw.dat = read.csv(file.choose(),header=T) # 1????_?м???
View(raw.dat)
names(raw.dat)

data = raw.dat[,-c(1:2,4:6)]
names(data)

library(HH)
sum(vif(data[,-1])>10)
library(corrplot)
corrplot(cor(data[,-1]),order="hclust")

#linear regression
null = lm(starbucks~1,data=data)
full = lm(starbucks~.,data=data)
step(null,scope=list(lower=null,upper=full),direction="both")

fit = lm(formula = starbucks ~ fjob + flive30 + flive10 + mlive30 + 
           apt.area + move20 + bank + start.index + move30 + mmove + 
           drink.month + cloth.month + cloth.num, data = data)
summary(fit)

resid = fit$residuals
shapiro.test(resid)
library(lmtest)
bptest(fit)
dwtest(fit)  #inpendence satisfied
 
##### poisson reg. AIC #####

null = glm(starbucks~1,data=data,family="poisson",offset = raw.dat$area)
full = glm(starbucks~.,data=data,family="poisson",offset = raw.dat$area)
step(null,scope=list(lower=null,upper=full),direction="both") #default AIC

aic.fit = glm(formula = starbucks ~ move20 + flive40 + bus + cloth.month + 
                market + apt.price + cloth.sales + fjob + govern + mjob + 
                subway + mlive10 + mlive40 + mlive30 + flive30 + drink.month + 
                mlive60 + mlive50 + apt.num + cos.num + high + drink.num + 
                uni + flive20 + mlive20 + theater, family = "poisson", data = data, 
              offset = raw.dat$area)
summary(aic.fit)
sort(names(coef(aic.fit))[-1])

##### poisson reg. BIC #####

step(null,scope=list(lower=null,upper=aic.fit),direction="both",k=log(nrow(data)))
#including k: considering with BIC

bic.fit = glm(formula = starbucks ~ move20 + mlive10 + flive40 + bus + 
                mlive40 + subway + market + fjob + cloth.sales + mlive30 + 
                cos.num + flive30 + govern + apt.num + drink.month + apt.price + 
                cloth.month + mlive60 + drink.num + high + mlive50, family = "poisson", 
              data = data, offset = raw.dat$area)

summary(bic.fit)
dwtest(bic.fit)
library(AER)
dispersiontest(bic.fit,alternative="two.sided")

sort(names(coef(bic.fit))[-1])

##### prediciting candidate dong #####

pred.y = predict(bic.fit,type="response")
sum(pred.y>=data$starbucks+1)
candidate = raw.dat[which(pred.y>=data$starbucks+1),]; View(candidate)

library(randomForest)
set.seed(10)

rf.fit = randomForest(data[,sort(names(coef(bic.fit))[-1])],data$starbucks,ntree=200,mtry=5)
plot(rf.fit)
varImpPlot(rf.fit)
sum(rf.fit$predicted>=data$starbucks+1)
candidate = raw.dat[which(rf.fit$predicted>=data$starbucks+1),]; View(candidate)

library(ggplot2)
library(ggmap)
starbucks<-read.csv(file.choose(),header=F,stringsAsFactors = F)
seoul <- get_map("Seoul, South Korea", zoom=11,maptype = "roadmap") #???????? ????
map+geom_point(data=candidate,aes(x=long,y=lat),size=3,color="red",alpha=0.7)
