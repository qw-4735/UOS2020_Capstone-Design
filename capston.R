install.packages("dplyr")
library("dplyr")
install.packages("ggplot2")
library('ggplot2')
dff<-read.csv('C:/Users/rstudioid/Desktop/BUS_STATION_BOARDING_MONTH_201912_1.csv')
head(dff)
join<-read.csv('C:/Users/rstudioid/Desktop/캡스톤 실습/대여소_결합(0716).csv')
head(join)
str(join)
table(is.na(join))
sum(is.na(join))
colSums(is.na(join))
join_1<-join
join_1$유동10대미[is.na(join_1$유동10대미)]<-0
sum(is.na(join_1$유동10대미))
join_1$유동10대[is.na(join_1$유동10대)]<-0
sum(is.na(join_1$유동10대))
join_1$유동20대[is.na(join_1$유동20대)]<-0
sum(is.na(join_1$유동20대))
join_1$유동30대[is.na(join_1$유동30대)]<-0
sum(is.na(join_1$유동30대))
join_1$유동40대[is.na(join_1$유동40대)]<-0
sum(is.na(join_1$유동40대))
join_1$유동50대[is.na(join_1$유동50대)]<-0
sum(is.na(join_1$유동50대))
join_1$유동60대[is.na(join_1$유동60대)]<-0
sum(is.na(join_1$유동60대))
join_1$유동70대이[is.na(join_1$유동70대이)]<-0
sum(is.na(join_1$유동70대))

plot(join$대여~join$반납)
cor(join$대여,join$반납)
join_1$amount=(join$대여+join$반납)
head(join_1)
str(join_1)

boxplot(join_1[,c(11,13)])

attach(join)
is.na(join$버스_승객)
bus_top20<-dff %>% group_by(노선번호) %>% summarise(승차총수=sum(top),하차총수=sum(hach))%>% arrange(desc(승차총수))%>% slice(1:20)
bus_person20<-join %>% select(버스_승객) %>%   arrange(desc(버스_승객))%>% slice(1:20)
subway_person20<-join %>% select(지하철_승) %>% arrange(desc(지하철_승))%>% slice(1:20)
plot(bus_person20,subway_person20)
cor(bus_person20,subway_person20)
cor(join$버스_승객,join$지하철_승)
a<-join$버스_승객
b<-join$지하철_승
cor(a,b)
plot(a~b)

flow_people<-join[,43:48]
live_people<-join[,33:38]

people<-join[,c(33,34,35,36,37,38,43,44,45,46,47,48)]
str(people)
people<-na.omit(people)
cor(people)

flow_people<-na.omit(flow_people)
cor(flow_people,live_people)
is.na(people)
pca<-prcomp(people, center = T, scale. = T)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(people, histogram=TRUE, pch=19)

p.cor<-cor(people)
corrplot(p.cor, method="number")
corrplot::corrplot.mixed(p.cor,lower="number",upper="ellipse",lower.col = "black",number.cex=0.5,p.mat = res1$p, insig = "p-value")
?corrplot.mixed
?corrplot
res1 <- cor.mtest(people, conf.level = .95)

install.packages("corrplot")
library("corrplot")
corrplot(p.cor,type="upper",method="ellipse")
corrplot(p.cor,type="lower", method="number",col="black")

join_2<-join_1[,c(11,13,14,15,16:38,42:50)] #전체 설명변수
str(join_2)
j.cor<-cor(join_2) #전체 설명변수 correlation -> 인구끼리 제외 딱히 높은 상관계수 존재하지 않음

set.seed(12345)   #rmse(10 fold cv)
train_control <- trainControl(method = "repeatedcv",   
                              number = 10, repeats = 3)  
model <- train(amount ~., data = join_2,   
               method = "lm",  
               trControl = train_control)
print(model)
model4<-train(amount~.,data=join_2,  method = 'leapBackward', trControl=train_control)
print(model4)
model3<-train(amount ~ 버스_승객 + 버스_경유+ 지하철_승 + 면적_아파 + 
                면적_주거 + 거리_지하 + 거리_자전 + 거리_문화 + 거리_영화 + 
                거리_관광 + 거리_대학 + 거리_초중 + 거리_의료 + 거리_공원 + 
                거리_특화 + 거리_교통 + 거리_하천 + 평균_경사 + 주거10대 + 
                주거20대 + 주거40대 + 주거50대 + near + medium + 
                유동10 + 유동20대 + 유동30대 + 유동40대 + 유동50대 + 유동60대 + 유동70대이,
              data = join_2, method="lm",trControl= train_control)
print(model3)
model2<-train(amount ~., data=join_2, method="rf", trControl=train_control)
print(model2)
model9<-train(amount~.,data=join_2,method="rf",trControl=train_control,mtry=15)
print(model9)
model5<-train(amount~.^2,data=join_2,method = "lm",trControl = train_control)
print(model5)
reg<-lm(amount ~ 버스_승객 + 버스_경유+ 지하철_승 + 면적_아파 + 
          면적_주거 + 거리_지하 + 거리_자전 + 거리_문화 + 거리_영화 + 
          거리_관광 + 거리_대학 + 거리_초중 + 거리_의료 + 거리_공원 + 
          거리_특화 + 거리_교통 + 거리_하천 + 평균_경사 + 주거10대 + 
          주거20대 + 주거40대 + 주거50대 + near + medium + 
          유동10 + 유동20대 + 유동30대 + 유동40대 + 유동50대 + 유동60대 + 유동70대이,data=join_2)
summary(reg)
multi.reg<-lm(amount~.^2,data=join_2)
summary(multi.reg)
vif(multi.reg)
multi.reg.step<-step(multi.reg,direction="backward")
summary(multi.reg.step)
vif(reg.step)
reg.step<-step(reg, direction = "both")
summary(reg.step)
str(join_2)

join.log<-within(join_2, logy<-log(join_2$amount))
join.log<-join.log[,c(-40)]
str(join.log)
reg.log<-lm(logy~., data=join.log)
summary(reg.log)
model7<-train(logy~.,data=join.log,method = "lm",trControl = train_control)
print(model7)
reg$formula
join_3<-join_1[,c(11:50)] 
str(join_3)
zjoin_3<-as.data.frame(scale(join_3))
str(zjoin_3)
l2<-lm(join_3$amount~. , data=join_3) 
summary(l2)
vif(l2)
set.seed(12345)
lt<-lm(train1$amount~., data=train1)
summary(lt)
vif(lt)yhat_lt<-predict(fit.train, test1)
yhat_lt<-predict(lt, test1)
mean((test1$amount-yhat_lt)^2)
fit.train<-step(lt, direction = "both") 
summary(fit.train)
vif(fit.train)


yhat_lt<-predict(fit.m, train1)
mean((test1$amount-yhat_lt)^2)

join.log<-within(join_2, logy<-log(join_2$amount))
join.log<-join.log[,c(-40)]
str(join.log)
reg.log<-lm(logy~., data=join.log)
summary(reg.log)
vif(train.log.model)
train.log.model.select<-step(train.log.model, direction = "both") 
summary(train.log.model.select)
vif(train.log.model.select)
result1<-predict(fit.ddd, test1)
result2<-predict(train.log.model.select, test)
test4<-test[,-49]
head(test)
str(test)
sum(is.na(result2))
str(test1)
test.log<-within(test1, logy<-log(test1$amount))
mean((test.log$logy-result1)^2)


vif(fit.dd)
summary(fit.dd)
install.packages("leaps") 
library("leaps")
require(leaps)
leaps<-regsubsets(join_3$amount~. , data=join_3, nbest=1)
plot(leaps, scale="adjr2")

fit.both<-step(l2, direction = "both") 
summary(fit.both)
vif(fit.both)

test<-read.csv('C:/Users/rstudioid/Desktop/캡스톤 실습/대여소후보_결합(0717)(1).csv')
str(test)
result<-predict(fit.both, test)
str(result)
result<-as.vector(result)
str(result)
plot(fit.both)
return()
durbinWatsonTest(fit.both) 
crPlots(fit.both) 
return()
ncvTest(fit.both) 
spreadLevelPlot(fit.both)
summary(car::powerTransform(join_3$amount)) 
join_3<-within(join_3, logy<-log(join_3$amount))
join_3<-join_3[,c(-40)]
fit.t<-lm(logy~., data=join_3)
str(join_3)
summary(fit.t)
plot(fit.t)
par(mfrow=c(1,1))
return()
fit.both.t<-step(fit.t, direction = "both")
summary(fit.both.t)
vif(fit.t)
leaps.t<-regsubsets(logy~. , data=join_3, nbest=1)
plot(leaps.t, scale="adjr2")
fit.d<-lm(logy ~ 버스_승객 + 버스_경유+ 지하철_승 + 면적_아파 + 
            면적_주거 + 거리_지하 + 거리_자전 + 거리_문화 + 거리_영화 + 
            거리_관광 + 거리_대학 + 거리_초중 + 거리_의료 + 거리_공원 + 
            거리_특화 + 거리_교통 + 거리_하천 + 평균_경사 + 
            주거20대 + 주거50대 + near, data = join_3)
summary(fit.d)
vif(fit.d)
par(mfrow=c(1,1)) 
plot(fit.d)
return()
install.packages("gvlma")
library(gvlma)
gvmodel<-gvlma(fit.d)
summary(gvmodel)
durbinWatsonTest(fit.d)
crPlots(fit.d) 
return()
ncvTest(fit.d) 
fit.backward.t<-step(fit.t, direction = "backward") 
summary(fit.backward.t)
str(fit.backward.t)
vif(fit.backward.t)
test<-read.csv('C:/Users/rstudioid/Desktop/캡스톤 실습/대여소후보_결합(0717)(1).csv')

result<-predict(fit.d, newdata= test)
?predict
as.data.frame(fit.d)
str(result)
test$result<-result
str(test)
test<-test[,c(9:49)]
write.csv(test, "C:/Users/rstudioid/Desktop/캡스톤 실습/test1.csv")
result<-as.vector(result)

test2<-within(test, logy<-log(test$result))
str(test2)
if(test2$logy<0) logy<-0
ifelse(test2$logy<0, 0, test2$logy)
test2$logy


install.packages("boot")
library(boot)
model1 <- glm(fit.d)
cv.err=cv.glm(data=test, glmfit=model1,K=10)
round(cv.err$delta,2)

set.seed(12345)
cv.error.10=rep(0,10)
for(i in 1:10){
  model1<-glm(fit.d)
  cv.error.10[i]=cv.glm(join_3,model1, K=10)$delta[1]
}
cv.error.10

install.packages("forecast")
library(forecast)
accuracy(fit.d)

relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}
fit<-fit.d
relweights(fit, col="blue")
mean()
leaps<-regsubsets(logy~. , data=join_3, nbest=1)
plot(leaps, scale="adjr2")

return()
fit.forward<-step(l2, direction = "forward") 
summary(fit.forward)
vif(fit.forward)

fit.backward<-step(l2, direction = "backward") 
summary(fit.backward)
vif(fit.backward)

par(mfrow=c(1,1)) 
plot(fit.d)
return()


install.packages("randomForest")
library(randomForest)
library(caret)
set.seed(12345)
ntree<-c(200,300,500,700)
ntree<-c(500,500,500,500)
mtry<-c(17:20)
param<-data.frame(n=ntree, m=mtry)
param
for (i in param$n) {
  cat('ntree=',i, '\n')
  for(j in param$m){
    cat('mtry')
    rf<-randomForest(join_2$amount~.,data=join_2, ntree=i, mtry=j,
                     na.action = na.omit)
    print(rf)
  }
} 

#randomforest
set.seed(12345)
rf1=randomForest(join_2$amount~.,data=join_2,
                 mtry=20,importance=TRUE)
print(rf1)
yhat.rf= predict(rf1, newdata = test1)
mean((yhat.rf-test1$amount)^2) #randomforest mse
importance(rf1)
varImpPlot(rf1)
?varImpPlot
oob.pred<-predict(rf1, type="prob", OOB=TRUE)

rf2=randomForest(test1$amount~. , data=test1, 
                 mtry=13,ntree=500, importance=TRUE)
yhat.rf= predict(rf1, newdata = test1)
install.packages("ModelMetrics")
auc(test1$amount, yhat.rf)
rf1 %>% 
  predict(test1) %>% 
  bind_cols(test1) %>%
  metrics(truth=test1$amount, estimate=)
train1$amount.f<-as.factor(train1$amount)
str(train1$amount)
str(train1)
train1<-train1[,-42]

print(rf1)
plot(rf1)

yhat.rf= predict(rf1, newdata = test1)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
set.seed(12345)
mtry <- 14
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(join_2$amount~., data=join_2, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
rf_random <- train(amount ~ .,
                   data = join_2,
                   method = 'rf',
                   metric = 'Accuracy',
                   trControl = control)
print(rf_random)


class(yhat.rf)
class(test1$amount)
as.factor(yhat.rf)
confusionMatrix(factor(yhat.rf), factor(test1$amount))


t_index<-sample(1:nrow(join_2), size=nrow(join_2))
split_index<-split(t_index, 1:10)
str(split_index)
class(split_index)
split_index[[1]]
accuracy_3 <- c()        
for(i in 1:10){
  test <- join_2[split_index[[i]],]  
  train <- join_2[-split_index[[i]],]  
  
  set.seed(123)
  rf1=randomForest(train$amount~. , data=train, 
                   mtry=14,ntree=300, importance=TRUE)
  plot(rf1)
  importance(rf1)    
  varImpPlot(rf1)     
  
  test_pred <- predict(rf1, test)
  table <- table(real=test$amount, predict=test_pred)
  
  
  accuracy_3[i] <- sum(diag(table))/sum(table)
}
mean(accuracy_3)
mean((yhat.rf-test1$amount)^2)
str(train1)
str(join_2)
str(rf)
importance(rf1)
varImpPlot(rf1)
?varImpPlot
rf.predictions <- predict(rf1, test, type="class")
test$pred<-round(rf.predictions,3)
str(test)
write.csv(test, "C:/Users/rstudioid/Desktop/캡스톤실습/test.rf4.csv")
