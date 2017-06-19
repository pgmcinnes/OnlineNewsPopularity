data <- read.csv("OnlineNewsPopularity.csv")
library(ggplot2)
suppressWarnings(library(reshape2))
library(DAAG)
library(leaps)
library(glmnet)
library(ISLR)
library(class)
library(MASS)
data <- data.frame(data)

##~~~~~~~~~CLEAN THE DATA~~~~~~~~~~~##
plot(data$shares)
outliers <- (data[(data$shares > 400000),])
data<-data[!(data$url == "http://mashable.com/2013/07/03/low-cost-iphone/"),]
data <- data[!(data$url == "http://mashable.com/2013/11/12/roomba-880-review/"),]
data<- data[!(data$url == "http://mashable.com/2013/11/18/kanye-west-harvard-lecture/"),]
data<- data[!(data$url == "http://mashable.com/2014/04/09/first-100-gilt-soundcloud-stitchfix/"),]
attach(data)
data[,61] <- as.numeric(data[,61])
data<-data[,-1]
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~PLOT FREQUENCY OF EACH ATTRIBUTE~##
par(mfrow=c(3,4))
for(i in 1:length(data)){
  hist(data[,i],xlab=names(data[i]))
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~ADD BINARY AND SCALE DATA~~~~~##
shares01 <- rep(0, length(shares))
shares01[shares > median(shares)] <- 1
data1 <- data.frame(data, shares01)
data1[,1:12]<-scale(data1[,1:12])
data1[,19:30]<-scale(data1[,19:30])
data1[,39:61]<-scale(data1[,39:61])
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~~~~~~INITIAL GLM FIT~~~~~~~~~~##
firstfit <- glm(shares01~.,data=data1)
summary(firstfit)
par(mfrow=c(2,3))
plot(firstfit)
res <- residuals(firstfit)
for(i in 1:length(res)){
  res[i]<-res[i]^2
}
MSEfirstfit<-mean(res)
MSEfirstfit<-sum(summary(firstfit)$coefficients[,2])/length(summary(firstfit)$coefficients[,2])

##outlier in var "n_unique_tokens", "n_non_stop_unique_tokens" and "n_non_stop_words", which might be due to typing error.
##These are not significant predictors in a full model, so we will remove them. 
##non stop unique tokens intuitively should not be a significant predictor, so we removed it as well. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##REMOVE THE AFOREMENTIONED COLUMNS~##
data<-data[data[,4]<1,]
for(i in c(11,20,44,45,46,48,49,50,53)){
  data<-data[data[,i]!=0,]
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~REMOVE TAILS USING LOG FUNCTION~##
for(i in c(3,7,8,9,10,22,26:30,39:43,47,60)){
  if(!sum(data[,i]==0)){
    data[,i]<-log(data[,i]); names(data)[i] <- paste("log_",names(data)[i],sep="")
  }
  else{
    data[,i]<-sqrt(data[,i]); names(data)[i]<- paste("sqrt_", names(data)[i], sep="")  
  }
}
data <- data[, -c(19,21,23,25)]
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##LINEAR MODEL OF FULL DATA USING LOG##

full <- lm(log_shares~ ., data=data)
summary(full)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~SPLIT DATA BY ARTICLE TYPE~~~~~##

data$news_sub <- rep("lifestyle", nrow(data))
data$news_sub[data$data_channel_is_bus==1] <- "bus"
data$news_sub[data$data_channel_is_entertainment==1] <- "entertainment"
data$news_sub[data$data_channel_is_socmed==1] <- "socmed"
data$news_sub[data$data_channel_is_tech==1] <- "tech"
data$news_sub[data$data_channel_is_world==1] <- "world"
##test visualization
p1 <- ggplot(data=data, aes(as.factor(news_sub), log_shares))
p1 + geom_boxplot()
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~~~~SPLIT DATA BY DAY~~~~~~~~~~##

data$news_day <- rep("Sunday", nrow(data))
data$news_day[data$weekday_is_monday==1] <- "Monday"
data$news_day[data$weekday_is_tuesday==1] <- "Tuesday"
data$news_day[data$weekday_is_wednesday==1] <- "Wednesday"
data$news_day[data$weekday_is_thursday==1] <- "Thursday"
data$news_day[data$weekday_is_friday==1] <- "Friday"
data$news_day[data$weekday_is_saturday==1] <- "Saturday"
p1 <- ggplot(data=data, aes(as.factor(news_day), log_shares))
p1 + geom_boxplot()
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~~~~~~FIND ADJ R2~~~~~~~~~~~~~~##
sublist<-split(data, data$news_sub)
RsqrSub <- data.frame("sub"=names(sublist), "Rsqr"=rep(0,6))
for(i in 1:6){
  sublist[[i]]<-sublist[[i]][,-c(57,58)]
}

for(i in 1:6){
  temp<-lm(log_shares~ ., data=sublist[[i]])
  RsqrSub[i,2]<-summary(temp)$adj.r.squared
}
##~~~~~~~PLOT ADJ R SQUARED~~~~~~~##
ggplot(aes(x=factor(sub), y=Rsqr), data=RsqrSub) + geom_bar(stat="identity") + labs(x="news subjects", y="Adjusted R squared")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##CREATE REGRESSION OF ONLY BUS AND SOCMED##
names<-names(sublist[[1]])
total <- rbind(sublist[[1]],sublist[[4]])

totalfull <- lm(log_shares ~ ., data=total) 
summary(totalfull)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~REMOVE PREDICTORS WITH P VALUE NA##

total <- total[,names(total)!="n_non_stop_words"]
total <- total[,names(total)!="data_channel_is_lifestyle"]
total <- total[,names(total)!="data_channel_is_entertainment"]
total <- total[,names(total)!="data_channel_is_tech"]
total <- total[,names(total)!="data_channel_is_world"]
total <- total[,names(total)!="weekday_is_sunday"]
total <- total[,names(total)!="is_weekend"]
total <- total[,names(total)!="rate_negative_words"]
par(mfrow=c(2,3))
plot(totalfull, which = c(1:6))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~SEARCH AND DESTROY OUTLIERS~~~##
outliers <- c(4507,5371,16282,17138,17267)
tot<-total[!rownames(total) %in% outliers,]
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~BACKWARDS MODEL SELECTION~~~~~##
totfull <- lm(log_shares ~ ., data=tot) 
backstep <- step(totfull, direction= "backward", trace = 0)
summary(lm(formula(backstep), data=tot))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~FORWARDS MODEL SELECTION~~~~~~##
smallfit <- lm(log_shares ~ 1, data=tot)
forwardstep <- step(smallfit, scope = formula(totfull), direction = "forward", trace = 0)
summary(lm(formula(forwardstep), data=tot))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##COMPARE BACK & FORWARD COEFFICIENTS##
bc <- names(backstep$coefficients)
fc <- names(forwardstep$coefficients)
c <- unique(c(bc, fc))
bcind <- c %in% bc
fcind <- c %in% fc
varcompare <- data.frame("Variables"=c, "Backward"=bcind, "Forward"=fcind, "Both Exist" = bcind&fcind&(bcind||fcind))
varcompare
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##FIND CORRELATION FOR COEFFS IN COMMON##

desM <- tot[,names(tot) %in% c]
melting<-melt(cor(desM))
melting <- melting[melting$value!=1,]
melting <- melting[order(melting$value, decreasing =T),]
names(melting) <- c("var1", "var2", "correlation")
head(melting)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##BUILD MODELS TO COMPARE EACH PLUS INTERACTING COEFFICIENTS##
intfit1 <- update(backstep, .~.+sqrt_self_reference_avg_sharess:sqrt_self_reference_min_shares)
intfit2 <- update(forwardstep, .~.+sqrt_self_reference_avg_sharess:sqrt_self_reference_min_shares)
intfit3 <- update(forwardstep, .~.+abs_title_sentiment_polarity:title_subjectivity)
intfit4 <- update(forwardstep, .~.+sqrt_self_reference_avg_sharess:sqrt_self_reference_min_shares+abs_title_sentiment_polarity:title_subjectivity)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~~~~~GRAPH TO COMPARE~~~~~~~~~~##
par(mfrow=c(1,1))
R2<-c(summary(backstep)$adj.r.squared,summary(forwardstep)$adj.r.squared,summary(intfit1)$adj.r.squared,summary(intfit2)$adj.r.squared,summary(intfit3)$adj.r.squared,summary(intfit4)$adj.r.squared)
barplot(R2, 1:6, 0.005, names = "1:6", ylab = "R2")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~GRAPH CROSS VALIDATION FOLDS~~~##
modellist <- list(backstep, forwardstep, intfit1, intfit2, intfit3, intfit4)
modellistnames <- list("backstep", "forwardstep", "intfit1", "intfit2", "intfit3", "intfit4")
ms <- numeric()
df <- numeric()
for(i in 1:6){
  cv <- suppressWarnings(CVlm(data=tot, modellist[[i]], m=6, printit=F))
  ms <- c(ms,attributes(cv)$ms)
  df <- c(df, attributes(cv)$df)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~CREATE BINARY BASED ON LOG~~~~~##
shares01log <- rep(0, length(tot$log_shares))
shares01log[tot$log_shares > median(tot$log_shares)] <- 1
tot <- data.frame(tot, shares01log)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~TRAINING AND TEST USING BINARY~~##
train <- 1:(floor(length(tot$log_shares) * 0.8))
test <- (floor(length(tot$log_shares) *0.8 + 1)):length(tot$log_shares)
tot.train <- tot[train,]
tot.test <- tot[test,]
shares01log.test <- tot$shares01log[test]
x=model.matrix(tot$log_shares~.,tot )[,-1]
y=tot$log_shares
y.test <- y[test]
grid=10^seq(10,-2, length =100)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~~~~RIDGE REGRESSION~~~~~~~~~~~##
ridge.mod=glmnet (x,y,alpha=0, lambda=grid)
plot(ridge.mod)
predict(ridge.mod,s=50,type="coefficients")[1:48,]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:48,]
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
#var(cv.out$cvm)

bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
RIDGEMSE <- mean((ridge.pred-y.test)^2)
RIDGEMSE
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:48,]
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~~~~~~~~~LASSO~~~~~~~~~~~~~~~~~##
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
LASSOMSE<-mean((lasso.pred-y.test)^2)
LASSOMSE
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:48,]
lasso.coef
lasso.coef[lasso.coef!=0]
##Lasso out performs ridge in general
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~~~~~~~~~~LDA~~~~~~~~~~~~~~~~~~##
fit.lda <- lda(shares01log ~ ., data = tot, subset = train)
pr <- predict(fit.lda, tot.test, type = "response")
t<-table(pr$class, shares01log.test)
mean(pr$class !=shares01log.test)
passrate <- ((t[1] + t[4])/(t[1]+t[2]+t[3]+t[4]))
require(MASS)
fractions(passrate)
failrate <- 1 - passrate
c( ("Fail Rate: "),failrate)
type1 <- t[2] /(t[1]+t[2]+t[3]+t[4])
c( ("Type 1 errors: "),type1)
type2 <- t[3]/(t[1]+t[2]+t[3]+t[4])
c( ("Type 2 Errors:"),type2)
powerLDA <- 1 - type2
c("Power of the Model", powerLDA)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~~~~~~~~KNN~~~~~~~~~~~~~~~~~~~~##
train.knn <- tot[train, ]
test.knn <- tot[test, ]
shares01log.train<- tot$shares01log[train]
##LOOP THAT PULLS THE BEST K-VALUE
knns_b <-seq(1,20,2 )
knn_dfb <- data.frame( knn_num = c(), knn_mean= c())
#PLEASE RUN THE LOOP ALL TOGETHER, STEPPING THROUGH IT CAUSES AN ERROR
for (i in 1:length(knns_b))
{ k <- knns_b[i]
nam <- paste("KNNb", k, sep = "_")
m <- assign(nam, knn(train.knn, test.knn, shares01log.train, k))
knn_dfb[i, 'knn_num'] <- nam
knn_dfb[i, 'knn_mean'] <- mean(m == shares01log.test)
}
knn_dfb
knn_best <- knn_dfb[which.max(knn_dfb$knn_mean),]
print(paste('KNN Model', knn_best[1], 'has the highest mean with', knn_best[2]))

pred.knn <- knn(train.knn, test.knn, shares01log.train , k = 1)
t<-table(pred.knn, shares01log.test)
passrate <- ((t[1] + t[4])/(t[1]+t[2]+t[3]+t[4]))
require(MASS)
fractions(passrate)
failrate <- 1 - passrate
c( ("Fail Rate: "),failrate)
type1 <- t[2] /(t[1]+t[2]+t[3]+t[4])
c( ("Type 1 errors: "),type1)
type2 <- t[3]/(t[1]+t[2]+t[3]+t[4])
c( ("Type 2 Errors:"),type2)
powerKNN <- 1 - type2
c("Power of the Model", powerKNN)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
print(c("The MSE of fitting all of the data to a glm:", MSEfirstfit))
names(ms)<-modellistnames
print(c("Best Step Linear Model by MSE: ", ms[which.min(ms)]))
reg <- c("Ridge Regression" = RIDGEMSE,"Lasso" =LASSOMSE)
print(c("Best Regression(Lasso or Ridge) by Average MSE: ",reg[which.min(reg)]))
model <- c("KNN" = powerKNN, "LDA" = powerLDA)
print(c("Best Predictive Model(LDA or KNN) by Power: ",model[which.max(model)]))


