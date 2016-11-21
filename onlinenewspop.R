rm(list=ls())
#####
#Final Project
#Online News Data Predictors
#
#####

#MySQL Data Connection
#install.packages("RMySQL")
library(RMySQL)
mydb = dbConnect(MySQL(), user='root', password='root', dbname='online_news', host='127.0.0.1')
#dbListTables(mydb)
rs = dbSendQuery(mydb, "select * from onlinenewspopularity")
newsDFrame = fetch(rs, n=-1)
#save(newsDFrame,file="newDFrame.Rda")
#dbDisconnect(mydb)

#Data frame info
str(newsDFrame)
length(newsDFrame)

####Set up learning set
#set.seed(5072)
#n = nrow(newsDFrame)
#smp_size <- floor(0.8 * n)

####Simple Fit
news_names = names(newsDFrame[1:61])
x = newsDFrame[,2:60]
y = newsDFrame[,61]
lm.fit = lm(shares ~.-url ,data = newsDFrame) #Too large a set to fit a model with? Error: cannot allocate vector of size 11.7Gb
coef(lm.fit)


plot(lm.fit$fitted.values,newsDFrame$shares)






