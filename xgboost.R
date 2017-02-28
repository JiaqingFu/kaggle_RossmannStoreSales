require(xgboost)
data.train<-read.csv('D:/Rproject/xgboost/train1.csv',header = T)
data.test<-read.csv('D:/Rproject/xgboost/test1.csv',header = T)
data.train<-data.train[data.train$Open==1,]
train<-as.matrix(data.frame(data.train$DayOfWeek,data.train$StateHoliday,
data.train$Promo,data.train$SchoolHoliday,data.train$Date,data.train$Store))
label<-as.matrix(data.frame(data.train$Sales))
bst <- xgboost(data = train, label = label, max.depth = 20, eta = 1, nround = 150
               , objective = "reg:linear")
pred <- predict(bst, as.matrix(data.frame(data.test$DayOfWeek,data.test$StateHoliday,
        data.test$Promo,data.test$SchoolHoliday,data.test$Date,data.test$Store)))

fi<-data.test[data.test$Open==0,]
pred[fi$Id]<-0
write.csv(data.frame(pred), file = "c:/sub1-1.csv",col.names=c('Sales'))
