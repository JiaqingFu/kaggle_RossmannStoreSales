library(data.table)
library(zoo)
library(forecast)
library(plyr)
require(xgboost)

# test data
test <- fread("./test.csv")
test[, Date: = as.Date(Date)]
test$Open[is.na(test$Open)] <- 0.5
test$year <- 3
test$month <- as.numeric((format(test$Date, "%m")))
test$day <- as.numeric(as.factor(format(test$Date, "%d")))
test$monday = test$month + test$day / 31
test$StateHoliday <- as.numeric(as.factor(test$StateHoliday))
test$SchoolHoliday <- as.numeric(as.factor(test$SchoolHoliday))
# train data
train <- fread("./train.csv")
train[, Date: = as.Date(Date)]
train <- train[train$Open == 1,]
train <- train[train$StateHoliday == "0" | train$StateHoliday == "a",]
train[,logSales: = log1p(Sales)]
# store data
store <- fread("./store.csv")
train <- merge(train,store,by = "Store")
test <- merge(test,store,by = "Store")

train1 = as.data.frame(train)
train1$year <- as.numeric(as.factor(format(train1$Date, "%Y")))
train1$month <- as.numeric((format(train1$Date, "%m")))
train1$day <- as.numeric(as.factor(format(train1$Date, "%d")))
train1$monday = train1$month + train1$day / 31.0
train1 <- train1[train1$monday > 8 & train1$monday <= 9.55,]
train1$StateHoliday <- as.numeric(as.factor(train1$StateHoliday))
train1$SchoolHoliday <- as.numeric(as.factor(train1$SchoolHoliday))
label <- as.matrix(data.frame(train1$Sales))
# train2<-as.matrix(data.frame(train1$DayOfWeek,train1$StateHoliday,
# train1$Promo,train1$SchoolHoliday,train1$year,train1$monday,train1$Store))

cat("begin train")
xgb_fit = function(x) {
  label <- as.matrix(data.frame(x$logSales))
  train2 <- as.matrix(
    data.frame(
      x$DayOfWeek,x$StateHoliday,
      x$Promo2,x$CompetitionDistance,x$Promo,x$SchoolHoliday,x$monday
    )
  )
  bst <-
    xgboost(
      data = train2, label = label, max.depth = 20, eta = 0.3, nround = 150,
      nfold = 5, objective = "reg:linear"
    )
  cat(mode(bst))
  return(bst)
}
model <- dlply(train1,.(Store),xgb_fit)
#bst <- xgboost(data = train2, label = label, max.depth = 20, eta = 1, nround = 150,
#               nfold = 5, objective = "reg:linear")

cat("begin predict")
xgb_pre = function(x,y) {
  index = x$Store[1]
  fitt = y[[index]]
  forc <-
    predict(fitt, as.matrix(
      data.frame(
        x$DayOfWeek,x$StateHoliday,
        x$Promo2,x$CompetitionDistance,x$Promo,x$SchoolHoliday,x$monday
      )
    ))
  return(data.frame(x$Id,forc))
}
pred <- ddply(test,.(Store),xgb_pre,model)
# pred <- predict(bst, as.matrix(data.frame(test$DayOfWeek,test$StateHoliday,
# test$Promo,test$SchoolHoliday,test$year,test$monday,test$Store)))
pred <- pred[order(pred$x.Id),]
submission <- data.frame(Id = pred$x.Id, Sales = expm1(pred$forc))
pred <- pred$forc
length(pred[pred < 0])
fi <- test[test$Open == 0,]
pred[fi$Id] <- 0
length(pred[pred < 0])
pred[pred < 0] <- 0

write.csv(submission, file = "c:/sub6-2.csv",row.names = F)
# temp <- as.data.frame(train)
# temp$year <- as.factor(format(temp$Date, "%Y"))
# temp$month <- as.factor(format(temp$Date, "%m"))
# temp$day<- as.factor(format(temp$Date, "%d"))
# agg <- aggregate(Sales ~ ., data=temp[, c("Sales", "month" ,"year","day")], FUN=sum)
# #sales ~ . == sales ~ month+year+day
# SalesTS <- ts(agg$Sales, start=2013, frequency=365)
# col = rainbow(3)
# seasonplot(SalesTS, col=col, year.labels.left = TRUE, pch=19, las=1)
