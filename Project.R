data=read.csv("Walmart_date_seperate.csv")
head(data)
dim(data)[1]
hist(data$Weekly_Sales, main="", xlab="Weekly Sales")
boxplot(Weekly_Sales~Store, main="Boxplot for weekly sales versus stores",xlab="Store",ylab="Weekly Sales",data=data)
boxplot(Weekly_Sales~Month, main="Boxplot for weekly sales versus months",xlab="Months",ylab="Weekly Sales",data=data)
boxplot(Weekly_Sales~Year, main="Boxplot for weekly sales versus years",xlab="Years",ylab="Weekly Sales",data=data)
counts=table(data$Holiday_Flag,data$Month)
barplot(counts,main="Barplot for number of holidays in months",xlab="Months",ylab="Count of 0 or 1",)
plot(data$Temperature,data$Weekly_Sales,xlab="Temperature",ylab="Weekly Sales",main="Relation between weekly sales and temperature",cex=0.5)
abline(lm(Weekly_Sales~Temperature,data=data),col="red")
plot(data$Fuel_Price,data$Weekly_Sales,xlab="Fuel_Price",ylab="Weekly Sales",main="Relation between weekly sales and fuel price",cex=0.5)
abline(lm(Weekly_Sales~Fuel_Price,data=data),col="red")
plot(data$CPI,data$Weekly_Sales,xlab="CPI",ylab="Weekly Sales",main="Relation between weekly sales and CPI",cex=0.5)
abline(lm(Weekly_Sales~CPI,data=data),col="red")

#Test train split
set.seed(0)
sample_size=floor(0.8*nrow(data))
picked=sample(seq_len(nrow(data)),size=sample_size)
train=data[picked,]

# Converting the numerical cateogrical variables to predictors
train$Store = as.factor(train$Store)
train$Holiday_Flag = as.factor(train$Holiday_Flag)
train$Year = as.factor(train$Year)
train$Month = as.factor(train$Month)
train$Day = as.factor(train$Day)
train$Weekday = as.factor(train$Weekday)

test=data[-picked,]
test$Store = as.factor(test$Store)
test$Holiday_Flag = as.factor(test$Holiday_Flag)
test$Year = as.factor(test$Year)
test$Month = as.factor(test$Month)
test$Day = as.factor(test$Day)
test$Weekday = as.factor(test$Weekday)

# Fit the model
model1 <- lm(Weekly_Sales~., data=train)
summary(model1)
which(summary(model1)$coeff[,4]>0.05)
model1_resids=rstandard(model1)
model1_fits=model1$fitted

#Linearity assumption for model1
plot(model1_fits,model1_resids, main="Scatter plot of residuals and fitted values(model1)", xlab="Fitted Values", ylab="Residuals",cex=0.5)
plot(train$Temperature,model1_resids, main="Scatter plot of residuals and temperature", xlab="Temperature", ylab="Residuals",cex=0.5)
plot(train$Fuel_Price,model1_resids, main="Scatter plot of residuals and fuel price", xlab="Fuel Price", ylab="Residuals",cex=0.5)
plot(train$CPI,model1_resids, main="Scatter plot of residuals and CPI", xlab="CPI", ylab="Residuals",cex=0.5)
plot(train$Unemployment,model1_resids, main="Scatter plot of residuals and unemployment", xlab="Unemployment", ylab="Residuals", cex=0.5)

#Normality assumption for model1
hist(model1_resids, nclass=100)
library("car")
qqPlot(model1_resids, main="qq plot of model1 residuals")
qqline(model1_resids,col="red")

#outliers for model1
cook=cooks.distance(model1)
plot(cook,main="cook distance of model1",type="h")

#check multicollinearity for model1
vif(model1) #remove CPI

#clean data for testing and training submodel
clean_outlier_index=which(cook<=(4/dim(train)[1]))
clean_outlier_train=train[clean_outlier_index,]
clean_CPI_train=subset(train,select=-c(CPI))
clean_CPI_test=subset(test,select=-c(CPI))
clean_outlier_CPI_train=subset(clean_outlier_train,select=-c(CPI))

#model2(Delete CPI column)
model2 <- lm(Weekly_Sales~., data=clean_CPI_train)
summary(model2)
which(summary(model2)$coeff[,4]>0.05)
model2_resids=rstandard(model2)
model2_fits=model2$fitted

#model3(Delete outliers)
model3 <- lm(Weekly_Sales~., data=clean_outlier_train)
summary(model3)
which(summary(model3)$coeff[,4]>0.05)
model3_resids=rstandard(model3)
model3_fits=model3$fitted

#model4(Squared Weekly_Sales)
model4 <- lm(sqrt(Weekly_Sales)~., data=train)
summary(model4)
which(summary(model4)$coeff[,4]>0.05)
model4_resids=rstandard(model4)
model4_fits=model4$fitted

#model5(Delete CPI column and squared Weekly_Sales)
model5 <- lm(sqrt(Weekly_Sales)~., data=clean_CPI_train)
summary(model5)
which(summary(model5)$coeff[,4]>0.05)
model5_resids=rstandard(model5)
model5_fits=model5$fitted

#model6(Delete outliers and squared Weekly_Sales)
model6 <- lm(sqrt(Weekly_Sales)~., data=clean_outlier_train)
summary(model6)
which(summary(model6)$coeff[,4]>0.05)
model6_resids=rstandard(model6)
model6_fits=model6$fitted

#model7(Delete outliers and delete CPI column)
model7 <- lm(Weekly_Sales~., data=clean_outlier_CPI_train)
summary(model7)
which(summary(model7)$coeff[,4]>0.05)
model7_resids=rstandard(model7)
model7_fits=model7$fitted

#model8(Delete outliers and delete CPI column and squared Weekly_Sales)
model8 <- lm(sqrt(Weekly_Sales)~., data=clean_outlier_CPI_train)
summary(model8)
which(summary(model8)$coeff[,4]>0.05)
model8_resids=rstandard(model8)
model8_fits=model8$fitted

#prediction1
pred1=predict(model1,test,interval='prediction')
test.pred1=pred1[,1]
test.lwr1=pred1[,2]
test.upr1=pred1[,3]
mean((test.pred1-test$Weekly_Sales)^2)#MSPE
sum((test.pred1-test$Weekly_Sales)^2)/sum((test$Weekly_Sales-mean(test$Weekly_Sales))^2)

#prediction2
pred2=predict(model2,clean_CPI_test,interval='prediction')
test.pred2=pred2[,1]
test.lwr2=pred2[,2]
test.upr2=pred2[,3]
mean((test.pred2-clean_CPI_test$Weekly_Sales)^2)#MSPE
sum((test.pred2-clean_CPI_test$Weekly_Sales)^2)/sum((clean_CPI_test$Weekly_Sales-mean(clean_CPI_test$Weekly_Sales))^2)

#prediction3
pred3=predict(model3,test,interval='prediction')
test.pred3=pred3[,1]
test.lwr3=pred3[,2]
test.upr3=pred3[,3]
mean((test.pred3-test$Weekly_Sales)^2)#MSPE
sum((test.pred3-test$Weekly_Sales)^2)/sum((test$Weekly_Sales-mean(test$Weekly_Sales))^2)

#prediction4
pred4=predict(model4,test,interval='prediction')
test.pred4=pred4[,1]^2
test.lwr4=pred4[,2]^2
test.upr4=pred4[,3]^2
mean((test.pred4-test$Weekly_Sales)^2)#MSPE
sum((test.pred4-test$Weekly_Sales)^2)/sum((test$Weekly_Sales-mean(test$Weekly_Sales))^2)

#prediction5
pred5=predict(model5,clean_CPI_test,interval='prediction')
test.pred5=pred5[,1]^2
test.lwr5=pred5[,2]^2
test.upr5=pred5[,3]^2
mean((test.pred5-clean_CPI_test$Weekly_Sales)^2)#MSPE
sum((test.pred5-clean_CPI_test$Weekly_Sales)^2)/sum((clean_CPI_test$Weekly_Sales-mean(clean_CPI_test$Weekly_Sales))^2)

#prediction6
pred6=predict(model6,test,interval='prediction')
test.pred6=pred6[,1]^2
test.lwr6=pred6[,2]^2
test.upr6=pred6[,3]^2
mean((test.pred6-test$Weekly_Sales)^2)#MSPE
sum((test.pred6-test$Weekly_Sales)^2)/sum((test$Weekly_Sales-mean(test$Weekly_Sales))^2)

#prediction7
pred7=predict(model7,clean_CPI_test,interval='prediction')
test.pred7=pred7[,1]
test.lwr7=pred7[,2]
test.upr7=pred7[,3]
mean((test.pred7-clean_CPI_test$Weekly_Sales)^2)#MSPE
sum((test.pred7-clean_CPI_test$Weekly_Sales)^2)/sum((clean_CPI_test$Weekly_Sales-mean(clean_CPI_test$Weekly_Sales))^2)

#prediction8
pred8=predict(model8,clean_CPI_test,interval='prediction')
test.pred8=pred8[,1]^2
test.lwr8=pred8[,2]^2
test.upr8=pred8[,3]^2
mean((test.pred8-clean_CPI_test$Weekly_Sales)^2)#MSPE
sum((test.pred8-clean_CPI_test$Weekly_Sales)^2)/sum((clean_CPI_test$Weekly_Sales-mean(clean_CPI_test$Weekly_Sales))^2)

#Assumptions for model2
plot(model2_fits,model2_resids, main="Scatter plot of residuals and fitted values(model2)", xlab="Fitted Values", ylab="Residuals",cex=0.5)
hist(model2_resids, nclass=100)
library("car")
qqPlot(model2_resids,main="qq plot of model2 residuals")
qqline(model2_resids,col="red")

#Assumptions for model3
plot(model3_fits,model3_resids, main="Scatter plot of residuals and fitted values(model3)", xlab="Fitted Values", ylab="Residuals",cex=0.5)
hist(model3_resids, nclass=100)
library("car")
qqPlot(model3_resids,main="qq plot of model3 residuals")
qqline(model3_resids,col="red")

#Assumptions for model4
plot(model4_fits,model4_resids, main="Scatter plot of residuals and fitted values(model4)", xlab="Fitted Values", ylab="Residuals",cex=0.5)
hist(model4_resids, nclass=100)
library("car")
qqPlot(model4_resids,main="qq plot of model4 residuals")
qqline(model4_resids,col="red")

#Assumptions for model5
plot(model5_fits,model5_resids, main="Scatter plot of residuals and fitted values(model5)", xlab="Fitted Values", ylab="Residuals",cex=0.5)
hist(model5_resids, nclass=100)
library("car")
qqPlot(model5_resids,main="qq plot of model5 residuals")
qqline(model5_resids,col="red")

#Assumptions for model6
plot(model6_fits,model6_resids, main="Scatter plot of residuals and fitted values(model6)", xlab="Fitted Values", ylab="Residuals",cex=0.5)
hist(model6_resids, nclass=100)
library("car")
qqPlot(model6_resids,main="qq plot of model6 residuals")
qqline(model6_resids,col="red")

#Assumptions for model7
plot(model7_fits,model7_resids, main="Scatter plot of residuals and fitted values(model7)", xlab="Fitted Values", ylab="Residuals",cex=0.5)
hist(model7_resids, nclass=100)
library("car")
qqPlot(model7_resids,main="qq plot of model7 residuals")
qqline(model7_resids,col="red")

#Assumptions for model8
plot(model8_fits,model8_resids, main="Scatter plot of residuals and fitted values(model8)", xlab="Fitted Values", ylab="Residuals",cex=0.5)
hist(model8_resids, nclass=100)
library("car")
qqPlot(model8_resids,main="qq plot of model8 residuals")
qqline(model8_resids,col="red")

#perform feature selection
full=lm(Weekly_Sales~.,data=clean_CPI_train)
minimum=lm(Weekly_Sales~Store+Holiday_Flag+Month,data=clean_CPI_train)
step(full, scope=list(lower=minimum, upper=full), direction="backward")


