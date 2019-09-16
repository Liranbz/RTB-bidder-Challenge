#-------------------------------------------owner: Liran Ben-Zion-------------------------------------------
#-------------------------------------------email- bzliran@gmail.com----------------------------------------

rm(list=ls())
library(dplyr)
library(data.table)
library(mice)
library(ggplot2)
library(corrplot)
library(RCurl)
library(Hmisc)

#--------------------------------load data-------------------------------------------------------------

data <- read.csv(file="C:\\Users\\bzlir\\Desktop\\Startapp\\data.csv", header=TRUE, sep=",")
data_to_predict<-read.csv(file="C:\\Users\\bzlir\\Desktop\\Startapp\\data_to_predict.csv", header=TRUE, sep=",")
data<-as.data.frame(data,stringsAsFactors =FALSE) 
str(data)
names(data)
#-----------------------------Data preparation--------------------------------------------
data$product<-as.factor(data$product)
data$eDate<-as.Date(as.character(data$eDate,format="%y/%m/%d")) # Formatting date as date format from string

# Checking how many NA are there
missing_values_summary_table<-t(md.pattern(data, plot = TRUE))
data <- na.omit(data)

#transformations on data
summary(data$clicks)
boxplot(data$clicks)
outliers <- boxplot(data$clicks, plot=FALSE)$out
data[which(data$clicks %in% outliers),]
clean_data <- data[-which(data$clicks %in% outliers),]
boxplot(clean_data$clicks)
summary(clean_data$clicks)
data$log_clicks<-(log10(data$clicks))

hist(log10(data$impressions))
data$Log_impressions<-(log10(data$impressions))

summary(data$Log_impressions)
#boxplot(data$clicks)


#data<-data[,-c(1:2)]

#--------------------------------------Create sets for train and test--------------------------------------
#sample data
row_sampler=function(df){
  set.seed(789)
  n_rows_data=(nrow(df))
  random_row_nums <-sample(x=1:n_rows_data,size=n_rows_data,replace = FALSE)
  return(random_row_nums)
}

Train_test_division=function(train_fraction,df){
  random_rows=row_sampler(df)
  Division_point=round(nrow(df)*train_fraction,digits = 0)
  Train_indices=random_rows[1:Division_point]
  Test_indices=random_rows[(1+Division_point):length(random_rows)]
  Train=df[Train_indices,]
  Test=df[Test_indices,]
  return(list(Train=Train,Test=Test))
}
Train_test_Data=Train_test_division(0.75,data)
Train=Train_test_Data$Train
Test=Train_test_Data$Test
#---------------------------------------Machine Learning models-------------------------

library(caret)
library(foreach)
library(doParallel)
#--------train control-----
trCtrl <- trainControl(
  method = "repeatedcv"
  , number = 2
  , repeats = 5
  , allowParallel = TRUE
)


#----------------------------------------model_xgbLinear with some features-------------------------------------------
model_xgbLinear1<-train(form = Train$Log_impressions~., data=Train, trControl = trCtrl,method='xgbLinear')

summary(model_xgbLinear1) # summarizing the model
print(model_xgbLinear1)
plot(model_xgbLinear1)
varImp(object=model_xgbLinear1)
plot(varImp(object=model_xgbLinear1),main="model_xgbLinear - Variable Importance, 7 features")

#Predictions
predictions1<-predict.train(object=model_xgbLinear1,Test[,-Test$Log_impressions],type="raw")
RMSE_model_xgbLinear1=RMSE(predictions1,Test$Log_impressions)

#----------------------------------------RF- with features-------------------------------------------

model_rf<-train(form = Train$Log_impressions~., data=Train, trControl = trCtrl,method='rf')

summary(model_rf)
print(model_rf)
plot(model_rf)
varImp(object=model_rf)
plot(varImp(object=model_rf),main="model_rf - Variable Importance")

#Predictions
predictions_rf<-predict.train(object=model_rf,Test[,-Test$Log_impressions],type="raw")
RMSE_model_xgbLinear1=RMSE(predictions_rf,Test$Log_impressions)