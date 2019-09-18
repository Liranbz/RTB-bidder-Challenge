#-------------------------------------------owner: Liran Ben-Zion-------------------------------------------
#-------------------------------------------email- bzliran@gmail.com----------------------------------------

rm(list=ls())
install.packages("dplyr")
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
#-----------------------------Data preparation--------------------------------------------
data$eDate<-as.Date(as.character(data$eDate,format="%y/%m/%d")) # Formatting date as date format from string

# Checking how many NA are there
#missing_values_summary_table<-t(md.pattern(data, plot = TRUE))
data <- na.omit(data)

#transformations on data
summary(data$clicks)
boxplot(data$clicks)

outliers <- boxplot(data$clicks, plot=FALSE)$out
data <- data[-which(data$clicks %in% outliers),]

data$log_clicks<-(log10(data$clicks))

#impressions
hist(log10(data$impressions))
data$Log_impressions<-(log10(data$impressions))

data<-data[,-c(1,2,16)]
data_to_predict<-data_to_predict[,-c(1)]
data<-data[,-c(15)]
#reduce number of levels in categorical variables
library(forcats)
data$country<-fct_lump(data$country, 52)
data$subProduct<-fct_lump(data$subProduct, 52)
data$country<-fct_lump(data$country, 52)
data$campaign<-fct_lump(data$campaign, 52)
data$advPackage<-fct_lump(data$advPackage, 52)



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

data<-data[sample(nrow(data), 20000), ]

Train_test_Data=Train_test_division(0.8,data)
Train=Train_test_Data$Train
Test=Train_test_Data$Test


#---------------------------------------Machine Learning models------------------------

#----------------------------------------RF- with features-------------------------------------------
install.packages("randomForest")
library(randomForest)

names(Train)
gc()
memory.limit(size = 56000)
str(Train)
# Create a Random Forest model with default parameters
model1 <- randomForest(Log_impressions ~ eDate+channel+os+networkType+deviceType+publisherCategory+advertiserCategory+advMaturity+rate+clicks+AverageWinPrice..CPM., data = Train, importance = TRUE)
model1
plot(model1)
importance(model1)
varImpPlot(model1)
table(predict(model1),Train$Log_impressions)

# Fine tuning parameters of Random Forest model
tune.rf <- tuneRF(Train[,-c(15)],Train[,15], stepFactor=0.5)
print(tune.rf)


model2 <- randomForest(Log_impressions ~ eDate+channel+os+networkType+deviceType+publisherCategory+advertiserCategory+advMaturity+rate+clicks+AverageWinPrice..CPM., data = Train, ntree = 401, mtry = 6, importance = TRUE)
model2
plot(model2)
importance(model2)
varImpPlot(model2)
prediction <-predict(model2, Test)
library(Metrics)

MSE<-mean((prediction - Test$Log_impressions)^2)

model3 <- randomForest(Log_impressions ~., data = Train, ntree = 400, mtry = 6, importance = TRUE)
model3
plot(model3)
importance(model3)
varImpPlot(model3)
prediction <-predict(model3,Test)
mse<-mean((prediction - Test$Log_impressions)^2)
library(Metrics)

MSE<-mean((prediction - Test$Log_impressions)^2)
#------------------use data to predict-----------------
data_to_predict<-as.data.frame(data_to_predict,stringsAsFactors =FALSE) 
str(data_to_predict)
data_to_predict$eDate<-as.Date(as.character(data_to_predict$eDate,format="%y/%m/%d")) # Formatting date as date format from string

# Checking how many NA are there
data_to_predict <- na.omit(data_to_predict)

#create sub levels
data_to_predict$country<-fct_lump(data_to_predict$country, 52)
data_to_predict$subProduct<-fct_lump(data_to_predict$subProduct, 52)
data_to_predict$country<-fct_lump(data_to_predict$country, 52)
data_to_predict$campaign<-fct_lump(data_to_predict$campaign, 52)
data_to_predict$advPackage<-fct_lump(data_to_predict$advPackage, 52)
data_to_predict$publisherCategory<-fct_lump(data_to_predict$publisherCategory, 52)

#copy_data<-data_to_predict

#use the daata for predicition
prediction_values <-predict(model3, copy_data)



