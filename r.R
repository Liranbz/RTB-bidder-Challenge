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
names(data)
#-----------------------------Data preparation--------------------------------------------
#data$subProduct<-as.numeric(data$subProduct)
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
data<-clean_data
boxplot(clean_data$clicks)
summary(clean_data$clicks)
data$log_clicks<-(log10(data$clicks))

hist(log10(data$impressions))
data$Log_impressions<-(log10(data$impressions))

summary(data$Log_impressions)
#boxplot(data$clicks)
copy_data<-data
data<-data[sample(nrow(data), 40000), ]
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
Train_test_Data=Train_test_division(0.6,data)
Train=Train_test_Data$Train
Test=Train_test_Data$Test


#---------------------------------------Machine Learning models------------------------

#----------------------------------------RF- with features-------------------------------------------
install.packages("randomForest")
library(randomForest)

names(Train)
gc()
memory.limit(size = 56000)

# Create a Random Forest model with default parameters
model1 <- randomForest(Log_impressions ~ eDate+channel+os+networkType+deviceType+publisherCategory+advertiserCategory+advMaturity+rate+clicks+AverageWinPrice..CPM., data = Train, importance = TRUE)
model1
plot(model1)
importance(model1)
varImpPlot(model1)
table(predict(model1),Train$Log_impressions)

# Fine tuning parameters of Random Forest model
tune.rf <- tuneRF(Train[,-c(1,5,11:14,16,18)],Train[,19], stepFactor=0.5)
print(tune.rf)

model2 <- randomForest(Log_impressions ~ eDate+channel+os+networkType+deviceType+publisherCategory+advertiserCategory+advMaturity+rate+clicks+AverageWinPrice..CPM., data = Train, ntree = 501, mtry = 6, importance = TRUE)
model2
plot(model2)
importance(model2)
varImpPlot(model2)
