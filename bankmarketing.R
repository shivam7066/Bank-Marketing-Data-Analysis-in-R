rm(list=ls())

#Date: December 2nd, 2018
#Author: Shivam Pandit
#Purpose: Analysis of Bank Marketing Dataset

library(ggplot2)
library(caret) # Accuracy
library(e1071)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(caTools)
library(descr)

#Read data
#Downloading dataset and importing
library(data.table)
temp <- tempfile()
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/00332/OnlineNewsPopularity.zip', temp)
data  <- data.table(read.table(unzip(zipfile = temp, 
                                     files = 'OnlineNewsPopularity/OnlineNewsPopularity.csv', 
                                     junkpaths = TRUE), header = TRUE, sep = ',', stringsAsFactors = FALSE))

#Using Local Dataset location
#data <- read.csv(file="C:/Users/Shivam Pandit/Desktop/Data Science/Individual Project/bank-additional-full.csv", header=TRUE, sep=",")

#summary of data
dim(data)
names(data)

#summary before cleaning
summary(data)

#############################################
# Exploratory Data Analysis & Cleaning Data:
#############################################

# Check for any missing values:
sum(is.na(data))

summary(data)
 
d2 <- data

#Converting quantititative values to numeric class

d2$age <- as.numeric(d2$age)
d2$duration <- as.numeric(d2$duration)
d2$campaign <- as.numeric(d2$campaign)
d2$pdays <- as.numeric(d2$pdays)
d2$previous <- as.numeric(d2$previous)
d2$emp.var.rate <- as.numeric(d2$emp.var.rate)
d2$cons.price.idx <- as.numeric(d2$cons.price.idx)
d2$cons.conf.idx <- as.numeric(d2$cons.conf.idx)
d2$nr.employed <- as.numeric(d2$nr.employed)

#checking classes of attributes after transformation
sapply(d2,class)


#########################################################
# Sampling the dataset into training data and test data:
#############################################################
set.seed(2262)
sample <- sample.int(n = nrow(d2), size = floor(.8*nrow(d2)), replace = F)
traindata <- data[sample, ]
testdata  <- data[-sample, ]

sapply(d2,class)


#Analyzing barplots of variables
par(mfrow=c(2,2))
for(i in 1:length(data))
  {barplot(prop.table(table(data[,i])) , 
           xlab=names(data[i]), ylab= "Frequency (%)" , col = rainbow(3))}
  
summary(data)
sapply(d3,class)

##########################################################
###   Implementing CART
#########################################################

# Classification and Regression Trees
bank.cart<-rpart(y ~ ., traindata , method = 'class')

par(mfrow=c(1,1))
fancyRpartPlot(bank.cart , digits=2 , palettes = c("Purples", "Oranges"))

#predict
cart_pred <- predict( bank.cart , testdata , type = "class")
cart_prob <- predict( bank.cart , testdata , type = "prob")

# Confusion matrix
confusionMatrix(cart_pred , testdata$y)

### Cross table validation for CART
CrossTable(testdata$y, cart_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#########################
#Implementing C5.0
###########################
#install.packages("C50")
library(C50)
bank.c50 <- C5.0(y ~ . , traindata , trials=15)
bank.c50.pred<-predict(bank.c50,testdata,type="class" )
bank.c50.prob<-predict(bank.c50,testdata,type="prob" )
# Confusion matrix
confusionMatrix(bank.c50.pred, testdata$y)

### Cross table validation for random forest
CrossTable(testdata$y, bank.c50.pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

############################################
#Implementing KNN
###########################################
bank.knn <- train(y ~ ., data = traindata, method = "knn", 
                  maximize = TRUE,
                  trControl = trainControl(method = "cv", number = 10),
                  preProcess=c("center", "scale"))

predictedkNN <- predict(bank.knn , newdata = testdata)
confusionMatrix(predictedkNN , testdata$y)

### Cross table validation for KNN
CrossTable(testdata$y, predictedkNN,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))



