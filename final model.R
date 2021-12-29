library(tidyverse)
library(data.table)
library(dplyr)
library(readr)
library(MASS)
library(leaps)
library(randomForest)
library(caret)
library(ROSE)
library(e1071)
library(DMwR)
library(party)
library(pROC)

rm(list = ls())

setwd("C:/Users/Kai/OneDrive - UT Cloud/Documents/Praktikum Mitacs/logistic_regression_dataset_new")

myData <- read.csv("mergedDatawithcolour.csv")          #read in the data for the model
myData <- myData[!is.na(myData$A),]       #remove all rows where there is a missing value in the column "A"
myData <- myData[!is.na(myData$A2020),]   #remove all rows where there is a missing value in the column "A2020"
myData$A2020 <- as.factor(myData$A2020) 

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(myData), replace = TRUE, prob = c(0.8, 0.2))
train <- myData[ind==1,]
test <- myData[ind==2,]

#delete columns which are not used for the model
train[c(1,2,3,4,5,6,7,8,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37)] <- list(NULL)
test[c(1,2,3,4,5,6,7,8,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37)] <- list(NULL)

#create stratified random forest model including all variables
nRareSamples = 300
rf.strata <-  randomForest(A2020 ~ .,
                           data = train, mtry=3, ntree=1001, importance=TRUE, strata=train$A2020,
                           sampsize=c(nRareSamples,nRareSamples))

rf.strata
plot(rf.strata)

#evaluate model on testing data set
cm <- confusionMatrix(predict(rf.strata, test), test$A2020, positive = "1")
cm
cm$byClass

#create roc curve of model with testing data set and print auc
ptrains <- predict(rf.strata,test,type = "prob")
roc.curve(test$A2020,ptrains[,2])$auc
rf.roc <- roc(test$A2020,ptrains[,2])
rf.roc$auc

#text block for ggplot
grob <- grobTree(textGrob("AUC = 0.91", x=0.4,  y=0.75, hjust=0.5,
                          gp=gpar(col="black", fontsize=14)))

#create ggplot of roc curve
ggroc(rf.roc) +
  annotation_custom(grob) +
  ggtitle("ROC Random Forest") +
  theme_bw() +
  theme(
    plot.title = element_text(color="black", size=17, face="bold",hjust = 0.5)) 
  

