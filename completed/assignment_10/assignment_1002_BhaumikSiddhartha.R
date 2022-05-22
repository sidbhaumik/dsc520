# Assignment: ASSIGNMENT 10.2
# Name: Bhaumik, Siddhartha
# Date: 2022-05-18

## Set the working directory to the root of your DSC 520 directory
setwd("/Users/siddharthabhaumik/Documents/GitHub/dsc520")

## Load the `data/r4ds/heights.csv` to
patient_df <- read.csv("data/ThoraricSurgery.csv")

## Viewing Sample data
View(head(patient_df))

# Fit a linear model
patient.glm <- glm(Risk1Yr ~ DGN + PRE4 + PRE5  + PRE6  + PRE7  + PRE8 + PRE9 + PRE10 + PRE11 + PRE14 +
                      PRE17 + PRE19 + PRE25 + PRE30 + PRE32 + AGE, data = patient_df, family = "binomial")

# Print the model
patient.glm

# View the summary of your model
summary(patient.glm)

## Based on the summary results, it seems below 2 variables has the maximum effect on survival rate:
## 1) PRE17
## 2) PRE7

## What is the accuracy of your model?

set.seed(1234)
#load necessary packages
library(caTools)
library(caret)
library(InformationValue)
library(ISLR)

# 70:30 data split into training and validation
data_split = sample.split(patient_df, SplitRatio = 0.7)

training_data = subset(patient_df, data_split==TRUE)
test_data = subset(patient_df, data_split==FALSE)

# Print data frames
print(dim(training_data))
print(dim(test_data))

# Fit a linear model
new_patient_glm = glm( Risk1Yr ~ . , family="binomial", data = training_data)

#Summary
summary(new_patient_glm)

#Prediction based on test data
predict_data = predict(new_patient_glm, newdata = test_data, type = "response")

# Confusion matrix on test set
confusionMatrix(test_data$Risk1Yr,predict_data)

#calculate sensitivity: The “true positive rate” 
sensitivity(test_data$Risk1Yr,predict_data)

#calculate specificity: The “true negative rate”
specificity(test_data$Risk1Yr,predict_data)

#calculate total mis-classification error rate
misClassError(test_data$Risk1Yr,predict_data)

## The total misclassification error rate is 19.8% for this model which indicates its a fairly accurate model since the error rate is on the lower end.
