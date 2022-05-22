# Assignment: ASSIGNMENT 10.3
# Name: Bhaumik, Siddhartha
# Date: 2022-05-20

## Set the working directory to the root of your DSC 520 directory
setwd("/Users/siddharthabhaumik/Documents/GitHub/dsc520")

## Load the `data/r4ds/heights.csv` to
class_df <- read.csv("data/binary-classifier-data.csv")

## Viewing Sample data
View(head(class_df))

# Fit a linear model
class.glm <- glm(label ~ x + y, data = class_df, family = "binomial")

# Print the model
class.glm

# View the summary of your model
summary(class.glm)


## What is the accuracy of your model?

set.seed(1234)
#load necessary packages
library(caTools)
library(caret)
library(InformationValue)
library(ISLR)

# 70:30 data split into training and validation
data_split = sample.split(class_df, SplitRatio = 0.7)

training_data = subset(class_df, data_split==TRUE)
test_data = subset(class_df, data_split==FALSE)

# Print data frames
print(dim(training_data))
print(dim(test_data))

# Fit a linear model
new_class_glm = glm( label ~ . , family="binomial", data = training_data)

#Summary
summary(new_class_glm)

#Prediction based on test data
predict_data = predict(new_class_glm, newdata = test_data, type = "response")

# Confusion matrix on test set
confusionMatrix(test_data$label,predict_data)

#calculate sensitivity: The “true positive rate” 
sensitivity(test_data$label,predict_data)

#calculate specificity: The “true negative rate”
specificity(test_data$label,predict_data)

#calculate total mis-classification error rate
misClassError(test_data$label,predict_data)

## The total misclassification error rate is 41.88% for this model which indicates its not a accurate model since the error rate is on the higher end.
