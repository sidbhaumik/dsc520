# Assignment: ASSIGNMENT 11.2.1
# Name: Bhaumik, Siddhartha
# Date: 2022-05-28

# Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("class")
install.packages("tidymodels")
install.packages("gridExtra")
install.packages("kknn")

# Loading package
library(e1071)
library(caTools)
library(class)
library(tidyverse)
library(tidymodels)
library(gridExtra)
library(plyr)
library(ggplot2)
library(kknn)

set.seed(123)

## Set the working directory to the root of your DSC 520 directory
setwd("/Users/siddharthabhaumik/Documents/GitHub/dsc520")

## Load the `data/binary-classifier-data` to
bin_class_df <- read.csv("data/binary-classifier-data.csv")

## Viewing Sample data
head(bin_class_df)

## summary
summary(bin_class_df)

## Check Data Structure of the object
str(bin_class_df)

## Load the `data/trinary-classifier-data` to
tri_class_df <- read.csv("data/trinary-classifier-data.csv")

## Viewing Sample data
head(tri_class_df)

## summary
summary(tri_class_df)

## Check Data Structure of the object
str(tri_class_df)

# Plot the data from each dataset using a scatter plot.

## Since we have more than two variables and we want to find the correlation 
## between one variable versus the remaining ones,I use the scatterplot matrix. 

pairs(~label+x+y, data = bin_class_df, main = "Scatterplot Matrix for Binary Classifier")

pairs(~label+x+y, data = tri_class_df, main = "Scatterplot Matrix for Trinary Classifier")

## Simple plot for binary classifier
with(bin_class_df,{plot(x,y,col=label)})

## Simple plot for trinary classifier
with(tri_class_df,{plot(x,y,col=label)})

# Splitting binary classifier data into train and test data
split <- sample.split(bin_class_df, SplitRatio = 0.7)
train_bin_class <- subset(bin_class_df, split == "TRUE")
test_bin_class <- subset(bin_class_df, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_bin_class[, 1:3])
test_scale <- scale(test_bin_class[, 1:3])

# Fitting KNN Model for binary_classifier dataset
bin_class_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_bin_class$label,
                      k = 1)
bin_class_knn


# Confusion Matrix
bin_class_cm <- table(test_bin_class$label, bin_class_knn)

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(bin_class_knn != test_bin_class$label)
print(paste('Accuracy =', 1-misClassError))

# K = 3
bin_class_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_bin_class$label,
                      k = 3)
misClassError <- mean(bin_class_knn != test_bin_class$label)
print(paste('Accuracy =', 1-misClassError))

test_accuracy <- paste('Accuracy =', 1-misClassError)

# K = 5
bin_class_knn <- knn(train = train_scale,
                     test = test_scale,
                     cl = train_bin_class$label,
                     k = 5)
misClassError <- mean(bin_class_knn != test_bin_class$label)
print(paste('Accuracy =', 1-misClassError))

# K = 10
bin_class_knn <- knn(train = train_scale,
                     test = test_scale,
                     cl = train_bin_class$label,
                     k = 10)
misClassError <- mean(bin_class_knn != test_bin_class$label)
print(paste('Accuracy =', 1-misClassError))

# K = 15
bin_class_knn <- knn(train = train_scale,
                     test = test_scale,
                     cl = train_bin_class$label,
                     k = 15)
misClassError <- mean(bin_class_knn != test_bin_class$label)
print(paste('Accuracy =', 1-misClassError))

# K = 20
bin_class_knn <- knn(train = train_scale,
                     test = test_scale,
                     cl = train_bin_class$label,
                     k = 20)
misClassError <- mean(bin_class_knn != test_bin_class$label)
print(paste('Accuracy =', 1-misClassError))

# K = 25
bin_class_knn <- knn(train = train_scale,
                     test = test_scale,
                     cl = train_bin_class$label,
                     k = 25)
misClassError <- mean(bin_class_knn != test_bin_class$label)
print(paste('Accuracy =', 1-misClassError))

# Splitting Trinary classifier data into train and test data
split <- sample.split(tri_class_df, SplitRatio = 0.7)
train_tri_class <- subset(tri_class_df, split == "TRUE")
test_tri_class <- subset(tri_class_df, split == "FALSE")

# Feature Scaling
train_tri_scale <- scale(train_tri_class[, 1:3])
test_tri_scale <- scale(test_tri_class[, 1:3])

# Fitting KNN Model for trinary_classifier dataset
tri_class_knn <- knn(train = train_tri_scale,
                     test = test_tri_scale,
                     cl = train_tri_class$label,
                     k = 1)
tri_class_knn


# Confusion Matrix
tri_class_cm <- table(test_tri_class$label, tri_class_knn)

tri_class_cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(tri_class_knn != test_tri_class$label)
print(paste('Accuracy =', 1-misClassError))

# K = 3
tri_class_knn <- knn(train = train_tri_scale,
                     test = test_tri_scale,
                     cl = train_tri_class$label,
                     k = 3)
misClassError <- mean(tri_class_knn != test_tri_class$label)
print(paste('Accuracy =', 1-misClassError))

# K = 5
tri_class_knn <- knn(train = train_tri_scale,
                     test = test_tri_scale,
                     cl = train_tri_class$label,
                     k = 5)
misClassError <- mean(tri_class_knn != test_tri_class$label)
print(paste('Accuracy =', 1-misClassError))

# K = 10
tri_class_knn <- knn(train = train_tri_scale,
                     test = test_tri_scale,
                     cl = train_tri_class$label,
                     k = 10)
misClassError <- mean(tri_class_knn != test_tri_class$label)
print(paste('Accuracy =', 1-misClassError))

# K = 15
tri_class_knn <- knn(train = train_tri_scale,
                     test = test_tri_scale,
                     cl = train_tri_class$label,
                     k = 15)
misClassError <- mean(tri_class_knn != test_tri_class$label)
print(paste('Accuracy =', 1-misClassError))

# K = 20
tri_class_knn <- knn(train = train_tri_scale,
                     test = test_tri_scale,
                     cl = train_tri_class$label,
                     k = 20)
misClassError <- mean(tri_class_knn != test_tri_class$label)
print(paste('Accuracy =', 1-misClassError))

# K = 25
tri_class_knn <- knn(train = train_tri_scale,
                     test = test_tri_scale,
                     cl = train_tri_class$label,
                     k = 25)
misClassError <- mean(tri_class_knn != test_tri_class$label)
print(paste('Accuracy =', 1-misClassError))

# Looking back at the plots of the data, do you think a linear classifier would work well on these datasets?

## No, it doesn't look like from the results. The current model accuracy is 100% 
## whereas for linear model (done previous week) it was merely 41% accuracy.
## Logistic regression goes very well on high dimensional data sets with a lot of training points, 
## but if your data is not linearly separable the algorithm won't work well.

# How does the accuracy of your logistic regression classifier from last week compare?  Why is the accuracy different between these two methods?
## There is a big difference in accuracy between both methods. 
## The current method gives 100% accuracy whereas last weeks gives only 41% accuracy.