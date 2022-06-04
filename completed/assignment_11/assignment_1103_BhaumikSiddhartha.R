# Assignment: ASSIGNMENT 11.2.2
# Name: Bhaumik, Siddhartha
# Date: 2022-05-29

# Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("class")
install.packages("tidymodels")
install.packages("gridExtra")
install.packages("kknn")
install.packages("factoextra")
install.packages("cluster")

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
library(factoextra)  # clustering algorithms & visualization
library(cluster)  # clustering algorithms

set.seed(12)

## Set the working directory to the root of your DSC 520 directory
setwd("/Users/siddharthabhaumik/Documents/GitHub/dsc520")

## Load the `data/clustering-data.csv` to
clstr_df <- read.csv("data/clustering-data.csv")

## Viewing Sample data
head(clstr_df)

## summary
summary(clstr_df)

## Check Data Structure of the object
str(clstr_df)


# Plot the data from dataset using a scatter plot.

plot(clstr_df$x,clstr_df$y,main = "DS Cluster Data", xlab = "X", ylab = "Y")


#remove rows with missing values
clstr_upd_df <- na.omit(clstr_df)

#scale each variable to have a mean of 0 and sd of 1
clstr_upd_df <- scale(clstr_upd_df)

head(clstr_upd_df)


#DETERMINE HOW MANY CLUSTERS IS OPTIMAL
#plot number of clusters vs. total within sum of squares
fviz_nbclust(clstr_upd_df, kmeans, method = "wss")

# For this plot it appear that there is a bit of an elbow or “bend” at k = 4 clusters.

# Perform K-Means Clustering with Optimal K(i.e.with k = 4 clusters)
# nstart = 25 will generate 25 initial configurations. This approach is often recommended.

clstr_km <- kmeans(clstr_upd_df, centers = 4, nstart = 25)

#view results
head(clstr_km)

# Cluster center
clstr_km$centers

#plot results of final k-means model
fviz_cluster(clstr_km, geom = "point",data = clstr_upd_df) + ggtitle("k = 4")

#add cluster assignment to original data
final_data <- cbind(clstr_upd_df, cluster = clstr_km$cluster)

#view final data
head(final_data)

#find means of each cluster
aggregate(clstr_upd_df, by=list(cluster=clstr_km$cluster), mean)

# Perform K-Means Clustering with other K value 2

clstr_km <- kmeans(clstr_upd_df, centers = 2, nstart = 25)

#view results
head(clstr_km)

# Cluster center
clstr_km$centers

#plot results of final k-means model
fviz_cluster(clstr_km, geom = "point",data = clstr_upd_df) + ggtitle("k = 2")

#add cluster assignment to original data
final_data <- cbind(clstr_upd_df, cluster = clstr_km$cluster)

#view final data
head(final_data)

#find means of each cluster
aggregate(clstr_upd_df, by=list(cluster=clstr_km$cluster), mean)

# Perform K-Means Clustering with other K value 3

clstr_km <- kmeans(clstr_upd_df, centers = 3, nstart = 25)

#view results
head(clstr_km)

# Cluster center
clstr_km$centers

#plot results of final k-means model
fviz_cluster(clstr_km, geom = "point",data = clstr_upd_df) + ggtitle("k = 3")

#add cluster assignment to original data
final_data <- cbind(clstr_upd_df, cluster = clstr_km$cluster)

#view final data
head(final_data)

#find means of each cluster
aggregate(clstr_upd_df, by=list(cluster=clstr_km$cluster), mean)

# Perform K-Means Clustering with other K value 5

clstr_km <- kmeans(clstr_upd_df, centers = 5, nstart = 25)

#view results
head(clstr_km)

# Cluster center
clstr_km$centers

#plot results of final k-means model
fviz_cluster(clstr_km, geom = "point",data = clstr_upd_df) + ggtitle("k = 5")

#add cluster assignment to original data
final_data <- cbind(clstr_upd_df, cluster = clstr_km$cluster)

#view final data
head(final_data)

#find means of each cluster
aggregate(clstr_upd_df, by=list(cluster=clstr_km$cluster), mean)

# Perform K-Means Clustering with other K value 6

clstr_km <- kmeans(clstr_upd_df, centers = 6, nstart = 25)

#view results
head(clstr_km)

# Cluster center
clstr_km$centers

#plot results of final k-means model
fviz_cluster(clstr_km, geom = "point",data = clstr_upd_df) + ggtitle("k = 6")

#add cluster assignment to original data
final_data <- cbind(clstr_upd_df, cluster = clstr_km$cluster)

#view final data
head(final_data)

#find means of each cluster
aggregate(clstr_upd_df, by=list(cluster=clstr_km$cluster), mean)

# Perform K-Means Clustering with other K value 7

clstr_km <- kmeans(clstr_upd_df, centers = 7, nstart = 25)

#view results
head(clstr_km)

# Cluster center
clstr_km$centers

#plot results of final k-means model
fviz_cluster(clstr_km, geom = "point",data = clstr_upd_df) + ggtitle("k = 7")

#add cluster assignment to original data
final_data <- cbind(clstr_upd_df, cluster = clstr_km$cluster)

#view final data
head(final_data)

#find means of each cluster
aggregate(clstr_upd_df, by=list(cluster=clstr_km$cluster), mean)

# Perform K-Means Clustering with other K value 8

clstr_km <- kmeans(clstr_upd_df, centers = 8, nstart = 25)

#view results
head(clstr_km)

# Cluster center
clstr_km$centers

#plot results of final k-means model
fviz_cluster(clstr_km, geom = "point",data = clstr_upd_df) + ggtitle("k = 8")

#add cluster assignment to original data
final_data <- cbind(clstr_upd_df, cluster = clstr_km$cluster)

#view final data
head(final_data)


#find means of each cluster
aggregate(clstr_upd_df, by=list(cluster=clstr_km$cluster), mean)

# Perform K-Means Clustering with other K value 9

clstr_km <- kmeans(clstr_upd_df, centers = 9, nstart = 25)

#view results
head(clstr_km)

# Cluster center
clstr_km$centers

#plot results of final k-means model
fviz_cluster(clstr_km, geom = "point",data = clstr_upd_df) + ggtitle("k = 9")

#add cluster assignment to original data
final_data <- cbind(clstr_upd_df, cluster = clstr_km$cluster)

#view final data
head(final_data)

#find means of each cluster
aggregate(clstr_upd_df, by=list(cluster=clstr_km$cluster), mean)

# Perform K-Means Clustering with other K value 10

clstr_km <- kmeans(clstr_upd_df, centers = 10, nstart = 25)

#view results
head(clstr_km)

# Cluster center
clstr_km$centers

#plot results of final k-means model
fviz_cluster(clstr_km, geom = "point",data = clstr_upd_df) + ggtitle("k = 10")

#add cluster assignment to original data
final_data <- cbind(clstr_upd_df, cluster = clstr_km$cluster)

#view final data
head(final_data)

#find means of each cluster
aggregate(clstr_upd_df, by=list(cluster=clstr_km$cluster), mean)

# Perform K-Means Clustering with other K value 11

clstr_km <- kmeans(clstr_upd_df, centers = 11, nstart = 25)

#view results
head(clstr_km)

# Cluster center
clstr_km$centers

#plot results of final k-means model
fviz_cluster(clstr_km, geom = "point",data = clstr_upd_df) + ggtitle("k = 11")

#add cluster assignment to original data
final_data <- cbind(clstr_upd_df, cluster = clstr_km$cluster)

#view final data
head(final_data)

#find means of each cluster
aggregate(clstr_upd_df, by=list(cluster=clstr_km$cluster), mean)

# Perform K-Means Clustering with other K value 12

clstr_km <- kmeans(clstr_upd_df, centers = 12, nstart = 25)

#view results
head(clstr_km)

# Cluster center
clstr_km$centers

#plot results of final k-means model
fviz_cluster(clstr_km, geom = "point",data = clstr_upd_df) + ggtitle("k = 12")

#add cluster assignment to original data
final_data <- cbind(clstr_upd_df, cluster = clstr_km$cluster)

#view final data
head(final_data)

#find means of each cluster
aggregate(clstr_upd_df, by=list(cluster=clstr_km$cluster), mean)




