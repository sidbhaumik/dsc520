# Assignment: ASSIGNMENT 8.2.3
# Name: Bhaumik, Siddhartha
# Date: 2022-05-06

## Load required package
library(ggplot2)
library(pastecs)
library(dplyr)
library(purrr)
library(stringr)

theme_set(theme_minimal())


## Set the working directory to the root of your DSC 520 directory
setwd("/Users/siddharthabhaumik/Documents/GitHub/dsc520")

## Load the `data/week-7-housing.xlsx`
housing_df <- readxl::read_excel("data/week-7-housing.xlsx")

# Get the structure of the data frame
str(housing_df)

# Get summary of data in Data Frame
summary(housing_df)

# Viewing sample data
View(head(housing_df))

## 3.a.i) Any issues in the housing dataset which needs cleanup?
# Yes, I see 'ctyname' column has many missing values like 'NA'.
housing_upd_df <- housing_df %>% filter(`Sale Date` > "1999-12-31") %>% filter(year_built > "2000") %>% filter(`ctyname` != 'NA') 

# checking dimensions
dim(housing_upd_df)
# Viewing sample data
View(head(housing_upd_df))

## 3.b.i) Explain any transformations or modifications you made to the dataset
# I have removed rows with missing 'ctyname' i.e. 'ctyname' as 'NA'. This will make my dataset cleaner as I don't want to process any rows which doesn't have ctyname populated.
# Also, I have excluded rows with 'Sale Date' and 'year built' older than Jan 2000 as I am only interested in looking at the sale data and year built for last 15 years.

## 3.b.ii) Create two variables; one that will contain the variables Sale Price and Square Foot of Lot 
## (same variables used from previous assignment on simple regression) and one that will contain Sale Price and several additional predictors of your choice. 
## Explain the basis for your additional predictor selections.

## Fit a linear model using the `sq_ft_lot` variable as the predictor and `Sale Price` as the outcome
price_per_sq_ft_df <-  lm(`Sale Price` ~ sq_ft_lot, data = housing_upd_df)

price_per_sq_ft_df2 <-  lm(`Sale Price` ~ sq_ft_lot + year_built + square_feet_total_living + zip5 + bedrooms + current_zoning, data = housing_upd_df)

# Few additional predictors which are important for a home buyer and have significant weight on the house price are:
# location i.e. zip5, year_built, number of bedrooms, total living area, total lot square feet and zone.

## 3.b.iii) Execute a summary() function on two variables defined in the previous step to compare the model results. 
## What are the R2 and Adjusted R2 statistics? Explain what these results tell you about the overall model. 
## Did the inclusion of the additional predictors help explain any large variations found in Sale Price?

  summary(price_per_sq_ft_df)
# Multiple R-squared:  0.03711,	Adjusted R-squared:  0.03675 
# R-squared and Adjusted R-squared values are very low indicating that the predictor or independent variable is not capturing the trend in the target variable.

summary(price_per_sq_ft_df2)
# Multiple R-squared:  0.1851,	Adjusted R-squared:  0.1802 
# Addition of more number of predictors significantly increased the R-squared and Adjusted R-squared values which means the predictors are capturing or have a positive effect on the target variable.

## 3.b.iv)   Considering the parameters of the multiple regression model you have created. What are the standardized betas for each parameter and what do the values indicate?

library(lm.beta)

lm.beta(price_per_sq_ft_df)

lm.beta(price_per_sq_ft_df2)

summary(lm.beta(price_per_sq_ft_df2))

  ## 3.b.v)  Calculate the confidence intervals for the parameters in your model and explain what the results indicate.

# Calculate the mean and standard error
l.model1 <- lm( `Sale Price` ~ sq_ft_lot, housing_upd_df)

l.model2 <- lm( `Sale Price` ~ year_built, housing_upd_df)

l.model3 <- lm( `Sale Price` ~ square_feet_total_living, housing_upd_df)

l.model4 <- lm( `Sale Price` ~ zip5, housing_upd_df)

l.model5 <- lm( `Sale Price` ~ bedrooms, housing_upd_df)

l.model6 <- lm( `Sale Price` ~ current_zoning, housing_upd_df)

# Calculate the confidence interval
confint(l.model1, level=0.95)
confint(l.model2, level=0.95)
confint(l.model3, level=0.95)
confint(l.model4, level=0.95)
confint(l.model5, level=0.95)
confint(l.model6, level=0.95)

## 3.b.vi)  Assess the improvement of the new model compared to your original model (simple regression model) by testing 
## whether this change is significant by performing an analysis of variance.
anova(price_per_sq_ft_df,price_per_sq_ft_df2)

## 3.b.vii)  Perform case wise diagnostics to identify outliers and/or influential cases, storing each function's output 
## in a dataframe assigned to a unique variable name.

#price_per_sq_ft_df2 <-  lm(`Sale Price` ~ sq_ft_lot + year_built + square_feet_total_living + zip5 + bedrooms + current_zoning, data = housing_upd_df)
# Finding outliers for all of my predictors

# Predictor sq_ft_lot
summary(housing_upd_df$sq_ft_lot)

sq_ft_outlier_values <- boxplot.stats(housing_upd_df$sq_ft_lot,coef=3)$out  # outlier values.

# Display Outliers
print(sq_ft_outlier_values)

#Plot sq_ft_lot
boxplot(housing_upd_df$sq_ft_lot, main="SQUARE FEET LOT", ylab = "sq_ft_lot")

#Plot sq_ft_lot with outliers
mtext(paste("Outliers: ", paste(sq_ft_outlier_values, collapse=", ")), cex=0.6)

# Predictor year_built
summary(housing_upd_df$year_built)

yr_outlier_values <- boxplot.stats(housing_upd_df$year_built,coef=3)$out  # outlier values.

# Display Outliers
print(yr_outlier_values)

#Plot year_built
boxplot(housing_upd_df$year_built, main="YEAR BUILT", ylab = "year_built")

#Plot year_built with outliers
mtext(paste("Outliers: ", paste(yr_outlier_values, collapse=", ")), cex=0.6)

# Predictor square_feet_total_living
summary(housing_upd_df$square_feet_total_living)

sq_feet_outlier_values <- boxplot.stats(housing_upd_df$square_feet_total_living,coef=3)$out  # outlier values.

# Display Outliers
print(sq_feet_outlier_values)

#Plot square_feet_total_living
boxplot(housing_upd_df$square_feet_total_living, main="square_feet_total_living", ylab = "square_feet_total_living")

#Plot square_feet_total_living with outliers
mtext(paste("Outliers: ", paste(sq_feet_outlier_values, collapse=", ")), cex=0.6)

# Predictor bedrooms
summary(housing_upd_df$bedrooms)

bedr_outlier_values <- boxplot.stats(housing_upd_df$bedrooms,coef=10)$out  # outlier values.

# Display Outliers
print(bedr_outlier_values)
# Too many outliers or there is something wrong,

#Plot bedrooms
boxplot(housing_upd_df$bedrooms, main="BEDROOMS", ylab = "bedrooms")

# Predictor current_zoning
summary(housing_upd_df$current_zoning)

zone_outlier_values <- boxplot.stats(housing_upd_df$current_zoning,coef=3)$out  # outlier values.
# non-numeric argument to binary operator

# Display Outliers
print(zone_outlier_values)
# Too many outliers or there is something wrong,

#Plot current_zoning
boxplot(housing_upd_df$current_zoning, main="current_zoning", ylab = "current_zoning")


## 3.b.viii)  Calculate the standardized residuals using the appropriate command, specifying those that are +-2, 
## storing the results of large residuals in a variable you create.

# Model#1
l.modeli <- lm( `Sale Price` ~ sq_ft_lot, housing_upd_df)

#calculate the standardized residuals
standard_res_modeli <- rstandard(l.modeli)

print(head(standard_res_modeli))

#column bind standardized residuals back to original data frame
final_data_modeli <- cbind(housing_upd_df, standard_res_modeli)

#sort standardized residuals descending
head(final_data_modeli[order(-standard_res_modeli),])

# Model#2
l.modelii <- lm( `Sale Price` ~ year_built, housing_upd_df)

# calculate the standardized residuals
standard_res_modelii <- rstandard(l.modelii)

print(head(standard_res_modelii))

#column bind standardized residuals back to original data frame
final_data_modelii <- cbind(housing_upd_df, standard_res_modelii)

#sort standardized residuals descending
head(final_data_modelii[order(-standard_res_modelii),])

# Model#3
l.modeliii <- lm( `Sale Price` ~ square_feet_total_living, housing_upd_df)

# calculate the standardized residuals
standard_res_modeliii <- rstandard(l.modeliii)

print(head(standard_res_modeliii))

#column bind standardized residuals back to original data frame
final_data_modeliii <- cbind(housing_upd_df, standard_res_modeliii)

#sort standardized residuals descending
head(final_data_modeliii[order(-standard_res_modeliii),])

# Model#4
l.modeliv <- lm( `Sale Price` ~ zip5, housing_upd_df)

# calculate the standardized residuals
standard_res_modeliv <- rstandard(l.modeliv)

print(head(standard_res_modeliv))

#column bind standardized residuals back to original data frame
final_data_modeliv <- cbind(housing_upd_df, standard_res_modeliv)

#sort standardized residuals descending
head(final_data_modeliv[order(-standard_res_modeliv),])

# Model#5
l.modelv <- lm( `Sale Price` ~ bedrooms, housing_upd_df)

# calculate the standardized residuals
standard_res_modelv <- rstandard(l.modelv)

print(head(standard_res_modelv))

#column bind standardized residuals back to original data frame
final_data_modelv <- cbind(housing_upd_df, standard_res_modelv)

#sort standardized residuals descending
head(final_data_modelv[order(-standard_res_modelv),])

# Model#6
l.modelvi <- lm( `Sale Price` ~ current_zoning, housing_upd_df)

# calculate the standardized residuals
standard_res_modelvi <- rstandard(l.modelvi)

print(head(standard_res_modelvi))

#column bind standardized residuals back to original data frame
final_data_modelvi <- cbind(housing_upd_df, standard_res_modelvi)

#sort standardized residuals descending
head(final_data_modelvi[order(-standard_res_modelvi),])

## 3.b.ix)  Use the appropriate function to show the sum of large residuals.

sum(l.model1$residuals^2)
sum(l.model2$residuals^2)
sum(l.model3$residuals^2)
sum(l.model4$residuals^2)
sum(l.model5$residuals^2)
sum(l.model6$residuals^2)

## 3.b.x)  Which specific variables have large residuals (only cases that evaluate as TRUE)?
# All variables have a large residuals but Model4 and Model2 have the highest.

## 3.b.xi)  Investigate further by calculating the leverage, cooks distance, and covariance rations. Comment on all cases that are problematics.
summary(price_per_sq_ft_df2)

#calculate leverage for each observation in the model
hats1 <- as.data.frame(hatvalues(price_per_sq_ft_df2))

#display leverage stats for each observation
hats1

#sort observations by leverage, descending
hats1[order(-hats1['hatvalues(price_per_sq_ft_df2)']), ]
# We can see that the largest leverage value is 1.0. Since this isn’t greater than 2, we know that none of the observations in our dataset have high leverage.

# Cooks distance
plot(cooks.distance(price_per_sq_ft_df2))

# covariance ratio's
cov(housing_upd_df$`Sale Price`,housing_upd_df$sq_ft_lot, method = "pearson")

cov(housing_upd_df$`Sale Price`,housing_upd_df$year_built, method = "pearson")

cov(housing_upd_df$`Sale Price`,housing_upd_df$square_feet_total_living, method = "pearson")

cov(housing_upd_df$`Sale Price`,housing_upd_df$zip5, method = "pearson")


## 3.b.xii)  Perform the necessary calculations to assess the assumption of independence and state if the condition is met or not.

chisq.test(table(housing_upd_df$`Sale Price`,housing_upd_df$sq_ft_lot))

chisq.test(table(housing_upd_df$`Sale Price`,housing_upd_df$year_built))

chisq.test(table(housing_upd_df$`Sale Price`,housing_upd_df$square_feet_total_living))

chisq.test(table(housing_upd_df$`Sale Price`,housing_upd_df$bedrooms))

chisq.test(table(housing_upd_df$`Sale Price`,housing_upd_df$zip5))
# Since "Chi-squared approximation may be incorrect” appears, it means that the smallest expected frequencies is lower than 5. 

## 3.b.xiii)  Perform the necessary calculations to assess the assumption of no multicollinearity and state if the condition is met or not.
library(tidyverse)
library(corrplot)
library(car)

housing_cor_df <- housing_upd_df %>% select(`Sale Price` ,sq_ft_lot, year_built,bedrooms,square_feet_total_living, zip5 )

corrplot(cor(housing_cor_df), method = "number", type = "upper", diag = FALSE)

## 3.b.xiv)  Visually check the assumptions related to the residuals using the plot() and hist() functions. 
## Summarize what each graph is informing you of and if any anomalies are present.

#plot predictor variable1 vs. standardized residuals
plot(final_data_modeli$`Sale Price`, standard_res_modeli, ylab='Standardized Residuals', xlab='Sale Price') 
#add horizontal line at 0
abline(0, 0)

#Using hist() function
hist(final_data_modeli$`Sale Price`, col = "red", xlab = "Sale Price")
hist(standard_res_modeli,col= "blue", add = TRUE)

#plot predictor variable2 vs. standardized residuals
plot(final_data_modelii$`Sale Price`, standard_res_modelii, ylab='Standardized Residuals', xlab='Sale Price') 
#add horizontal line at 0
abline(0, 0)

#Using hist() function
hist(final_data_modelii$`Sale Price`, col = "red", xlab = "Sale Price")
hist(standard_res_modelii,col= "blue", add = TRUE)

#plot predictor variable3 vs. standardized residuals
plot(final_data_modeliii$`Sale Price`, standard_res_modeliii, ylab='Standardized Residuals', xlab='Sale Price') 
#add horizontal line at 0
abline(0, 0)

#Using hist() function
hist(final_data_modeliii$`Sale Price`, col = "red", xlab = "Sale Price")
hist(standard_res_modeliii,col= "blue", add = TRUE)

#plot predictor variable4 vs. standardized residuals
plot(final_data_modeliv$`Sale Price`, standard_res_modeliv, ylab='Standardized Residuals', xlab='Sale Price') 
#add horizontal line at 0
abline(0, 0)

#Using hist() function
hist(final_data_modeliv$`Sale Price`, col = "red", xlab = "Sale Price")
hist(standard_res_modeliv,col= "blue", add = TRUE)

#plot predictor variable5 vs. standardized residuals
plot(final_data_modelv$`Sale Price`, standard_res_modelv, ylab='Standardized Residuals', xlab='Sale Price') 
#add horizontal line at 0
abline(0, 0)

#Using hist() function
hist(final_data_modelv$`Sale Price`, col = "red", xlab = "Sale Price")
hist(standard_res_modelv,col= "blue", add = TRUE)

#plot predictor variable6 vs. standardized residuals
plot(final_data_modelvi$`Sale Price`, standard_res_modelvi, ylab='Standardized Residuals', xlab='Sale Price') 
#add horizontal line at 0
abline(0, 0)

#Using hist() function
hist(final_data_modelvi$`Sale Price`, col = "red", xlab = "Sale Price")
hist(standard_res_modelvi,col= "blue", add = TRUE)

## 3.b.xv)  Overall, is this regression model unbiased? 
## If an unbiased regression model, what does this tell us about the sample vs. the entire population model?
# Yes, the model seems to be unbiased based on the estimated values but different types of sample population needed to make a conclusion.







