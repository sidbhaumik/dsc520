# Assignment: ASSIGNMENT 5.2
# Name: Bhaumik, Siddhartha
# Date: 2022-04-15

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

# Using the dplyr package, use the 6 different operations to analyze/transform the data
# - GroupBy, Summarize, Mutate, Filter, Select, and Arrange
housing_df %>% filter(`Sale Price` > "700000")

housing_df %>% filter(`Sale Price` > "700000") %>% select(`Sale Date` , `Sale Price` ,ctyname, bedrooms, year_built)
  
housing_df %>% mutate(`Sale Price`,min_down_pay = `Sale Price`/ 5) %>% select(`Sale Date` , `Sale Price` ,min_down_pay, ctyname, bedrooms, year_built)


housing_df %>% mutate(`Sale Price`,min_down_pay = `Sale Price`/ 5) %>% select(`Sale Date` , `Sale Price` ,min_down_pay, ctyname, bedrooms, year_built) %>% arrange(desc(year_built))

housing_df %>% group_by(year_built) %>% select(`Sale Date` , `Sale Price` , ctyname, bedrooms, year_built)

housing_df %>% group_by(year_built) %>% summarise(n = n(), prices = mean(`Sale Price`, na.rm = TRUE)) %>% arrange(desc(year_built))

# Using the purrr package – perform 2 functions on your dataset.
# You could use zip_n, keep, discard, compact, etc.
  
keep(housing_df$bedrooms, function(x) x > 5)

discard(housing_df$ctyname, is.na)

# Use the cbind and rbind function on your dataset
high_price_df1 <- housing_df %>% filter(`Sale Price` > "700000") %>% select(`Sale Date` , `Sale Price` ,ctyname, bedrooms, year_built)

print(high_price_df1)

high_price_df2 <- housing_df %>% filter(`Sale Price` > "700000") %>% select(square_feet_total_living,sq_ft_lot)

print(high_price_df2)

# cbind function
cbinded_df <- cbind(high_price_df1, high_price_df2)

print(cbinded_df)

med_price_df3 <- housing_df %>% filter(`Sale Price` == "650000") %>% select(`Sale Date` , `Sale Price` ,ctyname, bedrooms, year_built)

print(med_price_df3)

# rbind function
rbinded_df <- rbind(high_price_df1,med_price_df3)

print(rbinded_df)

# Split a string, then concatenate the results back together

str_split(cbinded_df$ctyname, "NA", n = 2, simplify = TRUE)

# Creating new dataset for string split function

mbl_dvce_df <- c("apple iphone pro12 2019","apple iphone se13 2021",
                 "samsung galaxy s12 2021")

print(mbl_dvce_df)

#string split
mbl_dvce_splt_df <- str_split(mbl_dvce_df, " ", simplify = TRUE)

print(mbl_dvce_splt_df)

# string concatenate
paste(mbl_dvce_splt_df, collapse = " ")







