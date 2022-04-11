# Assignment: ASSIGNMENT 4.2.2
# Name: Bhaumik, Siddhartha
# Date: 2022-04-08

## Load required package
library(ggplot2)
library(qqplotr)
library(pastecs)
library(plyr)


theme_set(theme_minimal())


## Set the working directory to the root of your DSC 520 directory
setwd("/Users/siddharthabhaumik/Documents/GitHub/dsc520")

## Load the `data/week-7-housing.xlsx`
housing_df <- readxl::read_excel("data/week-7-housing.xlsx")

# Get the structure of the data frame
str(housing_df)

# Get summary of data in Data Frame
summary(housing_df)

# perform the following data transformations:
# Q1: Use the apply function on a variable in your dataset

apl_sls_price_df <- apply(housing_df[c('Sale Price')],2, median)

print(apl_sls_price_df)

# Q2: Use the aggregate function on a variable in your dataset
aggregate.data.frame(housing_df$`Sale Price`, by = list(housing_df$`Sale Date`), FUN = mean)

# Q3: Use the plyr function on a variable in your dataset – more specifically, 
#     I want to see you split some data, perform a modification to the data, 
#     and then bring it back together

ddply(housing_df, .(postalctyn, year_built), summarise, mean.sq_ft_lot = mean(sq_ft_lot))

ddply(housing_df, .(zip5, year_built), transform, max.square_feet_total_living = max(square_feet_total_living))

# Q4: Check distributions of the data

random_values = rnorm(housing_df$zip5, mean = 90, sd = 50)

ggplot(mapping = aes(sample = random_values)) + stat_qq_point(size = 2,color = "red") + stat_qq_line(color="green") + labs(title = "Housing Data Probability Plot", x = "Distribution based on Zip Code", y = "Density")

# Q5: Identify if there are any outliers

summary(housing_df)

# I see cityname column has missing values.


# Q6: Create at least 2 new variables

city_df <- select(filter(housing_df, ctyname == 'REDMOND'), c('Sale Date', 'Sale Price', 'ctyname', 'year_built'))

print(city_df)

lot_size_df <- select(filter(housing_df, ctyname == 'REDMOND', sq_ft_lot > 5000 ), c('Sale Date', 'Sale Price', 'ctyname', 'year_built', 'sq_ft_lot'))

print(lot_size_df)






