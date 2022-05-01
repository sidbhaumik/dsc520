# Assignment: ASSIGNMENT 7.2
# Name: Bhaumik, Siddhartha
# Date: 2022-04-29

library(ppcor)

## Set the working directory to the root of your DSC 520 directory
setwd("/Users/siddharthabhaumik/Documents/GitHub/dsc520")

## Load the `data/student-survey.csv` to
stud_survey_df <- read.csv("data/student-survey.csv")

print(stud_survey_df)

# Get the structure of the data frame
str(stud_survey_df)

#1. Use R to calculate the covariance of the Survey variables and 
#   provide an explanation of why you would use this calculation and what the results indicate.

## Using `cov()` compute covariance for
## Time Reading vs. Time TV
cov(stud_survey_df$TimeReading,stud_survey_df$TimeTV,method = "pearson")

cov(stud_survey_df$TimeReading,stud_survey_df$TimeTV,method = "spearman")

## Time Reading vs. Happiness
cov(stud_survey_df$TimeReading,stud_survey_df$Happiness, method = "pearson")

## Time TV vs. Happiness
cov(stud_survey_df$TimeTV,stud_survey_df$Happiness, method = "pearson")

## Time Reading vs. Gender
cov(stud_survey_df$TimeReading,stud_survey_df$Gender, method = "pearson")

## Time TV vs. Gender
cov(stud_survey_df$TimeTV,stud_survey_df$Gender, method = "pearson")

## compute covariance for entire dataset
cov(stud_survey_df)

# Answer: From the results, its clear that their is negative relationship between time spent reading when compared
#         with happiness and gender. 
#         For time spend watching TV, it shows a high positive relationship with happiness.

#2. Examine the Survey data variables. What measurement is being used for the variables? 
#   Explain what effect changing the measurement being used for the variables would have on the covariance calculation. 
#   Would this be a problem? Explain and provide a better alternative if needed.

print(stud_survey_df)

# Get the structure of the data frame
str(stud_survey_df)

# Get the dataframe summary
summary(stud_survey_df)

# Answer: The student survey data has 4 variables and 11 observations.
# 3 variables are of datatype Integer and one variable(Happiness) of datatype Number. Also, Happiness variable has fractional data
# upto 2 decimal places.
# By looking at the data and summary, I can see the min and max values for each variables but many key information are missing like
# measurement frequency for time reading, time tv, happiness. Is it daily stats, weekly, monthly, yearly, etc. No such information
# is provided. Also, from the data it seems time reading is captured in hours whereas time TV in minutes.Both are whole numbers.
# For Happiness, from the data it seems to be in percentage but not clear. Same goes for gender, we don't know what 0 and 1 represents.
# So, knowing the correct measurement scale/frequency will change the covariance results as well.

#3. Choose the type of correlation test to perform, explain why you chose this test, 
#   and make a prediction if the test yields a positive or negative correlation?

cor(stud_survey_df,method = "pearson")
cor(stud_survey_df,method = "spearman")
cor(stud_survey_df,method = "kendall")

# All 3 correlation methods gives similar results from negative and positive relationship perspective but I will go with
# Pearson method since its most widely used and relationship between variables(happiness vs timereading & timetv) seems to be linear.

#4. Perform a correlation analysis of:
#   i.   All variables
#   ii.  A single correlation between two a pair of the variables

## Using `cor()` compute correlation for
## Time Reading vs. Time TV
cor(stud_survey_df$TimeReading,stud_survey_df$TimeTV,method = "pearson")

cor(stud_survey_df$TimeReading,stud_survey_df$TimeTV,method = "spearman")

## Time Reading vs. Happiness
cor(stud_survey_df$TimeReading,stud_survey_df$Happiness, method = "pearson")

## Time TV vs. Happiness
cor(stud_survey_df$TimeTV,stud_survey_df$Happiness, method = "pearson")

## Time Reading vs. Gender
cor(stud_survey_df$TimeReading,stud_survey_df$Gender, method = "pearson")

## Time TV vs. Gender
cor(stud_survey_df$TimeTV,stud_survey_df$Gender, method = "pearson")

## compute covariance for entire dataset
cor(stud_survey_df)

#   iii. Repeat your correlation test in step 2 but set the confidence interval at 99%

## Time Reading vs. Happiness
cor.test(stud_survey_df$TimeReading,stud_survey_df$Happiness, method = "pearson",conf.level = .99)

## Time TV vs. Happiness
cor.test(stud_survey_df$TimeTV,stud_survey_df$Happiness, method = "pearson",conf.level = .99)

#   iv.  Describe what the calculations in the correlation matrix suggest about the relationship between the variables. 
#        Be specific with your explanation.

# Answer: Based on the results, I can see happiness and time reading has negative relationship whereas happiness and time watching
# TV has positive relationship.

#5. Calculate the correlation coefficient and the coefficient of determination, describe what you conclude about the results.

cor_timeread_df <- cor(stud_survey_df$TimeReading,stud_survey_df$Happiness) ^ 2

print(cor_timeread_df)

cor_timetv_df <- cor(stud_survey_df$TimeTV,stud_survey_df$Happiness) ^ 2

print(cor_timetv_df)

# Answer: The r-squared value for both cases are closer to 0. It means the variables selected doesnt have much correlation.

#6. Based on your analysis can you say that watching more TV caused students to read less? Explain.

## Time Reading vs. Time TV
cor_time_rd_tv_df <- cor(stud_survey_df$TimeReading,stud_survey_df$TimeTV,method = "pearson") ^ 2

print(cor_time_rd_tv_df)

# Answer: The r-squared value is closer to 1. It means the variables selected has a correlation and we can say that watching more TV caused students to read less.

#7. Pick three variables and perform a partial correlation, documenting which variable you are “controlling”. 
#   Explain how this changes your interpretation and explanation of the results.

pcor.test(stud_survey_df$TimeTV,stud_survey_df$Happiness, stud_survey_df$Gender)

# Answer: Based on initial correlation results, time spend watching TV shows higher correlation with Happiness when compared to time reading.
# But, now adding 3rd variable "gender" has changed the partial correlation between TimeTV & Happiness and has increased it
# further from 0.40 to 0.64.



