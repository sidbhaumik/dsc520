# Assignment: ASSIGNMENT 4.2.1
# Name: Bhaumik, Siddhartha
# Date: 2022-04-07

## Load the ggplot2 package
library(ggplot2)
library(qqplotr)
library(pastecs)
library(dplyr)

theme_set(theme_minimal())


## Set the working directory to the root of your DSC 520 directory
setwd("/Users/siddharthabhaumik/Documents/GitHub/dsc520")

## Load the `data/scores.csv` to
test_score_df <- read.csv("data/scores.csv")

# Get the structure of the data frame
str(test_score_df)

# Get summary of data in Data Frame
summary(test_score_df)

# Q1: What are the observational units in this study?
# Answer: There are 3 variables named Count, Score & Section and total of 38 observations.

# Q2: Identify the variables mentioned in the narrative paragraph and 
#     determine which are categorical and quantitative?        
# Answer: 3 variables: Count, Score, Section.
#         Categorical: Count & Section variable
#         Quantitative: Score variable

# Q3: Create one variable to hold a subset of your data set that contains only the Regular Section 
#     and one variable for the Sports Section.
# Answer: 'sport_sec_df' variable for Sports Section data
#         'regular_sec_df' variable for Regular Section data

# Data frame with Only Sports Section
sport_sec_df <- select(filter(test_score_df, Section == 'Sports'), c(Count, Score, Section))

print(sport_sec_df)

# Data frame with Only Regular Section
regular_sec_df <- select(filter(test_score_df, Section != 'Sports'), c(Count, Score, Section))

print(regular_sec_df)


# Summarizing the Sport section data frame
# Calculating mean of Score
summarise(sport_sec_df, mean = mean(Score))
# Mean test score is 307.36

# Calculating min of Score
summarise(sport_sec_df, min = min(Score))
# Minimum test score is 200

# Calculating max of Score
summarise(sport_sec_df, max = max(Score))
# Maximum test score is 395

# Calculating median of Score
summarise(sport_sec_df, med = median(Score))
# Median test score is 315

# Summarizing the Regular section data frame
# Calculating mean of Score
summarise(regular_sec_df, mean = mean(Score))
# Mean test score is 327.63

# Calculating min of Score
summarise(regular_sec_df, min = min(Score))
# Minimum test score is 265

# Calculating max of Score
summarise(regular_sec_df, max = max(Score))
# Maximum test score is 380

# Calculating median of Score
summarise(regular_sec_df, med = median(Score))
# Median test score is 325


# Q4: Use the Plot function to plot each Sections scores and the number of students achieving that score. 
# Answer: Please see below plots: sport_plot, regular_plot

# One row, two columns
par(mfrow = c(1, 2))

# The following two plots will be combined
#Sport Section Plot
sport_plot <- plot(sport_sec_df$Count,sport_sec_df$Score, col="red", type = "p", main = "Sport Student Scores Plot", xlab = "Num of Students", ylab = "Section Score", lwd = 3)  

#Regular Section Plot
regular_plot <- plot(regular_sec_df$Count,regular_sec_df$Score, col="blue", type = "p", main = "Regular Student Scores Plot", xlab = "Num of Students", ylab = "Section Score", lwd = 3)  

# Back to the original graphics device
par(mfrow = c(1, 1))

# Q4:a) Can you say that one section tended to score more points than the other? 
# Answer: Total points scored is same between 2 sections. 
#         But,Regular section scored more 20 points compared to Sports section. Even though Sports Section scored more 30 points but the difference is of only 1 student.

# Q4:b) Did every student in one section score more points than every student in the other section?
# Answer: Looking at the mean and median values, it seems students from Regular section did better than Sports section.

# Q4:c) What could be one additional variable that was not mentioned in the narrative that could be influencing the point distributions between the two sections?
# Answer: I felt the 'scores.csv' dataset was not well defined. The Count column name is misleading and until I re-read the question, I was thinking it holds the number of students.




