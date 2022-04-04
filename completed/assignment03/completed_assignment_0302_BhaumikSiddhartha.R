# Assignment: ASSIGNMENT 3.2
# Name: Bhaumik, Siddhartha
# Date: 2022-04-01

## Load the ggplot2 package
library(ggplot2)
library(qqplotr)
library(pastecs)
theme_set(theme_minimal())

## Set the working directory to the root of your DSC 520 directory
setwd("/Users/siddharthabhaumik/Documents/GitHub/dsc520")

## Load the `data/acs-14-1yr-s0201.csv` to
survey_df <- read.csv("data/acs-14-1yr-s0201.csv")


# Get the structure of the data frame
str(survey_df)

# Get summary of data in Data Frame
summary(survey_df)

#Another way for getting Summary/Descriptive statistics
stat.desc(survey_df)

# Display the num of rows in the Data Frame
nrow(survey_df)

# Display the num of columns in the Data Frame
ncol(survey_df)

# Create a Histogram of the HSDegree variable using the ggplot2 package.
# Set a bin size for the Histogram.
# Include a Title and appropriate X/Y axis labels on your Histogram Plot.
hist.HSDegree <- ggplot(survey_df, aes(HSDegree))  + geom_histogram(aes(y = ..density..), colour = "black", fill = "grey", bins = 20) + labs(title = "HSDegree Histogram", x = "Distribution based on HSdegree", y = "Density")

# Display Histogram
print(hist.HSDegree)

# Adding another layer to the above chart, the normal curve
hist.HSDegree + stat_function(fun = dnorm, args = list(mean = mean(survey_df$HSDegree, na.rm = TRUE), sd = sd(survey_df$HSDegree, na.rm = TRUE)), colour = "black", size = 1)


# Probability Plot of the HSDegree variable
# creating random data
random_values = rnorm(survey_df$HSDegree, mean = 90, sd = 50)

# plotting data with proper labels
# And adding line with proper properties
ggplot(mapping = aes(sample = random_values)) + stat_qq_point(size = 2,color = "red") + stat_qq_line(color="green") + labs(title = "HSDegree Probability Plot", x = "Distribution based on HSdegree", y = "Density")


