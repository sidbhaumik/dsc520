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


#Answer the following questions based on the Histogram produced:
#  Based on what you see in this histogram, is the data distribution unimodal?
# Answer: Yes, it's unimodel since it has one clear peak.The values increase at first, rising to a single peak after which it then decrease. 

#  Is it approximately symmetrical?
# Answer: No, it's not symmetrical since the data is skewed towards left.

#  Is it approximately bell-shaped?
# Answer: No, its not bell-shaped.
    
# Is it approximately normal?
# Answer: It's not normal.

#  If not normal, is the distribution skewed? If so, in which direction?
# Answer: The data distribution is skewed towards the left.

#  Include a normal curve to the Histogram that you plotted.
# Adding another layer to the above chart, the normal curve
hist.HSDegree + stat_function(fun = dnorm, args = list(mean = mean(survey_df$HSDegree, na.rm = TRUE), sd = sd(survey_df$HSDegree, na.rm = TRUE)), colour = "black", size = 1)
  
# Explain whether a normal distribution can accurately be used as a model for this data.

# Probability Plot of the HSDegree variable
# creating random data
random_values = rnorm(survey_df$HSDegree, mean = 90, sd = 50)

# plotting data with proper labels
# And adding line with proper properties
ggplot(mapping = aes(sample = random_values)) + stat_qq_point(size = 2,color = "red") + stat_qq_line(color="green") + labs(title = "HSDegree Probability Plot", x = "Distribution based on HSdegree", y = "Density")

# Answer the following questions based on the Probability Plot:
#  Based on what you see in this probability plot, is the distribution approximately normal? Explain how you know.
# Answer: Yes, it's approximately normal since it resides in a shape like straight line.

# If not normal, is the distribution skewed? If so, in which direction? Explain how you know.
# Answer: No, most of the data points resides in a straight line. So, its not skewed.

# Now that you have looked at this data visually for normality, you will now quantify normality with numbers using the stat.desc() function.
stat.desc(survey_df)

# In several sentences provide an explanation of the result produced for skew, kurtosis, and z-scores.
# Answer: The skewness, kurtosis and z-scores in the result set is due to the sample population with uneven distribution between different US states and counties. 
# Like I see lot of counties covered for California, New Jersey and New York but that's not the case for rest of the states. 
