
## Load required package
library(ggplot2)
library(pastecs)
library(dplyr)
library(purrr)
library(stringr)
library(factoextra)  # clustering algorithms & visualization
library(cluster)  # clustering algorithms
theme_set(theme_minimal())

# Q1 Data importing and cleaning steps are explained in the text and follow a logical process.Outline your data preparation and cleansing steps.

## Set the working directory to the root of your DSC 520 directory
setwd("/Users/siddharthabhaumik/Documents/GitHub/dsc520")

## Load 'Any Mental Illness in the Past Year Data' from 51 US states to
state_any_mental_df <- read.csv("map_data.csv")

## Viewing Sample data from 'Any mental illness/past year' dataset
head((state_any_mental_df))

## Load 'Major Depressive Episode in the Past Year Data' from 51 US states to
state_dep_mental_df <- read.csv("data/map_data_rcrd.csv")

## Viewing Sample data from 'Major depressive episode/past year' dataset
head((state_dep_mental_df))

## Load 'Received Mental health services in the Past Year Data' from 51 US states to
state_rcvd_mental_df <- read.csv("data/map_data_dep.csv")

## Viewing Sample data from 'Received Mental health services/past year' dataset
head((state_rcvd_mental_df))

## Its a clean dataset with relevant information from each US state. So, Nothing to filter out.

## Load the 'COVID-19 and Mental Health Search Terms data'  to
covid19_world_df <- readxl::read_excel("data/search_term_worldwide.xlsx")

covid19_us_df <- readxl::read_excel("data/search_term_us.xlsx")
## The search interest of mental health related terms on Google before and after the outbreak of COVID-19 pandemic reveals how public's concern is affected by the pandemic, 
## and its impact to mental health of people around the world.
## The mental health related search terms are "mental health", "depression", "anxiety", "ocd", "obsessive compulsive disorder", "insomnia", "panic attack", "counseling", "psychiatrist".
## Search interest is indicated by a number between 0 and 100, where 100 means the most popular point of time(by week), 1 means the least, and 0 no enough data.

## Viewing Worldwide Sample data
head((covid19_world_df))

## Viewing US Sample data
head((covid19_us_df))
## Its a clean dataset with relevant information. So, Nothing to filter out.

## Load the 'Mental Health in Tech Survey' to
tech_survey_df <- read.csv("survey.csv")

## This dataset is from a 2014 survey that measures attitudes towards mental health and frequency of mental health disorders in the workplace. 

## Viewing Sample data
head((tech_survey_df))

tech_survey_upd_df <- tech_survey_df %>% filter(Country == "United States") %>% filter(Age > 12) %>% select(Age,Gender,family_history,treatment,remote_work,work_interfere,benefits,wellness_program,seek_help,anonymity,mental_health_consequence,obs_consequence)

head(tech_survey_upd_df)

# Standardize Gender with Male, Female, Other
tech_survey_upd_df["Gender"][tech_survey_upd_df["Gender"] == "M" | tech_survey_upd_df["Gender"] == "m" 
                             | tech_survey_upd_df["Gender"] == "male" | tech_survey_upd_df["Gender"] == "mail" | tech_survey_upd_df["Gender"] == "Mail"
                             | tech_survey_upd_df["Gender"] == "Cis male" | tech_survey_upd_df["Gender"] == "Cis Male"
                             | tech_survey_upd_df["Gender"] == "Male-ish" | tech_survey_upd_df["Gender"] == "Male (CIS)"
                             | tech_survey_upd_df["Gender"] == "Man" | tech_survey_upd_df["Gender"] == "Cis Man"
                             | tech_survey_upd_df["Gender"] == "Malr" | tech_survey_upd_df["Gender"] == "msle"
                             | tech_survey_upd_df["Gender"] == "Mal" | tech_survey_upd_df["Gender"] == "Make"
                             | tech_survey_upd_df["Gender"] == "maile" | tech_survey_upd_df["Gender"] == "cis male"] <- "Male"


tech_survey_upd_df["Gender"][tech_survey_upd_df["Gender"] == "F" | tech_survey_upd_df["Gender"] == "f" 
                             | tech_survey_upd_df["Gender"] == "female" | tech_survey_upd_df["Gender"] == "femail"
                             | tech_survey_upd_df["Gender"] == "Cis Female" | tech_survey_upd_df["Gender"] == "cis-female/femme"
                             | tech_survey_upd_df["Gender"] == "Woman" | tech_survey_upd_df["Gender"] == "Female (cis)"
                             | tech_survey_upd_df["Gender"] == "Femake" | tech_survey_upd_df["Gender"] == "woman" 
                             | tech_survey_upd_df["Gender"] == "Female (trans)"] <- "Female"

tech_survey_upd_df["Gender"][tech_survey_upd_df["Gender"] == "Female (trans)" | tech_survey_upd_df["Gender"] == "queer/she/they" 
                             | tech_survey_upd_df["Gender"] == "non-binary" | tech_survey_upd_df["Gender"] == "Nah"
                             | tech_survey_upd_df["Gender"] == "Genderqueer" | tech_survey_upd_df["Gender"] == "Trans-female"
                             | tech_survey_upd_df["Gender"] == "Trans woman" ] <- "Others"

head(tech_survey_upd_df)


## Only considering survey results from United States as its the majority.
## Noticed some negative numbers under 'Age' column which I will filter out.
## Under 'Gender' column, I see lot of variation and spelling error like Male,Mail,maile, M, Cis Male, Female, Cis Female, etc. I will make it consistent as Male, Female, Other.  
## Dropped some columns like State, No of Employee, Tech company, etc. as I don't think they add much value.  

## Basically I am looking for how many people opted for 'Treatment'.

# Q2 With a clean dataset, show what the final data set looks like. However, do not print off a data frame with 200+ rows; show me the data in the most condensed form possible.

## Viewing Sample data from 'Any mental illness/past year' dataset
head(state_any_mental_df)

## Viewing Sample data from 'Major depressive episode/past year' dataset
head(state_dep_mental_df)

## Viewing Sample data from 'Received Mental health services/past year' dataset
head(state_rcvd_mental_df)

## Viewing US Sample data related to 'Covid19 & Mental health effect/awareness'
head(covid19_us_df)

## Viewing Mental health in US Tech industry
head(tech_survey_upd_df)

# Q: Analysis

# Covid19 US dataset

## Viewing US Sample data related to 'Covid19 & Mental health effect/awareness'
head(covid19_us_df)

#summary
summary(covid19_us_df)

# plot
boxplot(covid19_us_df[-1], col = "red", varwidth = TRUE, xlab = "Mental Health Disorders")

## During Covid19 pandemic, The mental health related search terms like "depression", "anxiety", "ocd", etc. increased significantly on google search engine.
## This shows people either impacted directly or hearing from someone from their family and friends. But, it shows increased awareness and looking for options on the internet to address this problem.
## From this dataset I can see Depression, Anxiety and counseling are the top 3 search items but other than that not much information can be extracted from this dataset.

# US States Mental Heath Data
## Viewing Sample data from 'Any mental illness/past year' dataset
head(state_any_mental_df)

str(state_any_mental_df)

state_any_mental_df

ggplot(state_any_mental_df, aes(x = state, y = estimate)) +
  geom_point(size = 2.5, color = "red") + 
  geom_segment(aes(x = state, xend = state, y = 0, yend = estimate)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.7)) # Rotate axis label

## The top 3 states with any mental illness in year 2018-19 are Utah, West Virginia & Oregon.

## Viewing Sample data from 'Major depressive episode/past year' dataset
head(state_dep_mental_df)

library(ggplot2)
ggplot(state_dep_mental_df, aes(x = state, y = estimate)) +
  geom_point(size = 2.5, color = "red") + 
  geom_segment(aes(x = state, xend = state, y = 0, yend = estimate)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.7)) # Rotate axis label

## The top 3 states with major depressive episode in year 2018-19 are Utah, Oregon & West Virginia.

## Viewing Sample data from 'Received Mental health services/past year' dataset
head(state_rcvd_mental_df)

ggplot(state_rcvd_mental_df, aes(x = state, y = estimate)) +
  geom_point(size = 2.5, color = "red") + 
  geom_segment(aes(x = state, xend = state, y = 0, yend = estimate)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.7)) # Rotate axis label

## The top 3 states with people receiving mental health related services in year 2018-19 are Vermont, Massachusetts & Maine.

## So, based on the plots, we can see that Utah, Oregon and West Virginia are the top 3 states for any mental health illness as well as major depressive episodes.
## But, from treatment perspective, none of the 3 are in the top 3 list. This shows that even though lot of cases are reported, still not everyone is availing any kind of treatments.

# Mental health at Work place
## Viewing Mental health in US Tech industry
head(tech_survey_upd_df)

#summary
summary(tech_survey_upd_df)

str(tech_survey_upd_df)

# Replace Gender, Wellness program & Seek help column values to numeric
tech_survey_upd_df["Gender"][tech_survey_upd_df["Gender"] == "Male "] <- 1

tech_survey_upd_df["Gender"][tech_survey_upd_df["Gender"] == "Female "] <- 2

tech_survey_upd_df["Gender"][tech_survey_upd_df["Gender"] == "Others"] <- 3

tech_survey_upd_df["family_history"][tech_survey_upd_df["family_history"] == "Yes"] <- 1

tech_survey_upd_df["family_history"][tech_survey_upd_df["family_history"] == "No"] <- 2

tech_survey_upd_df["wellness_program"][tech_survey_upd_df["wellness_program"] == "Yes"] <- 1
tech_survey_upd_df["wellness_program"][tech_survey_upd_df["wellness_program"] == "No"] <- 2
tech_survey_upd_df["wellness_program"][tech_survey_upd_df["wellness_program"] == "Don't know"] <- 3

tech_survey_upd_df["seek_help"][tech_survey_upd_df["seek_help"] == "Yes"] <- 1
tech_survey_upd_df["seek_help"][tech_survey_upd_df["seek_help"] == "No"] <- 2
tech_survey_upd_df["seek_help"][tech_survey_upd_df["seek_help"] == "Don't know"] <- 3


# Filter out age above 100
tech_survey_upd_df2 <- tech_survey_upd_df %>% filter(Age < 100) %>%  select(Age,Gender,family_history,treatment,wellness_program,seek_help)

tech_survey_upd_df2["treatment"][tech_survey_upd_df2["treatment"] == "Yes"] <- 1
tech_survey_upd_df2["treatment"][tech_survey_upd_df2["treatment"] == "No"] <- 2

# Plot
ggplot(tech_survey_upd_df2, aes(x=treatment, y=Age, col=Gender)) + geom_point() + ggtitle("Treatment vs. Age vs. Gender") + xlab("Treatment (Yes = 1, No = 2)") + ylab("Age")

ggplot(tech_survey_upd_df2, aes(x=treatment, y=family_history, col=wellness_program)) + geom_point() + ggtitle("Treatment vs. family_history vs. wellness_program") + xlab("Treatment (Yes = 1, No = 2)") + ylab("family_history (Yes = 1, No = 2)")

ggplot(tech_survey_upd_df2, aes(x=treatment, y=wellness_program)) + geom_point() + ggtitle("Treatment vs. wellness_program") + xlab("Treatment (Yes = 1, No = 2)") + ylab("wellness_program (Yes = 1, No = 2, Don't Know = 3)")

ggplot(tech_survey_upd_df2, aes(x=treatment, y=family_history, col=seek_help)) + geom_point() + ggtitle("Treatment vs. family_history vs. Seek_help") + xlab("Treatment (Yes = 1, No = 2)") + ylab("family_history (Yes = 1, No = 2)")

plot(tech_survey_upd_df2)

# My target variable here is 'treatment'.Workplaces which promotes mental health and support their employees through different benefits
# and wellness programs will see more people opting for treatments and other kind of help if needed.
# I see more Male population tied to mental health but I see the ratio of male population much higher than female and others in the dataset.
# This can be due to more Male working in the tech industry or the survey population is biased.
# I also looked at family history to see if those people tend to be more aware qnd seek help if needed but didnt see much correlation.

# Q3 What do you not know how to do right now that you need to learn to import and cleanup your dataset?
## I am not able to replace the values in Gender column for Male, Female and Others in a simple and short steps.
 
# Q4 Discuss how you plan to uncover new information in the data that is not self-evident.
## First I want to see if Covid19 pandemic has a direct impact on mental health cases.Also, if its related to other datasets like Major depressive episode or 
## people received treatment in past year.
## Tech survey data is older and pre-pandemic, so there won't be any relationship with Covid19 but I want to see how many people took any treatment, 
## how much workplace and work environment is responsible for the mental stress, 
## does people with family history in mental health tend to be more aware and willing to take treatment compared to rest, 
## Is there a gender bias and other stuffs.

# Q5 What are different ways you could look at this data to answer the questions you want to answer?
## I am planning to use generalized linear model or Correlation coefficient to explore the data and relationship between different factors and the outcome of mental health treatment and awareness.

# Q6 Do you plan to slice and dice the data in different ways, create new variables, or join separate data frames to create new summary information? Explain.
## Yes, I plan to join dataframes 'state_any_mental_df' and 'state_rcvd_mental_df' to see if I can find out the percentage of people 
## who reported some kind of mental illness and out of which how much percentage actually went for the treatment.
## I also plan to see the US states with highest vs lowest cases reported.
## For the tech survey data, I want to slice based on Gender and other columns like family history, company wellness program, mental health consequences, etc. and see if that has any relationship with number of people went for treatment.

# Q7 How could you summarize your data to answer key questions?
## I will use the summary function and other slice/dice and join methods as mentioned above.
 
# Q8 What types of plots and tables will help you to illustrate the findings to your questions? Ensure that all graph plots have axis titles, legend if necessary, scales are appropriate, appropriate geoms used, etc.).
## Box plot and histogram.

# Q9 What do you not know how to do right now that you need to learn to answer your questions?
## I need more data exploration to answer this.

# Q10 Do you plan on incorporating any machine learning techniques to answer your research questions? Explain.
## Yes, I plan to split the dataset, 70:30 into training and validation, use Generalized linear model to predict and check the model accuracy.
