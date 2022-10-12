# clear environment
rm(list = ls())


dir <-
  "/home/angelo/Documents/Uni/Courses/Advanced Statistics and programming/Assignments/assignment3/"

dirProg <- paste0(dir, "Code/")

dirData <- paste0(dir, "Data/")

dirRslt <- paste0(dir, "Graphics/")


library(plyr)
library(stargazer)
library(plm)
library(ggplot2)

library(wbstats)



library("tidyverse")
library("stargazer")
library("tidyverse")
library("reshape2")
library("Hmisc")

library("dplyr")
library("moments")
library("lm.beta")
library("fastDummies")
library("stringr")
library("extrafont")
library("gridExtra")
library("AER")
library("DAAG")

library(RColorBrewer)
library(psych)
library(ROCR)

########################################

df <- read.csv((file = paste0(dirData, "online_ratings_travel.csv")), sep = ";")


View(df)
# review_stars, dFiveStars, travel,   

# 


# Yelp is an online platform where individuals can post or find reviews of restau-
#   rants, mostly in urban environments; see https://www.yelp.com. Variation in
# reviews is as inevitable as desirable, since it helps individuals to select restau-
#   rants of their liking. At the same time it is desirable to identify sources that
# systematically contribute to variation in these reviews. In order to explore the
# relations, Yelp has published a data set that contains online ratings from their
# restaurant rating platform. A special feature of this data set is that a distinction
# can be made between contributors who review restaurants located outside their
# home city, i.e., while traveling, or within their home city. This is captured by
# the indicator variable travel. To optimize the recommendations of restaurants to
# website visitors, it is essential to know which variables play a role in the online
# rating behavior of people. It is your task to analyze the influence of certain
# variables (such as travel) on the online rating. A description of the data set is in





#### Task 1.1 : Create Dependent Dichotomous #### 
df <- df %>%
  mutate(dFiveStars = case_when(
    review_stars == 5 ~ 1,
    review_stars != 5 ~ 0
  ))


#### Task 1.2 : summary statistics #### 

# Choices: "category" because of categorical variable being relevant here
# Choices: "price_range" becasue of cat var
# Choices: "years_elite"
# Choices: "fans"
# Choices: "yelping_since"
# review_stars, dFiveStars, travel
# useful_sent


# calculate the years since 

View(df)

df <- df %>%
  select((c("review_stars", "dFiveStars", "travel", "years_elite", "fans", "price_range", "useful_sent", 'numb_friends')))

stargazer(
  df,
  type = "text",
  omit.summary.stat = c("N"),
  summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
  title = "Descriptive Statistics Time to Export"
)

stargazer(
  df,
  type = "latex",
  omit.summary.stat = c("N"),
  summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
  title = "Descriptive Statistics Time to Export"
)
stargazer(df, type = 'text')

#### Task 2.1 : Formal Specification w. interaction#### 
# retrieve the dummies in price range
df <- dummy_cols(df, select_columns = 'price_range')
# df <- dummy_cols(df, select_columns = 'good_for_groups')


# fix price_range_4
df[is.na(df$price_range_4), "price_range_4"] <- 0
df[is.na(df$price_range_3), "price_range_3"] <- 0
df[is.na(df$price_range_2), "price_range_2"] <- 0
df[is.na(df$price_range_1), "price_range_1"] <- 0



df %>% 
  summarise_all(~sum(is.na(.)))

table(df$travel)
table(df$price_range)
table(df$price_range)

skewness(df$numb_friends)



#### Task 3.1 :  Run the binary choice model (logit + probit and with goodness of fit measure here#### 

# Define the model
mdlA <- dFiveStars ~ travel + fans + useful_sent + numb_friends +  price_range_2 + price_range_3 + price_range_4 + price_range_NA + travel * numb_friends
# "review_stars", "dFiveStars", "travel", "years_elite", "fans", "price_range", "useful_sent", 'numb_friends'


# Estimate the model
# for comparison reasons 
rsltOLS    <-  lm(mdlA, data = df)

# logit GLM (generalized linear model); BINOMINAL DISTREIBUTION!!!
rsltLogit  <- glm(mdlA, data = df, 
                  family=binomial(link = "logit"))

# PROBIT IT IS A PROBIT LINK!!!!
rsltProbit <- glm(mdlA, data = df, 
                  family=binomial(link = "probit"))


#### Task 3.2 :  Run the cont model with review_starts (logit + probit and with goodness of fit measure here####


# logit GLM (generalized linear model); BINOMINAL DISTREIBUTION!!!
rsltLogit  <- glm(mdlA, data = df, 
                  family=binomial(link = "logit"))

# PROBIT IT IS A PROBIT LINK!!!!
rsltProbit <- glm(mdlA, data = df, 
                  family=binomial(link = "probit"))











# Summarise the results
summary(rsltLogit)

# Make tble of the results
stargazer(rsltOLS, rsltLogit, rsltProbit,
          # align = TRUE, 
          no.space = TRUE, 
          intercept.bottom = FALSE, type="text")

stargazer(rsltOLS, rsltLogit, rsltProbit,
          # align = TRUE, 
          no.space = TRUE, 
          intercept.bottom = FALSE, type="latex")







# get the odds ratio
stargazer(exp(rsltLogit$coefficients), type="text")

#' see slide 49: the odds ratio is simply the expected number of the successful events  divided by the 
#' number of unsuccessful events (expectd number of flight travelses divided by the expected number of NON
#'  flight travelers; when a dummy switches from 0 to 1)
#' for dummies it is fairly easy:
#' so take the exponential of all the logistic regression coef (slide 48)
#' eg -0.445 --> take the exponential: exp (-0.445)
#' 
#' Interpretation: the odds of traveling by flight decreases by roughly 35.9 percent
#' If the dummy switches from 0 to 1, then the odds by traveling by air (based on closeness of airport) tje odds of travleing by air increases by 48%!!!




# logit 




# probit 




# THE INTERACTION EFFECT INTERPRETATION 




# DO THE ODDS RATIO STUFF FOR INTERPRETATION ETC



# 













