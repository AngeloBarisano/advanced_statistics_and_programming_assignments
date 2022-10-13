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
library("irrCAC")


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
library("MASS")
library(RColorBrewer)
library(psych)
library(ROCR)

########################################

df_r <- read.csv(file = "/home/angelo/Documents/Uni/Courses/Advanced Statistics and programming/Assignments/assignment3/Data/online_ratings_travel.csv", sep = ";")
typeof(df_r)
# df_r)
# df_r)
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

colnames(df_r)[colnames(df_r) == "reviews_in_city_so_far"]    <- "reviews_in_city"



#### Task 1.1 : Create Dependent Dichotomous #### 
df_r <- df_r %>%
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

df_r <- df_r[,c("review_stars", "dFiveStars","reviews_in_city", "travel", "fans", 'numb_friends', "length"), drop=FALSE] 

typeof(df_r)


# df_r <- data.frame(df_r)

stargazer(
  df_r,
  type = "text",
  omit.summary.stat = c("N"),
  summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
  title = "Descriptive Statistics Time to Export"
)

stargazer(
  df_r,
  type = "latex",
  omit.summary.stat = c("N"),
  summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
  title = "Descriptive Statistics Time to Export"
)
stargazer(df_r, type = 'text')

#### Task 2.1 : Formal Specification w. interaction#### 
# retrieve the dummies in price range

# df_r <- dummy_cols(df_r, select_columns = 'good_for_groups')


# fix price_range_4




df_r %>% 
  summarise_all(~sum(is.na(.)))

table(df_r$travel)
table(df_r$price_range)
table(df_r$price_range)

skewness(df_r$numb_friends)



#### Task 3.1 :  Run the binary choice model (logit + probit and with goodness of fit measure here#### 

# Define the model
df_r$review_stars <- as.factor(df_r$review_stars)
df_r$dFiveStars <- as.factor(df_r$dFiveStars)


mdlbin <- dFiveStars ~ travel + reviews_in_city+   fans  + numb_friends + length +  travel * numb_friends

# ols
rsltOLS    <-  lm(mdlbin, data = df_r)


# logit GLM (generalized linear model); BINOMINAL DISTREIBUTION!!!
rsltLogit  <- glm(mdlbin, data = df_r, 
                  family=binomial(link = "logit"))

# PROBIT IT IS A PROBIT LINK!!!!
rsltProbit <- glm(mdlbin, data = df_r, 
                  family=binomial(link = "probit"))



summary(rsltLogit)
#### Task 3.2 :  Run the ORDERED RESPONSE model with review_starts (logit + probit and with goodness of fit measure here####

# Define the model
mdlordR <- review_stars ~  travel + reviews_in_city + fans  + numb_friends + length +  travel * numb_friends


# logit GLM (generalized linear model); BINOMINAL DISTREIBUTION!!!
# rsltordLogit  <- glm(mdlordR, data = df_r,
#                   family=binomial(link = "logit"))

# PROBIT IT IS A PROBIT LINK!!!!
# rsltordProbit <- glm(mdlordR, data = df_r, 
#                   family=binomial(link = "probit"))


rsltordLogit  <- polr(mdlordR, data = as.data.frame(df_r), method = "logistic")
summary(rsltordLogit)


rsltordProbit <- polr(mdlordR, data = df_r, method = "probit")

Nullmodel <- polr(review_stars ~ 1 , data = df_r, method = "probit")


summary(rsltordProbit)


stargazer(rsltLogit, rsltProbit, rsltordLogit, rsltordProbit, type="text",
          align=TRUE, no.space = TRUE, intercept.bottom = FALSE,
          se = list(NULL, NULL, NULL, NULL),  header=FALSE, 
          
          # single.row = TRUE, #
          
          column.sep.width = "0pt", 
          
          font.size = "small" 
          
)


#### Task 3.3: goodness of fit for each 

#------------------
# Collect relevant info
#------------------

# rsltLogit, rsltProbit, rsltordLogit, rsltordProbit

# Check contents of the glm object
str(rsltordLogit)

## Log-likelihood values
# rsltLogit
lnL.fitted_rsltLogit <- -0.5*rsltLogit$deviance
lnL.null_rsltLogit   <- -0.5*rsltLogit$null.deviance
lnL.fitted_rsltLogit
lnL.null_rsltLogit
# rsltProbit
lnL.fitted_rsltProbit <- -0.5*rsltProbit$deviance
lnL.null_rsltProbit   <- -0.5*rsltProbit$null.deviance
lnL.fitted_rsltProbit
lnL.null_rsltProbit
# rsltordLogit
lnL.fitted_rsltordLogit <- -0.5*rsltordLogit$deviance
lnL.null_rsltordLogit   <- -0.5*rsltordLogit$null.deviance
lnL.fitted_rsltordLogit
lnL.null_rsltordLogit
# rsltordProbit
lnL.fitted_rsltordProbit <- -0.5*rsltordProbit$deviance
lnL.null_rsltordProbit   <- -0.5*rsltordProbit$null.deviance
lnL.fitted_rsltordProbit
lnL.null_rsltordProbit


# Degrees of freedom of both models
str(rsltLogit)
# rsltLogit
df_r.fitted_rsltLogit  <- rsltLogit$df_r.residual
df_r.null_rsltLogit    <- rsltLogit$df_r.null
df_r.fitted_rsltLogit
df_r.null_rsltLogit
# rsltProbit
df_r.fitted_rsltProbit  <- rsltProbit$df_r.residual
df_r.null_rsltProbit    <- rsltProbit$df_r.null
df_r.fitted_rsltProbit
df_r.null_rsltProbit
# rsltordLogit
df_r.fitted_rsltordLogit  <- rsltordLogit$df_r.residual
df_r.null_rsltordLogit    <- rsltordLogit$df_r.null
df_r.fitted_rsltordLogit
df_r.null_rsltordLogit
# rsltordProbit
df_r.fitted_rsltordProbit  <- rsltordProbit$df_r.residual
df_r.null_rsltordProbit    <- rsltordProbit$df_r.null
df_r.fitted_rsltordProbit
df_r.null_rsltordProbit





# Number of predictors and sample size
# rsltLogit
K_rsltLogit  <- df_r.null_rsltLogit - df_r.fitted_rsltLogit
N_rsltLogit  <- df_r.null_rsltLogit + 1
K_rsltLogit
N_rsltLogit
# rsltProbit
K_rsltProbit <- df_r.null_rsltProbit - df_r.fitted_rsltProbit
N_rsltProbit  <- df_r.null_rsltProbit + 1
K_rsltProbit
N_rsltProbit

# rsltordLogit
K_rsltordLogit <- df_r.null_rsltordLogit - df_r.fitted_rsltordLogit
N_rsltordLogit  <- df_r.null_rsltordLogit + 1
K_rsltordLogit
N_rsltordLogit
# rsltordProbit
K_rsltordProbit  <- df_r.null_rsltordProbit - df_r.fitted_rsltordProbit
N_rsltordProbit  <- df_r.null_rsltordProbit + 1
K_rsltordProbit
N_rsltordProbit



# Predicted and observed value of the target
# rsltLogit
probLogit_rsltLogit <- predict.glm(rsltLogit, type = "response")
yvalue_rsltLogit    <- df_r$dFiveStars


# rsltProbit
probLogit_rsltProbit <- predict.glm(rsltProbit, type = "response")
yvalue_rsltProbit    <- df_r$dFiveStars


# rsltordLogit
probLogit_rsltordLogit <- predict.glm(rsltordLogit, type = "response")
yvalue_rsltordLogit    <- df_r$review_stars


# rsltordProbit
probLogit_rsltordProbit <- predict.glm(rsltordProbit, type = "response")
yvalue_rsltordProbit    <- df_r$review_stars



# 1. Pseudo R2 and adjusted pseudo R2, McFadden. Resembles 
# R2 defined as percentage explained variation and as 
# improvement of fitted model over null model. The adjusted 
# version penalizes extra predictor variables. The closer
# to 1, the larger the improvement

#' it resembles R2; it is defined as percentage of explanation and improvement over null model
#' the closer to 1 the larger the model improvement compare to the null model!


# rsltLogit
McFadden.R2_rsltLogit    <- 1 - (lnL.fitted_rsltLogit/lnL.null_rsltLogit)
McFadden.R2adj_rsltLogit <- 1 - ((lnL.fitted_rsltLogit - K_rsltLogit)/lnL.null_rsltLogit)
McFadden.R2_rsltLogit
McFadden.R2adj_rsltLogit
# rsltProbit
McFadden.R2_rsltProbit    <- 1 - (lnL.fitted_rsltProbit/lnL.null_rsltProbit)
McFadden.R2adj_rsltProbit <- 1 - ((lnL.fitted_rsltProbit - K_rsltProbit)/lnL.null_rsltProbit)
McFadden.R2_rsltProbit
McFadden.R2adj_rsltProbit
# rsltordLogit
McFadden.R2_rsltordLogit    <- 1 - (lnL.fitted_rsltordLogit/lnL.null_rsltordLogit)
McFadden.R2adj_rsltordLogit <- 1 - ((lnL.fitted_rsltordLogit - K_rsltordLogit)/lnL.null_rsltordLogit)
McFadden.R2_rsltordLogit
McFadden.R2adj_rsltordLogit
# rsltordProbit
McFadden.R2_rsltordProbit    <- 1 - (lnL.fitted_rsltordProbit/lnL.null_rsltordProbit)
McFadden.R2adj_rsltordProbit <- 1 - ((lnL.fitted_rsltordProbit - K_rsltordProbit)/lnL.null_rsltordProbit)
McFadden.R2_rsltordProbit
McFadden.R2adj_rsltordProbit





# 2. Cox & Snell. Resembles R2 as improvement of fitted 
# model over the null model. Taking N-th root yields the 
# contribution of each observation (it is based on the 
# LRT statistic, which explains the '2'). Note that the 
# maximum is smaller than 1.

#'you take acount for the number of observations as the goodness of fit 


# rsltLogit
CoxSnell.R2_rsltLogit <- 1 - (exp(lnL.null_rsltLogit)/exp(lnL.fitted_rsltLogit))^(2/N_rsltLogit)
CoxSnell.R2_rsltLogit
# rsltProbit
CoxSnell.R2_rsltProbit <- 1 - (exp(lnL.null_rsltProbit)/exp(lnL.fitted_rsltProbit))^(2/N_rsltProbit)
CoxSnell.R2_rsltProbit
# rsltordLogit
CoxSnell.R2_rsltordLogit <- 1 - (exp(lnL.null_rsltordLogit)/exp(lnL.fitted_rsltordLogit))^(2/N_rsltordLogit)
CoxSnell.R2_rsltordLogit
# rsltordProbit
CoxSnell.R2_rsltordProbit <- 1 - (exp(lnL.null_rsltordProbit)/exp(lnL.fitted_rsltordProbit))^(2/N_rsltordProbit)
CoxSnell.R2_rsltordProbit



# 3. Nagelkerke/Crag and Uhle. Resembles R2 as improvement
# of fitted model over the null model. SImilar to Cox and 
# Snell's pseudo R2, but divided by the latter's maximum.

#' builds  upon cox snel
# rsltLogit
Nagelkerke.R2_rsltLogit <- CoxSnell.R2_rsltLogit/(1 - exp(lnL.null_rsltLogit)^(2/N_rsltLogit))
Nagelkerke.R2_rsltLogit

# rsltProbit
Nagelkerke.R2_rsltProbit <- CoxSnell.R2_rsltProbit/(1 - exp(lnL.null_rsltProbit)^(2/N_rsltProbit))
Nagelkerke.R2_rsltProbit

# rsltordLogit
Nagelkerke.R2_rsltordLogit <- CoxSnell.R2_rsltordLogit/(1 - exp(lnL.null_rsltordLogit)^(2/N_rsltordLogit))
Nagelkerke.R2_rsltordLogit

# rsltordProbit
Nagelkerke.R2_rsltordProbit <- CoxSnell.R2_rsltordProbit/(1 - exp(lnL.null_rsltordProbit)^(2/N_rsltordProbit))
Nagelkerke.R2_rsltordProbit


# 4. Efron. Resembles R2 defined as percentage explained 
# variation and as squared correlation between predicted 
# and actual dependent values

#' this one resembles R2 explaiing the varaition as the squared correlationbetween the actual and the predicted depednent
#' difference between what you expect and what is actually going on difference

# rsltLogit
Efron.R2_rsltLogit <- 1 - 
  sum((yvalue_rsltLogit-probLogit_rsltLogit)^2)/sum((yvalue_rsltLogit-mean(yvalue_rsltLogit))^2)
Efron.R2_rsltLogit
# rsltProbit
Efron.R2_rsltProbit <- 1 - 
  sum((yvalue_rsltProbit-probLogit_rsltProbit)^2)/sum((yvalue_rsltProbit-mean(yvalue_rsltProbit))^2)
Efron.R2_rsltProbit
# rsltordLogit
Efron.R2_rsltordLogit <- 1 - 
  sum((yvalue_rsltordLogit-probLogit_rsltordLogit)^2)/sum((yvalue_rsltordLogit-mean(yvalue_rsltordLogit))^2)
Efron.R2_rsltordLogit
# rsltordProbit
Efron.R2_rsltordProbit <- 1 - 
  sum((yvalue_rsltordProbit-probLogit_rsltordProbit)^2)/sum((yvalue_rsltordProbit-mean(yvalue_rsltordProbit))^2)
Efron.R2_rsltordProbit



# 5. Count R2 and adjusted count R2. Percentage of correct 
# classification (Accuracy). The adjusted version compares 
# the number of correct classifications to the frequency of
# the dominant outcome.

#' the number of correct classigfications to the frequency of dominant outcome
#' essentially how often your model predicts the right outcome!
# rsltLogit
Count.R2_rsltLogit <- sum(yvalue_rsltLogit == (probLogit_rsltLogit>0.5))/length(yvalue_rsltLogit)
Count.R2_rsltLogit
maxFreq_rsltLogit     <- max(table(yvalue_rsltLogit))
Count.R2adj_rsltLogit <- (sum(yvalue_rsltLogit == (probLogit_rsltLogit>0.5)) - maxFreq_rsltLogit)/
  (length(yvalue_rsltLogit) - maxFreq_rsltLogit)
Count.R2adj_rsltLogit

# _rsltProbit
Count.R2_rsltProbit <- sum(yvalue_rsltProbit == (probLogit_rsltProbit>0.5))/length(yvalue_rsltProbit)
Count.R2_rsltProbit
maxFreq_rsltProbit     <- max(table(yvalue_rsltProbit))
Count.R2adj_rsltProbit <- (sum(yvalue_rsltProbit == (probLogit_rsltProbit>0.5)) - maxFreq_rsltProbit)/
  (length(yvalue_rsltProbit) - maxFreq_rsltProbit)
Count.R2adj_rsltProbit


# _rsltordLogit
Count.R2_rsltordLogit <- sum(yvalue_rsltordLogit == (probLogit_rsltordLogit>0.5))/length(yvalue_rsltordLogit)
Count.R2_rsltordLogit
maxFreq_rsltordLogit     <- max(table(yvalue_rsltordLogit))
Count.R2adj_rsltordLogit <- (sum(yvalue_rsltordLogit == (probLogit_rsltordLogit>0.5)) - maxFreq_rsltordLogit)/
  (length(yvalue_rsltordLogit) - maxFreq_rsltordLogit)
Count.R2adj_rsltordLogit


# _rsltordProbit
Count.R2_rsltordProbit <- sum(yvalue_rsltordProbit == (probLogit_rsltordProbit>0.5))/length(yvalue_rsltordProbit)
Count.R2_rsltordProbit
maxFreq_rsltordProbit     <- max(table(yvalue_rsltordProbit))
Count.R2adj_rsltordProbit <- (sum(yvalue_rsltordProbit == (probLogit_rsltordProbit>0.5)) - maxFreq_rsltordProbit)/
  (length(yvalue_rsltordProbit) - maxFreq_rsltordProbit)
Count.R2adj_rsltordProbit




Count.R2 <- sum(yvalue == (probLogit>0.5))/length(yvalue)

maxFreq     <- max(table(yvalue))
Count.R2adj <- (sum(yvalue == (probLogit>0.5)) - maxFreq)/
  (length(yvalue) - maxFreq)

t(round(
  cbind(McFadden.R2, McFadden.R2adj, CoxSnell.R2, 
        Nagelkerke.R2, Efron.R2, 
        Count.R2, Count.R2adj), 3))

####

#ACI and LIL

#### Task 3.4: APE etc!


















# get the odds ratio
stargazer(exp(rsltLogit$coefficients), type="text")
stargazer(exp(rsltProbit$coefficients), type="text")
stargazer(exp(rsltordLogit$coefficients), type="text")
stargazer(exp(rsltordProbit$coefficients), type="text")
#' see slide 49: the odds ratio is simply the expected number of the successful events  divided by the 
#' number of unsuccessful events (expectd number of flight travelses divided by the expected number of NON
#'  flight travelers; when a dummy switches from 0 to 1)
#' for dummies it is fairly easy:
#' so take the exponential of all the logistic regression coef (slide 48)
#' eg -0.445 --> take the exponential: exp (-0.445)
#' 
#' Interpretation: the odds of traveling by flight decreases by roughly 35.9 percent
#' If the dummy switches from 0 to 1, then the odds by traveling by air (based on closeness of airport) tje odds of travleing by air increases by 48%!!!













