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
library(gofcat)
library(ROCR)
install.packages("gofcat", dependencies = T)
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
skewness(df_r$reviews_in_city)


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

# Nullmodel <- polr(review_stars ~ 1 , data = df_r, method = "probit")


summary(rsltordProbit)


stargazer(rsltLogit, rsltProbit, rsltordLogit, rsltordProbit, type="text",
          align=TRUE, no.space = TRUE, intercept.bottom = FALSE,
          se = list(NULL, NULL, NULL, NULL),  header=FALSE, 
          
          # single.row = TRUE, #
          
          column.sep.width = "0pt", 
          
          font.size = "small" 
          
)


stargazer(rsltLogit, rsltProbit, rsltordLogit, rsltordProbit, type="text",
          align=TRUE, no.space = TRUE, intercept.bottom = FALSE,  header=FALSE, 
          
          # single.row = TRUE, #
          
          column.sep.width = "0pt", 
          
          font.size = "small" 
          
)





add.lnL <- c("lnL", round(AIC(rsltLogit),3)
             ,round(AIC(rsltProbit),3),
             round(logLik(rsltordLogit),3), 
             round(logLik(rsltordProbit),3))

add.Aic <- c("AIC", round(AIC(rsltLogit),3)
             ,round(AIC(rsltProbit),3),
             round(logLik(rsltordLogit),3), 
             round(logLik(rsltordProbit),3))

add.Aic
summary(rsltordLogit)
stargazer(rsltordLogit, type="text")
summary(rsltordProbit)




# summary to extract the intercepts 

est.Logit  <- summary(rsltordLogit)$coefficients
est.Probit <- summary(rsltordProbit)$coefficients

add.mu1.est <- c("mu.1",
                 round(est.Logit[nrow(est.Logit)-3,  "Value"], 3),
                 round(est.Probit[nrow(est.Logit)-3, "Value"], 3)
)
add.mu1.std <- c("",
                 round(est.Logit[nrow(est.Logit)-3,  "Std. Error"], 3),
                 round(est.Probit[nrow(est.Logit)-3, "Std. Error"], 3)
)
add.mu2.est <- c("mu.2",
                 round(est.Logit[nrow(est.Logit)-2,  "Value"], 3),
                 round(est.Probit[nrow(est.Logit)-2, "Value"], 3)
)
add.mu2.std <- c("",
                 round(est.Logit[nrow(est.Logit)-2,  "Std. Error"], 3),
                 round(est.Probit[nrow(est.Logit)-2, "Std. Error"], 3)
)


add.mu3.est <- c("mu.3",
                 round(est.Logit[nrow(est.Logit)-1,  "Value"], 3),
                 round(est.Probit[nrow(est.Logit)-1, "Value"], 3)
)
add.mu3.std <- c("",
                 round(est.Logit[nrow(est.Logit)-1,  "Std. Error"], 3),
                 round(est.Probit[nrow(est.Logit)-1, "Std. Error"], 3)
)

add.mu4.est <- c("mu.4",
                 round(est.Logit[nrow(est.Logit),  "Value"], 3),
                 round(est.Probit[nrow(est.Logit), "Value"], 3)
)
add.mu4.std <- c("",
                 round(est.Logit[nrow(est.Logit),  "Std. Error"], 3),
                 round(est.Probit[nrow(est.Logit), "Std. Error"], 3)
)



# Make the table
stargazer(rsltordLogit, rsltordProbit,
          align=TRUE, no.space = TRUE, intercept.bottom = TRUE, type="text",
          add.lines = list(add.mu1.est, add.mu1.std,
                           add.mu2.est, add.mu2.std,
                           add.lnL, add.Aic))



stargazer(rsltLogit, rsltProbit, rsltordLogit, rsltordProbit,
          align=FALSE, no.space = TRUE, intercept.bottom = FALSE, type="latex",
          add.lines = list(add.mu1.est, add.mu1.std,
                           add.mu2.est, add.mu2.std,
                           add.mu3.est, add.mu3.std,
                           add.mu4.est, add.mu4.std,
                           add.lnL, add.Aic))


#########

# quick way

library("piecewiseSEM")
piecewiseSEM::Rsquared(rsltLogit, measure = "mcfadden")
Rsquared(rsltProbit, measure = "mcfadden")

Rsquared(rsltordLogit, measure = "mcfadden")
Rsquared(rsltordProbit, measure = "mcfadden")


#### Task 3.3: goodness of fit for each 

#------------------
# Collect relevant info
#------------------

# rsltLogit, rsltProbit, rsltordLogit, rsltordProbit

# Check contents of the glm object





mdlbin_null <- dFiveStars ~ +1



# logit GLM (generalized linear model); BINOMINAL DISTREIBUTION!!!
rsltLogit_null  <- glm(mdlbin_null, data = df_r, 
                  family=binomial(link = "logit"))

# PROBIT IT IS A PROBIT LINK!!!!
rsltProbit_null <- glm(mdlbin_null, data = df_r, 
                  family=binomial(link = "probit"))




# Define the model
mdlordR_null <- review_stars ~  + 1


rsltordLogit_null  <- polr(mdlordR_null, data = as.data.frame(df_r), method = "logistic")



rsltordProbit_null <- polr(mdlordR_null, data = df_r, method = "probit")


rsltLogit_null
rsltProbit_null
rsltordLogit_null
rsltordProbit_null

## Log-likelihood values
# rsltLogit
lnL.fitted_rsltLogit <- -0.5*rsltLogit$deviance
lnL.null_rsltLogit   <- -0.5*rsltLogit_null$null.deviance
lnL.fitted_rsltLogit
lnL.null_rsltLogit
# rsltProbit
lnL.fitted_rsltProbit <- -0.5*rsltProbit$deviance
lnL.null_rsltProbit   <- -0.5*rsltProbit_null$null.deviance
lnL.fitted_rsltProbit
lnL.null_rsltProbit
# rsltordLogit
lnL.fitted_rsltordLogit <- -0.5*rsltordLogit$deviance
lnL.null_rsltordLogit   <- -0.5*rsltordLogit_null$null.deviance
lnL.fitted_rsltordLogit
lnL.null_rsltordLogit
# rsltordProbit
lnL.fitted_rsltordProbit <- -0.5*rsltordProbit$deviance
lnL.null_rsltordProbit   <- -0.5*rsltordProbit_null$null.deviance
lnL.fitted_rsltordProbit
lnL.null_rsltordProbit


# probit
rsltProbit_null  <- polr( review_stars ~  1, data = df_r, method = "logistic")
lnL.null <- -0.5* rsltLogit_null$deviance
lnL.fitted <- -0.5* rsltLogit$deviance
lnL.null <- -0.5* rsltLogit_null$deviance
df.fitted <- rsltLogit$df.residual
df.null <- rsltLogit_null$df.residual
K <- df.null - df.fitted
N <- df.null + 1
probLogit <- predict.glm(rsltLogit,type="response")
yvalue <- df_r$review_stars %>% as.numeric()

McFadden.R2_Logis <- 1 - (lnL.fitted/lnL.null)
McFadden.R2adj_Logis <- 1 - ((lnL.fitted - K)/lnL.null)

McFadden.R2_Logis


McFadden.R2adj_Logis

# probit
rsltProbit_null  <- polr( review_stars ~  1, data = df_r, method = "probit")
lnL.null <- -0.5* rsltLogit_null$deviance
lnL.fitted <- -0.5* rsltLogit$deviance
lnL.null <- -0.5* rsltLogit_null$deviance
df.fitted <- rsltLogit$df.residual
df.null <- rsltLogit_null$df.residual
K <- df.null - df.fitted
N <- df.null + 1
probLogit <- predict.glm(rsltLogit,type="response")
yvalue <- df_r$review_stars %>% as.numeric()

McFadden.R2_probit <- 1 - (lnL.fitted/lnL.null)
McFadden.R2adj_probit <- 1 - ((lnL.fitted - K)/lnL.null)

McFadden.R2_probit


McFadden.R2adj_probit






# probit bin
rsltProbit_null  <- glm( dFiveStars ~  1, data = df_r, 
                         family=binomial(link = "probit"))
lnL.null <- -0.5* rsltLogit_null$deviance
lnL.fitted <- -0.5* rsltLogit$deviance
lnL.null <- -0.5* rsltLogit_null$deviance
df.fitted <- rsltLogit$df.residual
df.null <- rsltLogit_null$df.residual
K <- df.null - df.fitted
N <- df.null + 1
probLogit <- predict.glm(rsltLogit,type="response")
yvalue <- df_r$review_stars %>% as.numeric()

McFadden.R2_binprobit <- 1 - (lnL.fitted/lnL.null)
McFadden.R2adj_binprobit <- 1 - ((lnL.fitted - K)/lnL.null)

McFadden.R2_binprobit


McFadden.R2adj_binprobit



# logit bin
rsltProbit_null  <- glm( dFiveStars ~  1, data = df_r, 
                         family=binomial(link = "logit"))
lnL.null <- -0.5* rsltLogit_null$deviance
lnL.fitted <- -0.5* rsltLogit$deviance
lnL.null <- -0.5* rsltLogit_null$deviance
df.fitted <- rsltLogit$df.residual
df.null <- rsltLogit_null$df.residual
K <- df.null - df.fitted
N <- df.null + 1
probLogit <- predict.glm(rsltLogit,type="response")
yvalue <- df_r$review_stars %>% as.numeric()

McFadden.R2_binlogit <- 1 - (lnL.fitted/lnL.null)
McFadden.R2adj_binlogit <- 1 - ((lnL.fitted - K)/lnL.null)

McFadden.R2_binlogit


McFadden.R2adj_binlogit











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



#------------------
# BINARY CHOICE MODELS APE: average partial effects
#------------------

# Determine average partial effects for all explanatory 
# variables regardless measurement levels

betaLogit  <- coefficients(rsltLogit)
betaProbit <- coefficients(rsltProbit)



# Predict (type="link) gives avg-x'beta, function dlogis/dnorm
# calculates the density f(avg-x'beta); different from PEA, these
# predictions are made for individual observations, which are
# averaged with function mean  - note that the estimates beta's
# are the same for each observation and are therefore left outside
# the averaging
APElogit.1   <- mean(dlogis(predict(rsltLogit, type="link")))*betaLogit
APEprobit.1  <- mean(dnorm(predict(rsltProbit, type="link")))*betaProbit

# The average partial effect for dummy variables is the average
# of the differences between success probabilities if the dummy 
# event does occur and does not occur for each observations; in 
# the example this only needs to be done for variable dNearby
tmp             <- df_r

# Predict (type="response") gives P(Y=1|avg-x'beta)
tmp$dFiveStars  <- 0

tmpAPElogit.0 <- 
  mean(predict(rsltLogit, newdata=tmp, type="response"))
tmpAPEprobit.0<- 
  mean(predict(rsltProbit,newdata=tmp, type="response"))

tmp$dFiveStars  <- 1

tmpAPElogit.1 <- 
  mean(predict(rsltLogit, newdata=tmp, type="response"))
tmpAPEprobit.1<- 
  mean(predict(rsltProbit,newdata=tmp, type="response"))

APElogit.2 <- APElogit.1
APElogit.2["travel"] <- tmpAPElogit.1 - tmpAPElogit.0

APEprobit.2 <- APEprobit.1
APEprobit.2["travel"]<- tmpAPEprobit.1 -tmpAPEprobit.0




stargazer(cbind(APElogit.1, APElogit.2,APEprobit.1,APEprobit.2), type = "text",
          header=FALSE,
          no.space = TRUE,  
          column.sep.width = "0pt", 
          digits = 4,
          font.size = "small")




#------------------
# ORDINAL RESPONSE MODELS APE: average partial effects
#------------------

#Cumulative prob


prb.Logit  <- as.data.frame(predict(rsltordLogit,  type="probs"))
prb.Probit <- as.data.frame(predict(rsltordProbit, type="probs"))


cdf.Logit.1 <- prb.Logit[, 1]

cdf.Logit.2 <- prb.Logit[, 1] + prb.Logit[, 2]

cdf.Logit.3 <- prb.Logit[, 1] + prb.Logit[, 2] + prb.Logit[, 3]

cdf.Logit.4 <- prb.Logit[, 1] + prb.Logit[, 2] + prb.Logit[, 3] + prb.Logit[, 4]

cdf.Logit.5 <- prb.Logit[, 1] + prb.Logit[, 2] + prb.Logit[, 3] + prb.Logit[, 4] + prb.Logit[, 5]


cdf.Probit.1 <- prb.Probit[, 1]
cdf.Probit.2 <- prb.Probit[, 1] + prb.Probit[, 2]
cdf.Probit.3 <- prb.Probit[, 1] + prb.Probit[, 2] + prb.Probit[, 3]
cdf.Probit.4 <- prb.Probit[, 1] + prb.Probit[, 2] + prb.Probit[, 3] + prb.Probit[, 4]
cdf.Probit.5 <- prb.Probit[, 1] + prb.Probit[, 2] + prb.Probit[, 3] + prb.Probit[, 4] + prb.Probit[, 5]



# Calculate density parts of the effects (Greene, p.910)
prb.Logit$pdf.1 <- 
  -dlogis(qlogis(cdf.Logit.1))
prb.Logit$pdf.2 <- 
  dlogis(qlogis(cdf.Logit.1)) - dlogis(qlogis(cdf.Logit.2))
prb.Logit$pdf.3 <- 
  dlogis(qlogis(cdf.Logit.2)) - dlogis(qlogis(cdf.Logit.3))
prb.Logit$pdf.4 <- 
  dlogis(qlogis(cdf.Logit.3)) - dlogis(qlogis(cdf.Logit.4))
prb.Logit$pdf.5 <- 
  dlogis(qlogis(cdf.Logit.4))


prb.Probit$pdf.1 <- 
  -dlogis(qlogis(cdf.Probit.1))
prb.Probit$pdf.2 <- 
  dlogis(qlogis(cdf.Probit.1)) - dlogis(qlogis(cdf.Probit.2))
prb.Probit$pdf.3 <- 
  dlogis(qlogis(cdf.Probit.2)) - dlogis(qlogis(cdf.Probit.3))
prb.Probit$pdf.4 <- 
  dlogis(qlogis(cdf.Probit.3)) - dlogis(qlogis(cdf.Probit.4))
prb.Probit$pdf.5 <- 
  dlogis(qlogis(cdf.Probit.4))



# Determine the average effects (apart from the estimated 
# effects)
avgAPE.Logit  <- colMeans(prb.Logit[c("pdf.1", "pdf.2", "pdf.3", "pdf.4", "pdf.5" )])
avgAPE.Probit <- colMeans(prb.Probit[c("pdf.1", "pdf.2", "pdf.3", "pdf.4", "pdf.5")])

# Extract the estimated effects from the logit
# and probit objects
est.Logit  <- coef(rsltordLogit)
est.Probit <- coef(rsltordProbit)

# Determine the APE
dfAPE.Logit  <- as.data.frame(avgAPE.Logit %*% t(est.Logit))
dfAPE.Probit <- as.data.frame(avgAPE.Probit %*% t(est.Probit))

rownames(dfAPE.Logit) <- rownames(dfAPE.Probit) <- c("P(y=1)", "P(y=2)", "P(y=3)", "P(y=4)", "P(y=5)" )



# Make the table

stargazer(dfAPE.Logit, summary = FALSE,
          align = F, no.space = F, type="latex",
          header=F,
          font.size = "small")


stargazer(dfAPE.Probit, summary = FALSE,
          align = F, no.space = F, type="latex",
          header=F,
          font.size = "small")









