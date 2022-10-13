# clear environment
rm(list = ls())


dir <-
  "/home/angelo/Documents/Uni/Courses/Advanced Statistics and programming/Assignments/assignment3/"

dirProg <- paste0(dir, "Code/")

dirData <- paste0(dir, "Data/")

dirRslt <- paste0(dir, "Graphics/")
library("MASS")
library(ggplot2)
library(tidyverse)
library(stargazer)
library(tibble)
library(dplyr)
library(moments)
library(plyr)
library(AER)
library(car)
library(scales)
library(lm.beta)
library(ggpubr)
library(rmarkdown)
library(grid)
library(reshape2)
library(wbstats)
library(plm)
library(reporttools)
########################################
df <- read.csv(file = paste0(dirData,"OnlineNewsPopularity.csv"),
                    stringsAsFactors = FALSE)

# View(df)


######################################################################################################################################################
#### Task 2.1 ########################################################################################################################################
######################################################################################################################################################
#Choose relevant variables 


df<- df[, c("shares", "n_unique_tokens", "num_hrefs", "num_imgs", 
                       "num_videos", "average_token_length", 
                       "title_sentiment_polarity","avg_negative_polarity", "rate_positive_words",
                       "num_keywords"                       )]


#Rename the columns
colnames(df)[colnames(df) == "n_unique_tokens"]<- "unique_tok"
colnames(df)[colnames(df) == "average_token_length"]<- "avg_token_len"
colnames(df)[colnames(df) == "avg_negative_polarity"]<- "avg_neg_pol"
colnames(df)[colnames(df) == "title_sentiment_polarity"]<- "title_sentiment"
colnames(df)[colnames(df) == "rate_positive_words"]<- "rate_pos_words"

df %>% 
  summarise_all(~sum(is.na(.)))

skewness(df$shares)
skewness(df$unique_tok)
skewness(df$num_hrefs)
skewness(df$num_imgs)
skewness(df$num_videos)
skewness(df$avg_token_len)
skewness(df$title_sentiment)
skewness(df$avg_neg_pol)
skewness(df$rate_pos_words)
skewness(df$num_keywords)


stargazer(df, median = TRUE, iqr = TRUE, title = "Summary Statistics News-Share",  
          omit.summary.stat = c("N"), digits=3,
          
          
          header=FALSE, type = "latex",
          
          single.row = TRUE, 
          
          no.space = TRUE, 
          
          column.sep.width = "0pt",
          
          font.size = "small" 
)

colnames(df)
stargazer(df, type = "text")


######################################################################################################################################################
#### Task 2.2 ########################################################################################################################################
######################################################################################################################################################
# estimate the model


#Poisson model

# Model specification
mdlA <- shares ~ unique_tok + num_hrefs + num_imgs + num_videos +  avg_token_len + title_sentiment + avg_neg_pol + rate_pos_words + num_keywords 

# Model estimation Simple Poson
rslt.Poisson <- glm(mdlA, data=df, family=c("poisson"))

# Estimate the quasi-Poisson and negative binomial models
rslt.Quasi   <- glm(mdlA, data=df, family=c("quasipoisson"))
rslt.NegBin  <- glm.nb(mdlA, data=df)

# get standardizzed poison SE
seWhite <- sqrt(diag(vcovHC(rslt.Poisson, type="HC0")))


stargazer(rslt.Poisson, rslt.Poisson, rslt.Quasi, rslt.NegBin, type="latex",
          align=FALSE, no.space = TRUE, intercept.bottom = FALSE,
          se = list(NULL, seWhite, NULL, NULL),  header=FALSE
          
          # single.row = TRUE, 
          
          # column.sep.width = "0pt", 
          
          # font.size = "small" 
          
)


stargazer(rslt.Poisson, rslt.Poisson, rslt.Quasi, rslt.NegBin,
          se = list(NULL, seWhite, NULL, NULL),
          align=FALSE, no.space=TRUE, intercept.bottom = FALSE, type="text",report=("vc*p"))

stargazer(rslt.Poisson, rslt.Poisson, rslt.Quasi, rslt.NegBin,
          se = list(NULL, seWhite, NULL, NULL),
          align=FALSE, no.space=TRUE, intercept.bottom = FALSE, type="latex")



# Perform Likehood Ratio test
lrtest(rslt.Poisson, rslt.NegBin)
# THIS MEANS THAT THE BINOMINAL MODEL IS PREFERRED!!!





exp(0.001)

######################################################################################################################################################
#### Task 2.3 ########################################################################################################################################
######################################################################################################################################################
# estimate the model
# based on the aforementioend findings that the negative binominal model is better or preferred, we choose 

# OLS 
rsltOLS <- lm(mdlA, data=df)

#Partial effects for NB model



# Type 'link' gives the predicted link function x'beta,
head(predict.glm(rslt.NegBin, type="link"))

# Find parameter estimates 
estBeta <- coef(rslt.NegBin)

# Calculate the average partial effects, APE
APE <- mean(exp(predict.glm(rslt.NegBin, type="link")))*estBeta
round(APE, 3)



#### Table to paste

stargazer(APE, summary = FALSE,
          align = TRUE, no.space = TRUE, type="latex")



stargazer(rslt.NegBin$coefficients, APE, rsltOLS$coefficients,
          align=FALSE, no.space=TRUE, intercept.bottom = FALSE, type="text")


stargazer(rslt.NegBin$coefficients, APE, rsltOLS$coefficients,
          align=FALSE, no.space=TRUE, intercept.bottom = FALSE, type="latex")



stargazer(rslt.NegBin, rsltOLS,
          p.auto = FALSE, intercept.bottom = F,
          type = 'latex')
round(APE, 3)
rslt.NegBin$coefficients
