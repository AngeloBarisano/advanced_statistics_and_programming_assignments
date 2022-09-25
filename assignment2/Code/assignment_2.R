# clear environment
rm(list = ls())

# import the necessary libraries
library("tidyverse")
library("stargazer")
library("tidyverse")
library("reshape2")
library("Hmisc")
library("ggplot2")
library("dplyr")
library("moments")
library("lm.beta")
library("fastDummies")
library("stringr")
library("extrafont")
library("gridExtra")
library("AER")
library("DAAG")
loadfonts()




setwd("/home/angelo/Documents/Uni/Courses/Advanced Statistics and programming/Assignments/assignment2")

df <- read.csv("Data/DiD_dataset-1.csv", header = TRUE, sep = ",")

# indicate child or not
df <- df %>%
    mutate(has_children = case_when(
        children > 0 ~ TRUE,
        children == 0 ~ FALSE
    ))

# first set up indicator that whether treatment vs non treatment period
df <- df %>% mutate(dperiod = case_when(year < 1993 ~ 1, year >= 1993 ~ 2))

df$nonwhite <- as.factor(df$nonwhite)
df$state <- as.factor(df$state)
df$year <- as.factor(df$year)

# Create Dummy indicating whether instance has child or not



# Create Indicator that states if before 1993 this instance is in the before treatment
# IMPORTANT! For this task figure out how the data is structured in terms of whether the same instance of women
# is already in the dataset multiple times (like true panel data)


#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Task 2; plot 3 dependent variables;

# a) annual earnings (earn)
ggplot(df, aes(year, earn, group = has_children, color = has_children)) +
    stat_summary(geom = "line", fun = mean) +
    labs(x = "Year", y = "Earnings", color = "Has Children") +
    theme_minimal() +
    geom_vline(xintercept = "1993") +
    theme_set(theme_bw() + theme(legend.position = "bottom")) +
    theme(
        axis.text.x = element_text(size = 20, family = "LM Roman 10"),
        axis.text.y = element_text(size = 20, family = "LM Roman 10"),
        axis.title = element_text(size = 20, family = "LM Roman 10"),
        legend.text = element_text(size = 20, family = "LM Roman 10")
    )

ggsave("Graphics/task2_earn_did.png", width = 11, height = 8)

# b) annual family income (finc)
ggplot(df, aes(year, finc, group = has_children, color = has_children)) +
    stat_summary(geom = "line", fun = mean) +
    labs(x = "Year", y = "Family Income", color = "Has Children") +
    theme_minimal() +
    geom_vline(xintercept = "1993") +
    theme_set(theme_bw() + theme(legend.position = "bottom")) +
    theme(
        axis.text.x = element_text(size = 20, family = "LM Roman 10"),
        axis.text.y = element_text(size = 20, family = "LM Roman 10"),
        axis.title = element_text(size = 20, family = "LM Roman 10"),
        legend.text = element_text(size = 20, family = "LM Roman 10")
    )

ggsave("Graphics/task2_finc_did.png", width = 11, height = 8)

# c) working/non-working (work)
ggplot(df, aes(year, work, group = has_children, color = has_children)) +
    stat_summary(geom = "line", fun = mean) +
    labs(x = "Year", y = "Work Proportion", color = "Has Children") +
    theme_minimal() +
    geom_vline(xintercept = "1993") +
    theme_set(theme_bw() + theme(legend.position = "bottom")) +
    theme(
        axis.text.x = element_text(size = 20, family = "LM Roman 10"),
        axis.text.y = element_text(size = 20, family = "LM Roman 10"),
        axis.title = element_text(size = 20, family = "LM Roman 10"),
        legend.text = element_text(size = 20, family = "LM Roman 10")
    )

ggsave("Graphics/task2_work_did.png", width = 11, height = 8)




#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Task 3
df_temp <- df %>%
    select((c("finc", "earn", "age", "ed", "unearn", "children", "work")))

df_temp_w_children <- subset(
    df, has_children == TRUE
)

df_temp_w_children <- df_temp_w_children %>%
    select((c("finc", "earn", "age", "ed", "unearn", "children", "work")))

df_temp_wo_children <- subset(
    df, has_children == FALSE
)

df_temp_wo_children <- df_temp_wo_children %>%
    select((c("finc", "earn", "age", "ed", "unearn", "children", "work")))

str(df_temp_w_children)


stargazer(
    df_temp,
    type = "latex",
    omit.summary.stat = c("N"),
    summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
    title = "Descriptive Statistics of ECIC",
    covariate.labels = c(
        "Family Income", "Earnings", "Age", "Education",
        "Education Years", "Unearned Income", "Count Children", "Work"
    )
)



stargazer(
    df_temp_w_children,
    type = "latex",
    omit.summary.stat = c("N"),
    summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
    title = "Descriptive Statistics of ECIC; With Children",
    covariate.labels = c(
        "Family Income", "Earnings", "Age", "Education",
        "Education Years", "Unearned Income", "Count Children", "Work"
    )
)

stargazer(
    df_temp_wo_children,
    type = "latex",
    omit.summary.stat = c("N"),
    summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
    title = "Descriptive Statistics of ECIC; Without Children",
    covariate.labels = c(
        "Family Income", "Earnings", "Age", "Education",
        "Education Years", "Unearned Income", "Count Children", "Work"
    )
)


#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Task 4 - create the matrixes

# Find averages per year/dperiod in per earn, finc, work
df_g_summary <- df %>%
    group_by(dperiod, has_children) %>%
    summarise_at(vars(earn, finc, work), funs(mean(., na.rm = TRUE)), round = 2)

df_g_summary_earn <- df %>%
    group_by(dperiod, has_children) %>%
    summarise_at(vars(earn), funs(mean(., na.rm = TRUE)), round = 2)

df_g_summary_finc <- df %>%
    group_by(dperiod, has_children) %>%
    summarise_at(vars(finc), funs(mean(., na.rm = TRUE)), round = 2)

df_g_summary_work <- df %>%
    group_by(dperiod, has_children) %>%
    summarise_at(vars(work), funs(mean(., na.rm = TRUE)), round = 2)

df_g_summary
library(ftExtra)
df_g_summary %>% separate_header(sep = "has_children")

# long to wide transformation
tmp_earn <- dcast(df_g_summary_earn, dperiod ~ has_children, value.var = c("earn"))
tmp_finc <- dcast(df_g_summary_finc, dperiod ~ has_children, value.var = c("finc"))
tmp_work <- dcast(df_g_summary_work, dperiod ~ has_children, value.var = c("work"))


tmp <- merge(tmp_earn, tmp_finc, by.x = "dperiod", by.y = "dperiod")
tmp <- merge(tmp, tmp_work, by.x = "dperiod", by.y = "dperiod")
tmp

tmp <- rbind(tmp, tmp[2, ] - tmp[1, ])
rownames(tmp) <- c("Before", "After", "Difference")
tmp[3, 1] <- NA
tmp


# round the output
tmp[, -1] <- round(tmp[, -1], 2)
tmp

# Make a table with the results
stargazer(tmp, summary = FALSE, align = TRUE, type = "text")

stargazer(tmp, summary = FALSE, align = TRUE, type = "latex")

#' df_g_summary


#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Task 5 -
# Analyze the DiD effect with appropriate regression models for the three
# dependent variables. Present these results in a proper table; and explain
# and interpret your findings. What is the effect of the policy introduction
# on the dependent variables? How does the effect change when adding
# control variables? Also, elaborate on whether robust standard errors seem
# necessary

# i) set up the models
# ii) standardize
# iii) run different models
# iiii) use robust standard errors by clustering on STATE; and apply white standard errors
# iiiii) maybe include year as well
# indicate child or not
df <- df %>%
    mutate(has_children = case_when(
        children > 0 ~ 1,
        children == 0 ~ 0
    ))
df$has_children <- as.factor(df$has_children)


##################
# Earn models
did_earn_sim <- earn ~ has_children + dperiod + has_children:dperiod
did_earn_expand <- earn ~ has_children + dperiod + has_children:dperiod + age + urate + ed + nonwhite

rsltdid_earn_sim <- lm(did_earn_sim, data = df)
rsltdid_earn_expand <- lm(did_earn_expand, data = df)


##################
# finc models
did_finc_sim <- finc ~ has_children + dperiod + has_children:dperiod
did_finc_expand <- finc ~ has_children + dperiod + has_children:dperiod + age + urate + ed + nonwhite

rsltdid_finc_sim <- lm(did_finc_sim, data = df)
rsltdid_finc_expand <- lm(did_finc_expand, data = df)


##################
# work models
did_work_sim <- work ~ has_children + dperiod + has_children:dperiod
did_work_expand <- work ~ has_children + dperiod + has_children:dperiod + age + urate + ed + nonwhite

rsltdid_work_sim <- lm(did_work_sim, data = df)
rsltdid_work_expand <- lm(did_work_expand, data = df)









stargazer(
    rsltdid_earn_sim,
    rsltdid_earn_expand,
    rsltdid_finc_sim,
    rsltdid_finc_expand,
    # rsltdid_work_sim,
    # rsltdid_work_expand,
    intercept.bottom = FALSE,
    align = TRUE,
    no.space = TRUE,
    # omit.labels = "Restaurant IDs?",
    type = "text"
)




stargazer(
    rsltdid_earn_sim,
    rsltdid_earn_expand,
    rsltdid_finc_sim,
    rsltdid_finc_expand,
    rsltdid_work_sim,
    rsltdid_work_expand,
    intercept.bottom = FALSE,
    # align = TRUE,
    no.space = TRUE,
    # omit.labels = "Restaurant IDs?",
    type = "latex"
)







#----------------------------------------
# implement robust standard errors
seBasic <- sqrt(diag(vcov(rsltdid_earn)))
seWhite <- sqrt(diag(vcovHC(rsltdid_earn, type = "HC0")))
seClust <- sqrt(diag(vcovHC(rsltdid_earn, cluster = "state")))







stargazer(rsltdid_earn, rsltdid_earn, rsltdid_earn, se = list(seBasic, seWhite, seClust), type = "text")







#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Task 6 -
# Further examine the presence of effect heterogeneity. Specifically, analyze
# if high-education mothers react differently to the EITC policy measure
# than low education mothers. Define low education as less than nine years
# of education. Explore the following effects of the EITC measure:
# • Single mothers with high education and with children. Compare
# them with single mothers with low education and with children.
# • Single women with low education, without children. Compare them
# with single women with low education, with children.


# first fix the unearn by multiplying by 1000 (because decimals are slipped)
df$unearn <- df$unearn * 1000

# How to define single mothers?
# assumption: if the family income equals the income generated by the women, we have a single women
# additionally: a higher proportion of Uneraned social income means usually single women
# additionally: when unearn == finc this is a sign of
View(df)




# subpoint 1: high education with children vs low education with children
# indicate in the data whether someone is high or low edui
# goal here: impact of education keeping children constant

df <- df %>%
    mutate(edu_lvl = case_when(
        ed >= 9 ~ "high",
        ed < 9 ~ "low"
    ))

# so subset the data so that you only contain women with children
# subset the data
df_has_children <- subset(df, has_children == 1)
View(df_has_children)

##################
# Earn models
did_earn_sim <- earn ~ edu_lvl + dperiod + edu_lvl:dperiod
did_earn_expand <- earn ~ edu_lvl + dperiod + edu_lvl:dperiod + age + urate + nonwhite

rsltdid_earn_sim <- lm(did_earn_sim, data = df_has_children)
rsltdid_earn_expand <- lm(did_earn_expand, data = df_has_children)


##################
# finc models
did_finc_sim <- finc  ~ edu_lvl + dperiod + edu_lvl:dperiod
did_finc_expand <- finc ~ edu_lvl + dperiod + edu_lvl:dperiod + age + urate + nonwhite

rsltdid_finc_sim <- lm(did_finc_sim, data = df_has_children)
rsltdid_finc_expand <- lm(did_finc_expand, data = df_has_children)


##################
# work models
did_work_sim <- work  ~ edu_lvl + dperiod + edu_lvl:dperiod
did_work_expand <- work ~ edu_lvl + dperiod + edu_lvl:dperiod + age + urate + nonwhite

rsltdid_work_sim <- lm(did_work_sim, data = df_has_children)
rsltdid_work_expand <- lm(did_work_expand, data = df_has_children)



stargazer(
    rsltdid_earn_sim,
    rsltdid_earn_expand,
    rsltdid_finc_sim,
    rsltdid_finc_expand,
    # rsltdid_work_sim,
    # rsltdid_work_expand,
    intercept.bottom = FALSE,
    align = TRUE,
    no.space = TRUE,
    # omit.labels = "Restaurant IDs?",
    type = "text"
)


stargazer(
    rsltdid_earn_sim,
    rsltdid_earn_expand,
    rsltdid_finc_sim,
    rsltdid_finc_expand,
    rsltdid_work_sim,
    rsltdid_work_expand,
    intercept.bottom = FALSE,
    # align = TRUE,
    no.space = TRUE,
    # omit.labels = "Restaurant IDs?",
    type = "latex"
)

str(df_has_children)

str(subset(df_has_children, edu_lvl == "low"))



#---------------------------------------------------------------------------------------
# subsection 2: Single women with low education, without children. Compare them
# with single women with low education, with children.
# low educ without children; Low edu with children
# goal here: impact of having children on ECTI keeping education constant

# first classify
df_low_edu <- subset(df, edu_lvl == "low")

View(df_low_edu)



##################
# Earn models
did_earn_sim <- earn ~ has_children + dperiod + has_children:dperiod
did_earn_expand <- earn ~ has_children + dperiod + has_children:dperiod + age + urate + nonwhite

rsltdid_earn_sim <- lm(did_earn_sim, data = df_low_edu)
rsltdid_earn_expand <- lm(did_earn_expand, data = df_low_edu)


##################
# finc models
did_finc_sim <- finc  ~ has_children + dperiod + has_children:dperiod
did_finc_expand <- finc ~ has_children + dperiod + has_children:dperiod + age + urate + nonwhite

rsltdid_finc_sim <- lm(did_finc_sim, data = df_low_edu)
rsltdid_finc_expand <- lm(did_finc_expand, data = df_low_edu)


##################
# work models
did_work_sim <- work ~ has_children + dperiod + has_children:dperiod
did_work_expand <- work ~ has_children + dperiod + has_children:dperiod + age + urate + nonwhite

rsltdid_work_sim <- lm(did_work_sim, data = df_low_edu)
rsltdid_work_expand <- lm(did_work_expand, data = df_low_edu)



stargazer(
    rsltdid_earn_sim,
    rsltdid_earn_expand,
    rsltdid_finc_sim,
    rsltdid_finc_expand,
    # rsltdid_work_sim,
    # rsltdid_work_expand,
    intercept.bottom = FALSE,
    align = TRUE,
    no.space = TRUE,
    # omit.labels = "Restaurant IDs?",
    type = "text"
)


stargazer(
    rsltdid_earn_sim,
    rsltdid_earn_expand,
    rsltdid_finc_sim,
    rsltdid_finc_expand,
    rsltdid_work_sim,
    rsltdid_work_expand,
    intercept.bottom = FALSE,
    # align = TRUE,
    no.space = TRUE,
    # omit.labels = "Restaurant IDs?",
    type = "latex"
)

str(df_low_edu)

str(subset(df_low_edu, has_children == 0))





#######################################################
#######################################################
#######################################################
# SECTION 2


df <- read.csv("Data/IV_dataset.csv", header = TRUE, sep = ",")


# select the relevant variables
df <- df %>% select(age, educ, lnwage, married, qob, SMSA, yob)


#-------------------------------------------------------------
# task 2 summary statistics
# relevant quantiative variables
# age,
# educ (years of education),
# lnwage(weekly earnings),
# marrital status which is categorical;
# quarter of birth of the recorded child;
# SMSA: categorical variable where someone lives (urban vs not urban);
# yob year of birth which is also categorical
df$wage <- exp(df$lnwage)

df_temp <- df %>%
    select((c("age", "educ", "lnwage", "wage")))


stargazer(
    df_temp,
    type = "latex",
    omit.summary.stat = c("N"),
    summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
    title = "Descriptive Statistics of ECIC",
    covariate.labels = c(
        "Age", "Years of Education", "Ln(Wage)", "Wage"
    )
)

#------------------------------------------------------------------
# task 3 run IV reg on ln wage by  education using yob as instrument
# convert yob to factor

# fuirst run the IV model
rslt2SLS.B <- ivreg(lnPacks ~ lnPrice + lnIncome
# yo uuse the bar sign to define the instruments ofr an endogenous variable!!
| lnIncome + TaxDiff + TaxLvl,
data = dfCigarettes95
)

summary(rslt2SLS.B, diagnostics = TRUE)



#------------------------------------------------------------------
# task 4 run IV reg on ln wage by  education using yob as instrument
# convert yob to factor
df$yob <- as.factor(df$yob)

rslt2SLS <-
    ivreg(lnwage ~ educ | educ + yob,
        data = df
    )
summary(rslt2SLS)

rslt2SLS <-
    ivreg(lnwage ~ educ | yob,
        data = df
    )
summary(rslt2SLS)



rslt2SLS <-
    ivreg(lnwage ~ educ | educ + TaxDiff,
        data = dfCigarettes95
    )

educ
summary(rslt2SLS)

stargazer(rslt2SLS, type = "text")



View(df)




rslt2SLS.A <-
    ivreg(lnPacks ~ lnPrice + lnIncome | lnIncome + TaxDiff,
        data = dfCigarettes95
    )
