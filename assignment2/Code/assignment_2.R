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
        children > 0 ~ 1,
        children == 0 ~ 0
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
    select((c("finc", "earn", "age", "urate", "ed", "unearn", "children", "work")))

df_temp_w_children <- subset(
    df, has_children == TRUE
)

df_temp_w_children <- df_temp_w_children %>%
    select((c("finc", "earn", "age", "urate", "ed", "unearn", "children", "work")))

df_temp_wo_children <- subset(
    df, has_children == FALSE
)
View(df)
df_temp_wo_children <- df_temp_wo_children %>%
    select((c("finc", "earn", "age", "ed", "urate", "unearn", "children", "work")))

str(df_temp_w_children)


stargazer(
    df,
    type = "latex",
    # omit.summary.stat = c("N"),
    summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
    title = "Descriptive Statistics of ECIC"
    # covariate.labels = c(
    #     "Family Income", "Earnings", "Age", "Education",
    #     "Education Years", "Unearned Income", "Count Children", "Work"
    # )
)
skewness(df$earn)
df %>% count(has_children)
table(df$nonwhite)
cor(df$earn, df$finc, )
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
cov_earn <- earn ~ age + urate + ed + nonwhite

did_earn_sim <- earn ~ has_children + dperiod + has_children:dperiod
did_earn_expand <- earn ~ has_children + dperiod + has_children:dperiod + age + urate + ed + nonwhite

rslt_earn_cov <- lm(cov_earn, data = df)
rsltdid_earn_sim <- lm(did_earn_sim, data = df)
rsltdid_earn_expand <- lm(did_earn_expand, data = df)


##################
# finc models
cov_finc <- finc ~ age + urate + ed + nonwhite

did_finc_sim <- finc ~ has_children + dperiod + has_children:dperiod
did_finc_expand <- finc ~ has_children + dperiod + has_children:dperiod + age + urate + ed + nonwhite

rslt_finc_cov <- lm(cov_finc, data = df)
rsltdid_finc_sim <- lm(did_finc_sim, data = df)
rsltdid_finc_expand <- lm(did_finc_expand, data = df)


##################
# work models
cov_work <- work ~ age + urate + ed + nonwhite

did_work_sim <- work ~ has_children + dperiod + has_children:dperiod
did_work_expand <- work ~ has_children + dperiod + has_children:dperiod + age + urate + ed + nonwhite

rslt_work_cov <- lm(cov_work, data = df)
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
    rslt_earn_cov,
    rsltdid_earn_sim,
    rsltdid_earn_expand,
    rslt_finc_cov,
    rsltdid_finc_sim,
    rsltdid_finc_expand,
    rslt_work_cov,
    rsltdid_work_sim,
    rsltdid_work_expand,
    intercept.bottom = FALSE,
    # align = TRUE,
    no.space = TRUE,
    # omit.labels = "Restaurant IDs?",
    type = "text"
)



#----------------------------------------

lmtest::bptest(rslt_earn_cov)
lmtest::bptest(rsltdid_earn_sim)
lmtest::bptest(rsltdid_earn_expand)

lmtest::bptest(rslt_finc_cov)
lmtest::bptest(rsltdid_finc_sim)
lmtest::bptest(rsltdid_finc_expand)

lmtest::bptest(rslt_work_cov)
lmtest::bptest(rsltdid_work_sim)
lmtest::bptest(rsltdid_work_expand)








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
        ed >= 9 ~ 1,
        ed < 9 ~ 0
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
    rsltdid_work_sim,
    rsltdid_work_expand,
    intercept.bottom = FALSE,
    align = TRUE,
    no.space = TRUE,
    # omit.labels = "Restaurant IDs?",
    type = "text",
    report = ("vc*p")
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

str(subset(df_has_children, edu_lvl == "high"))



#---------------------------------------------------------------------------------------
# subsection 2: Single women with low education, without children. Compare them
# with single women with low education, with children.
# low educ without children; Low edu with children
# goal here: impact of having children on ECTI keeping education constant

# first classify
df_low_edu <- subset(df, edu_lvl == 0)

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
    rsltdid_work_sim,
    rsltdid_work_expand,
    intercept.bottom = FALSE,
    align = TRUE,
    no.space = TRUE,
    # omit.labels = "Restaurant IDs?",
    type = "text",
    report = ("vc*p")
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

str(subset(df_low_edu, has_children == 1))





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

View(df)
stargazer(
    df,
    type = "latex",
    omit.summary.stat = c("N"),
    summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
    title = "Descriptive Statistics of ECIC"
    # covariate.labels = c(
    #     "Age", "Years of Education", "Ln(Wage)", "Wage"
    # )
)

table(df$qob)
#------------------------------------------------------------------
# task 3 run IV reg on ln wage by  education using yob as instrument
# convert yob to factor

df$yob_fac <- as.factor(df$yob)
df$qob_fac <- as.factor(df$qob)

# fuirst run the IV model
# rslt2SLS.B <- ivreg(lnPacks ~ lnPrice + lnIncome
# # yo uuse the bar sign to define the instruments ofr an endogenous variable!!
# | lnIncome + TaxDiff + TaxLvl,
# data = dfCigarettes95
# )

# summary(rslt2SLS.B, diagnostics = TRUE)

# first group by year of birth and yuarter of birth, then calcualte the avg lnwage, education for each quarter for each year
df_grouped_yq <- df %>%
    group_by(yob, qob) %>%
    summarise(avg_lnwage_yq = mean(lnwage), avg_yofeduc_yq = mean(educ))
View(df_grouped_yq)

ggplot(df_grouped_yq, aes(x = yob + (qob - 1) / 4, y = avg_yofeduc_yq)) +
    geom_line()



ak_age <- df %>%
    group_by(qob, yob) %>%
    summarise(lnw = mean(lnwage), s = mean(educ)) %>%
    mutate(q4 = (qob == 4))



ggplot(ak_age, aes(x = yob + (qob - 1) / 4, y = s)) +
    geom_line() +
    geom_label(mapping = aes(label = qob, color = q4)) +
    theme(legend.position = "none") +
    scale_x_continuous("Year of birth", breaks = 1930:1940) +
    scale_y_continuous("Years of Education",
        breaks = seq(12.2, 13.2, by = 0.2),
        limits = c(12.2, 13.2)
    )




# just make a similar plot which groups by year: so each year we will indicate where each
# quarter is: like the one on the right just each year grouped;
# so: calculate the mean Years of education per quarter per year and then plot that.





# simple
first_stage_check_sim <- lm(educ ~ qob, data = df)

# advanced
first_stage_check_adv <- lm(educ ~ qob + age + married, data = df)

# simple fac
first_stage_check_sim_fac <- lm(educ ~ qob_fac, data = df)

# advanced fac
first_stage_check_adv_fac <- lm(educ ~ qob_fac + age + married, data = df)



stargazer(
    first_stage_check_sim,
    first_stage_check_adv,
    first_stage_check_sim_fac,
    first_stage_check_adv_fac,
    intercept.bottom = FALSE,
    #   align = TRUE,
    no.space = TRUE,
    type = "text",
    report = ("vc*p")
)


#------------------------------------------------------------------
# task 4 run IV reg on ln wage by  education using yob as instrument



# rsltOLS5.1 <- ivreg(lnwage ~ educ | qob, data = df_IV)
# rsltOLS5.2 <- ivreg(lnwage ~ educ + married | married + qob, data = df_IV)
# rsltOLS5.3 <- ivreg(lnwage ~ educ + married + age | married + qob + age, data = df_IV)
# summary(rsltOLS5.2, diagnostics = TRUE)
# stargazer(rsltOLS5.2, type = "text")
# bptest(rsltOLS5.2)

# i) run basic OLS
rsltOLS_sim <- lm(lnwage ~ educ, data = df)

# ii) run OLS with controls - dont run hand made ivreg
rsltOLS_adv <- lm(lnwage ~ educ + age + married, data = df)

# iii) run Simple IVREG
rsltiv1_sim <- ivreg(lnwage ~ educ | qob, data = df)

# iiii) run more complex IV reg with more controls
rsltiv1_adv <- ivreg(lnwage ~ educ + age + married | qob + age + married, data = df)

# iiiii) run iv reg with out controls BUT ALSO MORE THAN oNE INSTURMENT FOR PART 5!
rsltiv2_sim <- ivreg(lnwage ~ educ | qob_fac, data = df)

# iiiiiii) run ivreg with controls and multiple instruments (qob + yob)
rsltiv2_adv <- ivreg(lnwage ~ educ + age + married | qob_fac + yob_fac + age + married, data = df)



stargazer(
    rsltOLS_sim,
    rsltOLS_adv,
    rsltiv1_sim,
    rsltiv1_adv,
    rsltiv2_sim,
    rsltiv2_adv,
    intercept.bottom = FALSE,
    #   align = TRUE,
    no.space = TRUE,
    type = "text"
)





#------------ IMPORTANT FOR PART 4 this is the correct reporting here


# i) run basic OLS
rsltOLS_sim <- lm(lnwage ~ educ, data = df)

# ii) run OLS with controls - dont run hand made ivreg
rsltOLS_adv <- lm(lnwage ~ educ + age + married, data = df)

# iii) run Simple IVREG - numeric
rsltiv1_sim_numeric <- ivreg(lnwage ~ educ | qob, data = df)

# iii) run Simple IVREG - fac
rsltiv1_sim_numeric_fac <- ivreg(lnwage ~ educ | qob_fac, data = df)

# iii) run adv IVREG - numeric
rsltiv1_adv_numeric <- ivreg(lnwage ~ educ + SMSA + married | qob + SMSA + married, data = df)

# iii) run adv IVREG - fac
rsltiv1_adv_numeric_fac <- ivreg(lnwage ~ educ + SMSA + married | qob_fac + SMSA + married, data = df)

# now overidentify
rsltiv1_adv_numeric_over <- ivreg(lnwage ~ educ + SMSA + married | qob + yob + SMSA + married, data = df)

# iii) run adv IVREG - fac
rsltiv1_adv_numeric_fac_over <- ivreg(lnwage ~ educ + SMSA + married | qob_fac + yob + SMSA + married, data = df)




stargazer(
    rsltOLS_sim,
    rsltOLS_adv,
    rsltiv1_sim_numeric,
    rsltiv1_sim_numeric_fac,
    rsltiv1_adv_numeric,
    rsltiv1_adv_numeric_fac,
    rsltiv1_adv_numeric_over,
    rsltiv1_adv_numeric_fac_over,
    intercept.bottom = FALSE,
    #   align = TRUE,
    no.space = TRUE,
    type = "text"
)


stargazer(
    rsltOLS_sim,
    rsltOLS_adv,
    rsltiv1_sim_numeric,
    rsltiv1_sim_numeric_fac,
    rsltiv1_adv_numeric,
    rsltiv1_adv_numeric_fac,
    rsltiv1_adv_numeric_over,
    rsltiv1_adv_numeric_fac_over,
    intercept.bottom = FALSE,
    #   align = TRUE,
    no.space = TRUE,
    type = "latex"
)



bptest(rsltOLS5.2)
