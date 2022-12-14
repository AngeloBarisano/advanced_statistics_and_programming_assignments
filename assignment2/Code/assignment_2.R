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


df <- df %>%
    mutate(has_children = case_when(
        children > 0 ~ 1,
        children == 0 ~ 0
    ))

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
    # no.space = TRUE,
    # omit.labels = "Restaurant IDs?",
    type = "latex"
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
seBasic <- sqrt(diag(vcov(rsltdid_earn_expand)))
seWhite <- sqrt(diag(vcovHC(rsltdid_earn_expand, type = "HC0")))
seClust <- sqrt(diag(vcovHC(rsltdid_earn_expand, cluster = "state")))

seBasic1 <- sqrt(diag(vcov(rsltdid_finc_expand)))
seWhite1 <- sqrt(diag(vcovHC(rsltdid_finc_expand, type = "HC0")))
seClust1 <- sqrt(diag(vcovHC(rsltdid_finc_expand, cluster = "state")))

seBasic2 <- sqrt(diag(vcov(rsltdid_work_expand)))
seWhite2 <- sqrt(diag(vcovHC(rsltdid_work_expand, type = "HC0")))
seClust2 <- sqrt(diag(vcovHC(rsltdid_work_expand, cluster = "state")))



stargazer(rsltdid_earn_expand, rsltdid_earn_expand, rsltdid_earn_expand, rsltdid_finc_expand, rsltdid_finc_expand, rsltdid_finc_expand, rsltdid_work_expand, rsltdid_work_expand, rsltdid_work_expand, se = list(seBasic, seWhite, seClust, seBasic1, seWhite1, seClust1, seBasic2, seWhite2, seClust2), type = "latex")







#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Task 6 -

df$unearn <- df$unearn * 1000



# subpoint 1: high education with children vs low education with children


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


cor.test(df$qob, y = df$educ)
cor(x = df$qob_fac, y = df$educ)

# first group by year of birth and yuarter of birth, then calcualte the avg lnwage, education for each quarter for each year
df_grouped_yq <- df %>%
    group_by(yob, qob) %>%
    summarise(avg_lnwage_yq = mean(lnwage), avg_yofeduc_yq = mean(educ)) %>%
    mutate(q34 = ((qob == 4) | (qob == 3)))

# View(df_grouped_yq)



ggplot(df_grouped_yq, aes(x = yob, y = avg_yofeduc_yq)) +
    geom_line() +
    geom_label(mapping = aes(label = qob, color = q34)) +
    scale_x_continuous("Year of Birth", breaks = 1930:1940) +
    scale_y_continuous("Years of Education",
        breaks = seq(12.2, 13.2, by = 0.2),
        limits = c(12.2, 13.2)
    ) +
    theme_set(theme_bw() + theme(legend.position = "none")) +
    theme(
        axis.text.x = element_text(size = 18, family = "LM Roman 10"),
        axis.text.y = element_text(size = 18, family = "LM Roman 10"),
        axis.title = element_text(size = 20, family = "LM Roman 10"),
        legend.text = element_text(size = 20, family = "LM Roman 10")
    )

ggsave("Graphics/task3_ivreg_proof.png", width = 11, height = 8)




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




#------------ IMPORTANT FOR PART 4 this is the correct reporting here




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




lmtest::bptest(rsltiv1_sim_numeric)
lmtest::bptest(rsltiv1_sim_numeric_fac)
lmtest::bptest(rsltiv1_adv_numeric)

lmtest::bptest(rsltiv1_adv_numeric_fac)
lmtest::bptest(rsltiv1_adv_numeric_over)
lmtest::bptest(rsltiv1_adv_numeric_fac_over)







# implement robust standard errors
seBasic <- sqrt(diag(vcov(rsltiv1_sim_numeric)))
seWhite <- sqrt(diag(vcovHC(rsltiv1_sim_numeric, type = "HC0")))


seBasic1 <- sqrt(diag(vcov(rsltiv1_adv_numeric)))
seWhite1 <- sqrt(diag(vcovHC(rsltiv1_adv_numeric, type = "HC0")))


seBasic2 <- sqrt(diag(vcov(rsltiv1_adv_numeric_over)))
seWhite2 <- sqrt(diag(vcovHC(rsltiv1_adv_numeric_over, type = "HC0")))




stargazer(rsltiv1_sim_numeric, rsltiv1_sim_numeric, rsltiv1_adv_numeric, rsltiv1_adv_numeric, rsltiv1_adv_numeric_over, rsltiv1_adv_numeric_over, se = list(seBasic, seWhite, seBasic1, seWhite1, seBasic2, seWhite2), type = "latex")



qplot(sample = rsltiv1_sim_numeric$residuals) + theme_classic()
grid.arrange(p1, p2, nrow = 1)


ggsave("Graphics/qqplots.png")

qqplot.income_hist <- qplot(sample = CASchools$income, stat = "qq")
qqplot.income_hist









#------------------------------------------------------------------
# task 5

# first run normal OLS
# endogeneous educ
# i) run basic OLS
rsltOLS_sim <- lm(lnwage ~ educ, data = df)

# ii) run OLS with controls - dont run hand made ivreg
rsltOLS_adv <- lm(lnwage ~ educ + SMSA + married, data = df)



# THen run the Two stage OLS manually

# 1.first with qob numeric
educ.hat <-
    fitted(lm(educ ~ qob, data = df))

rslt2sls_manual_sim <-
    lm(lnwage ~ educ.hat, data = df)

# 2.now do with covariates
educ.hat <-
    fitted(lm(educ ~ qob + SMSA + married, data = df))

rslt2sls_manual_adv <-
    lm(lnwage ~ educ.hat + SMSA + married, data = df)



# 3. then qob as factor
educ.hat <-
    fitted(lm(educ ~ qob_fac, data = df))

rslt2sls_manual_sim_fac <-
    lm(lnwage ~ educ.hat, data = df)


# 4. now do with covariates
educ.hat <-
    fitted(lm(educ ~ qob_fac + SMSA + married, data = df))

rslt2sls_manual_adv_fac <-
    lm(lnwage ~ educ.hat + SMSA + married, data = df)





stargazer(
    rsltOLS_sim,
    rsltOLS_adv,
    rslt2sls_manual_sim,
    rslt2sls_manual_adv,
    rslt2sls_manual_sim_fac,
    rslt2sls_manual_adv_fac,
    intercept.bottom = FALSE,
    #   align = TRUE,
    no.space = TRUE,
    type = "latex"
)


## perform Weak isntrument, Hasuamn, and sargan test


library(xtable)

summary(rsltiv1_adv_numeric, diagnostics = TRUE)
summary(rsltiv1_adv_numeric_fac, diagnostics = TRUE)
summary(rsltiv1_adv_numeric_over, diagnostics = TRUE)


# you need xtable
saa <- summary(rsltiv1_adv_numeric, diagnostics = TRUE)
saasaa <- summary(rsltiv1_adv_numeric_fac, diagnostics = TRUE)
saasaaa <- summary(rsltiv1_adv_numeric_over, diagnostics = TRUE)

statstable <- xtable(as.data.frame((saa$diagnostics)))
statstable2 <- xtable(as.data.frame((saasaa$diagnostics)))
statstable3 <- xtable(as.data.frame((saasaaa$diagnostics)))
print(statstable)
