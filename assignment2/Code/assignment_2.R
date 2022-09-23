# clear environment
rm(list = ls())

# import the necessary libraries
library("tidyverse")
library("stargazer")
library("tidyverse")
library("reshape")
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

df$nonwhite <- as.factor(df$nonwhite)
df$state <- as.factor(df$state)
df$year <- as.factor(df$year)


# indicate child or not
df <- df %>%
    mutate(has_children = case_when(
        children > 0 ~ TRUE,
        children == 0 ~ FALSE
    ))

# first set up indicator that whether treatment vs non treatment period
df <- df %>% mutate(dperiod = case_when(year < 1993 ~ 1, year >= 1993 ~ 2))



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
    geom_vline(xintercept = 1993) +
    theme_set(theme_bw() + theme(legend.position = "bottom")) +
    theme(
        axis.text.x = element_text(size = 17, family = "LM Roman 10"),
        axis.text.y = element_text(size = 17, family = "LM Roman 10"),
        axis.title = element_text(size = 20, family = "LM Roman 10"),
        legend.text = element_text(size = 16, family = "LM Roman 10")
    )

ggsave("Graphics/task2_earn_did.png")

# b) annual family income (finc)
ggplot(df, aes(year, earn, group = has_children, color = has_children)) +
    stat_summary(geom = "line", fun = mean) +
    labs(x = "Year", y = "Family Income", color = "Has Children") +
    theme_minimal() +
    geom_vline(xintercept = 1993) +
    theme_set(theme_bw() + theme(legend.position = "bottom")) +
    theme(
        axis.text.x = element_text(size = 17, family = "LM Roman 10"),
        axis.text.y = element_text(size = 17, family = "LM Roman 10"),
        axis.title = element_text(size = 20, family = "LM Roman 10"),
        legend.text = element_text(size = 16, family = "LM Roman 10")
    )

ggsave("Graphics/task2_finc_did.png")

# c) working/non-working (work)
ggplot(df, aes(year, earn, group = has_children, color = has_children)) +
    stat_summary(geom = "line", fun = mean) +
    labs(x = "Year", y = "Work Proportion", color = "Has Children") +
    theme_minimal() +
    geom_vline(xintercept = 1993) +
    theme_set(theme_bw() + theme(legend.position = "bottom")) +
    theme(
        axis.text.x = element_text(size = 17, family = "LM Roman 10"),
        axis.text.y = element_text(size = 17, family = "LM Roman 10"),
        axis.title = element_text(size = 20, family = "LM Roman 10"),
        legend.text = element_text(size = 16, family = "LM Roman 10")
    )

ggsave("Graphics/task2_work_did.png")




#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Task 3
df_temp <- df %>%
    select((c("finc", "earn", "age", "ed", "unearn", "children", "work")))

stargazer(
    df_temp,
    type = "latex",
    omit.summary.stat = c("N"),
    summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
    title = "Descriptive Statistics of Numeric Indepdenent and Dependent Varaible",
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
    group_by(dperiod) %>%
    summarise_at(vars(work, finc, earn), funs(mean(., na.rm = TRUE)), round = 2)

df_g_summary <- rbind(df_g_summary, df_g_summary[2, ] - df_g_summary[1, ])
rownames(df_g_summary) <- c("Before", "After", "Difference")
df_g_summary[3, 1] <- NA

# round the output
df_g_summary[, -1] <- round(df_g_summary[, -1], 2)


# Make a table with the results
stargazer(df_g_summary, summary = FALSE, align = TRUE, type = "text")

stargazer(df_g_summary, summary = FALSE, align = TRUE, type = "latex")

#' df_g_summary

#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------

# Find averages per period and per state
avgEmpl <- ddply(dfFastfood.long, .(dPeriod, cState), summarise,
    avgEmploy = mean(employ, na.rm = TRUE)
)

# Make table of the outcomes (transpose avgEmpl from long to  wide format with function dcast)
tmp <- dcast(avgEmpl, dPeriod ~ cState, value.var = "avgEmploy")
tmp <- rbind(tmp, tmp[2, ] - tmp[1, ])

rownames(tmp) <- c("Before", "After", "Difference")
tmp[3, "dPeriod"] <- NA


# Make a table with the results
stargazer(tmp, summary = FALSE, align = TRUE, type = "text")
