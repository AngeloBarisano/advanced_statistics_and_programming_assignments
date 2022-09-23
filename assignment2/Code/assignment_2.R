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


df <- df %>%
    mutate(has_children = children >= 1)
View(df)
# drop duplicates?


# Create Dummy indicating whether instance has child or not



# Create Indicator that states if before 1993 this instance is in the before treatment
# IMPORTANT! For this task figure out how the data is structured in terms of whether the same instance of women
# is already in the dataset multiple times (like true panel data)


#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Task 2; plot 3 dependent variables;
# initial prep
# i) group by year and take the mean
df_g_summary <- df %>%
    group_by(year) %>%
    summarise_at(vars(work, finc, earn), funs(mean(., na.rm = TRUE)))

# a) annual earnings (earn)
ggplot(df, aes(year, earn)) +
    stat_summary(geom = "line", fun = mean) +
    labs(x = "Year", y = "Earnings") +
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
ggplot(df, aes(year, finc)) +
    stat_summary(geom = "line", fun = mean) +
    labs(x = "Year", y = "Family Income") +
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
ggplot(df, aes(year, work)) +
    stat_summary(geom = "line", fun = mean) +
    labs(x = "Year", y = "Work") +
    geom_vline(xintercept = 1993) +
    theme_set(theme_bw() + theme(legend.position = "bottom")) +
    theme(
        axis.text.x = element_text(size = 17, family = "LM Roman 10"),
        axis.text.y = element_text(size = 17, family = "LM Roman 10"),
        axis.title = element_text(size = 20, family = "LM Roman 10"),
        legend.text = element_text(size = 16, family = "LM Roman 10")
    )

ggsave("Graphics/task2_work_did.png")



# d) working/non-working (work) --> COUNTERARGUMENT USING SUBSETS
ggplot(df, aes(year, work, color = "has_children"), cex.lab = 30) +
    labs(x = "Year", y = "Work", color = "has_children") +
    stat_summary(geom = "line", fun = mean) +
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

stargazer(
    df,
    type = "text",
    omit.summary.stat = c("N"),
    summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
    title = "Descriptive Statistics of Numeric Indepdenent and Dependent Varaible"
)


#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
# Task 4 - create the matrixes
# first set up indicator that whether treatment vs non treatment period
df <- df %>% mutate(df, dperiod = case_when(year < 1993 ~ 1, year >= 1993 ~ 2))
df

# Find averages per year/dperiod in per earn, finc, work

df_g_summary <- df %>%
    group_by(dperiod) %>%
    summarise_at(vars(work, finc, earn), funs(mean(., na.rm = TRUE)))

df_g_summary <- rbind(df_g_summary, df_g_summary[2, ] - df_g_summary[1, ])

rownames(df_g_summary) <- c("Before", "After", "Difference")


df_g_summary






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
