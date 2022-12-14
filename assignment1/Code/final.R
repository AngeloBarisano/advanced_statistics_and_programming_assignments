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
library("gtsummary")
library("stringr")
library("extrafont")
library("gridExtra")
library("AER")
library("DAAG")
loadfonts()




setwd("/home/angelo/Documents/Uni/Courses/Advanced Statistics and programming/Assignments/assignment1")

df <- read.csv("Data/train.csv", header = TRUE, sep = ",")



# display missing values by column
colSums(is.na(df))

# select all columns which have missing data
which(colSums(is.na(df)) > 0)









df_ <- df[, c(
    "Id",
    "SalePrice",
    "MoSold",
    "YrSold",
    "YearRemodAdd",
    "LotArea",
    "GrLivArea",
    "TotalBsmtSF",
    "BldgType",
    "MSZoning",
    "Neighborhood",
    "OverallQual"
)]


# create the interesting varaibles
df_$tot_liv_area <- df_$GrLivArea + df_$TotalBsmtSF

# time since remodeling at year of sale in years
df_$y_since_rem <- df_$YrSold - df_$YearRemodAdd



# adjust for family homes
# the distinction made here is simply that:
# stand alone house vs multiple houses together
match_df <- data.frame(
    old = c("1Fam", "2fmCon", "Duplex", "Twnhs", "TwnhsE"),
    new = c(
        "Single Family Home",
        "Multi-Unit Homes",
        "Multi-Unit Homes",
        "Multi-Unit Homes",
        "Multi-Unit Homes"
    )
)

df_ <-
    df_ %>% mutate(Building_type = match_df$new[match(BldgType, match_df$old)])



# group zoning
#' I have decided to leave the groups as small groups sizes are only a problem for anova
#' https://stats.stackexchange.com/questions/219071/sample-size-of-the-levels-of-a-categorical-variables -- CITE!
#' https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0229345
#' Additionally: the plot shows that even if these groups are small, they do not scatter that much
match_df <- data.frame(
    old = c("C (all)", "FV", "RH", "RL", "RM"),
    new = c(
        "Commercial",
        "Floating Village",
        "High Density",
        "Low Density",
        "Moderate Density"
    )
)

df_ <-
    df_ %>% mutate(MSZoning = match_df$new[match(MSZoning, match_df$old)])

#' however, further analysis will show that these two groups are quite comparable; so for the purpose of this analysis they are merged as well
#' Commercial and FV are too different to even be considered merged
match_df <- data.frame(
    old = c("Commercial", "Floating Village", "High Density", "Low Density", "Moderate Density"),
    new = c(
        "Commercial",
        "Floating Village",
        "Moderate2High Density",
        "Low Density",
        "Moderate2High Density"
    )
)

df_ <-
    df_ %>% mutate(MSZoning_gr = match_df$new[match(MSZoning, match_df$old)])

# fix y_since_rem
df[df$y_since_rem < 0, "y_since_rem"] <- 0








table_out <- table(df_$OverallQual_cat)
# Add cumFreq and proportions
table_out <- transform(table_out, cumFreq = cumsum(Freq), relative = prop.table(Freq))

#' based on the resulting table, the Quality variables will be split into
#' 3 groups; these groups will not be equally distributed, however, the size of the underlyign groups tend to be somewhat simialr
#' Furthermore: one would expect that about 67% of respondents are in the one std region
#' Thus, we assume that the distribution somewhat is according that of a normal distributi
table_out$cumsum_freq <- cumsum(table_out$relative)
table_out


match_df <- data.frame(
    old = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    new = c(
        "Low",
        "Low",
        "Low",
        "Low",
        "Medium",
        "Medium",
        "Medium",
        "High",
        "High",
        "High"
    )
)

df_ <-
    df_ %>% mutate(OverallQual_grouped = match_df$new[match(OverallQual, match_df$old)])



# adjust the scale of saleprice for easier handling
df_$SalePrice <- df_$SalePrice / 1000

# to make it clear that only certain operations can be performed on the data
# ID should be a character as string/factor operations are resptrictive by design
# i.e. no mathematical operations
df_$Id <- as.character(df_$Id)

# The month sold shoul also be considered a factor for the same reason before
df_$MoSold <- as.factor(df_$MoSold)
df_$YrSold <- as.factor(df_$YrSold)

# these are just standard variables to be converted to factors
df_$Neighborhood <- as.factor(df_$Neighborhood)
df_$BldgType <- as.factor(df_$BldgType)
df_$MSZoning <- as.factor(df_$MSZoning)

# OverallQual can be interpreted as both categorical and numeric; It wil be further elaobrated upon
df_$OverallQual_cat <- as.factor(df_$OverallQual)


# drop some columns which are now no logner needed
df_ <- df_[, !(names(df_) %in% c("YearRemodAdd", "TotalBsmtSF", "GrLivArea"))]

str(df_)




# display missing values by column
colSums(is.na(df_))


# select all columns which have missing data
which(colSums(is.na(df_)) > 0)







# Task 1

df_temp <- df_ %>%
    select(-one_of(c("Id", "MoSold", "YrSold", "BldgType")))
print(skewness(df_$SalePrice))
print(skewness(df_$tot_liv_area))

stargazer(
    df_temp,
    type = "text",
    omit.summary.stat = c("N"),
    summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
    title = "Descriptive Statistics of Numeric Indepdenent and Dependent Varaible",
    covariate.labels =
        c(
            "SalePrice", "Lot Area", "Quality", "Condition",
            "Total Living Space", "Years Since Remodeling"
        )
    # , initial.zero = F
    # ,single.row=TRUE)
)

stargazer(
    df_temp,
    # type = 'text',
    omit.summary.stat = c("N"),
    summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
    title = "Descriptive Statistics of Numeric Indepdenent and Dependent Varaible",
    covariate.labels =
        c(
            "SalePrice", "Lot Area", "Quality", "Condition",
            "Total Living Space", "Years Since Remodeling"
        )
    # , initial.zero = F
    # ,single.row=TRUE)
)


# plot 1
# livingSpace by SalePrice
scatter <- ggplot(df_, aes(tot_liv_area, SalePrice))
scatter + geom_point() + geom_smooth(method = "lm", color = "Red") + geom_smooth() + labs(x = "Living space in square feet", y = "Sale Price in 1000s") + xlim(0, 7000) + theme(
    axis.text.x = element_text(size = 17, family = "LM Roman 10"),
    axis.text.y = element_text(size = 17, family = "LM Roman 10"),
    axis.title = element_text(size = 20, family = "LM Roman 10")
)
#+ ggtitle("Sale Price by Living Space in square feet")
ggsave("h1.jpg")
#' Add Descritption: Two outliers left out for representative reasons

# smooth the plot

scatter <- ggplot(df_, aes(tot_liv_area, SalePrice))
scatter + geom_point() + geom_smooth() + labs(x = "Living space in square feet", y = "Sale Price in 1000s") + xlim(0, 7000)

#' Hypothesis 1: Total Living Space (IV) has a positive effect (potitive association with) on Sales Price (DV)







# plot 2
# Only inspect high density & Medium density to see if they ar compatible
scatter <-
    ggplot(subset(df_, MSZoning == "High Density" | MSZoning == "Moderate Density"), aes(tot_liv_area, SalePrice, colour = MSZoning))

scatter + geom_point() + geom_smooth(method = "lm", aes(fill = "MSZoning"), alpha = 0.1) + labs(x = "Living space in square feet", y = "Sale Price in 1000s", color = "MSZoning") #+ggtitle("Sale Price by Zone")
#' interestingly: Moderate and High density appear to be quite comparable


bar <- ggplot(df_, aes(MSZoning, SalePrice))

bar + stat_summary(
    fun = mean,
    geom = "bar",
    fill = "white",
    colour = "Black"
) + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Zoning", y = "Sale Price")



# plot 3

scatter <-
    ggplot(df_, aes(tot_liv_area, SalePrice, colour = MSZoning))
scatter + geom_point() + geom_smooth(method = "lm", alpha = 0.1) + labs(x = "Living space in square feet", y = "Sale Price in 1000s") + xlim(0, 7000) + theme_set(theme_bw() + theme(legend.position = "bottom")) + theme(
    axis.text.x = element_text(size = 15, family = "LM Roman 10"),
    axis.text.y = element_text(size = 15, family = "LM Roman 10"),
    axis.title = element_text(size = 20, family = "LM Roman 10"),
    legend.text = element_text(size = 14, family = "LM Roman 10")
)


#+ ggtitle("Sale Price by Living Space in square feet subsectioned by Zone")
ggsave("h2.1.jpg")



# Now obseve the groups we identified before

scatter <-
    ggplot(df_,
        aes(tot_liv_area, SalePrice, colour = MSZoning_gr),
        cex.lab = 30
    )
scatter + geom_point() + geom_smooth(method = "lm", alpha = 0.1) + labs(x = "Living space in square feet", y = "Sale Price in 1000s", color = "MSZoning_gr") + xlim(0, 7000) + theme_set(theme_bw() + theme(legend.position = "bottom")) + theme(
    axis.text.x = element_text(size = 17, family = "LM Roman 10"),
    axis.text.y = element_text(size = 17, family = "LM Roman 10"),
    axis.title = element_text(size = 20, family = "LM Roman 10"),
    legend.text = element_text(size = 16, family = "LM Roman 10")
)

ggsave("h2.2.jpg")



# Subanalysis; Neighborhoods & Clusters

scatter <- ggplot(df_, aes(tot_liv_area, SalePrice))
scatter + geom_point() + geom_smooth(method = "lm", color = "Red") + labs(x = "Living space in square feet", y = "Sale Price in 1000s")


scatter <- ggplot(df_, aes(tot_liv_area, SalePrice, colour = Neighborhood))
scatter + geom_point() + geom_smooth(method = "lm", aes(fill = "Neighborhood"), alpha = 0.1) + labs(x = "Living space in square feet", y = "Sale Price in 1000s", color = "Neighborhood")


scatter <- ggplot(df_, aes(tot_liv_area, SalePrice))
scatter + geom_point() + geom_smooth(method = "lm", color = "Red") + labs(x = "Living space in square feet", y = "Sale Price in 1000s") + xlim(0, 7000)


scatter <- ggplot(df_, aes(tot_liv_area, SalePrice, colour = Neighborhood))
scatter + geom_point() + geom_smooth(method = "lm", aes(fill = "Neighborhood"), alpha = 0.1) + labs(x = "Living space in square feet", y = "Sale Price in 1000s", color = "Neighborhood") + xlim(0, 7000)

df_subset <-
    subset(
        df_,
        Neighborhood == "NoRidge" |
            Neighborhood == "OldTown" |
            Neighborhood == "CollgCr" |
            Neighborhood == "NridgHt"
    )

scatter <- ggplot(df_subset, aes(tot_liv_area, SalePrice))
scatter + geom_point() + geom_smooth(method = "lm", color = "Red") + labs(x = "Living space in square feet", y = "Sale Price in 1000s") + xlim(0, 7000)


scatter <- ggplot(df_subset, aes(tot_liv_area, SalePrice, colour = Neighborhood))
scatter + geom_point() + geom_smooth(method = "lm", aes(fill = "Neighborhood"), alpha = 0.1) + labs(x = "Living space in square feet", y = "Sale Price in 1000s", color = "Neighborhood") + xlim(0, 7000)



# plot 4

#' the association betwen the state of the house (in terms of qualtiy) and the years since it was renovated/remodeled
#' is clear considering the first plot
scatter <- ggplot(df_, aes(y_since_rem, SalePrice))
scatter + geom_point() + geom_smooth(method = "lm", aes(fill = "y_since_rem"), alpha = 0.4) + labs(x = "Time Since last Remodeling at point of sale", y = "Sale Price in 1000s")
#' Increasing cears since the house was remodeled (at time of sale of house) has a negative association with Sale price; as is to be expected


ggplot(df_, aes(x = OverallQual_cat, y = SalePrice)) +
    geom_boxplot() +
    xlab("Quality of the property") +
    ylab("Sale Price in 1000s") +
    labs(size = "Years Since Remodeling")





ggplot(df_, aes(x = OverallQual, y = SalePrice)) +
    geom_point(aes(size = y_since_rem)) +
    xlab("Quality of the property") +
    ylab("Sale Price in 1000s") +
    theme_set(theme_bw() + theme(legend.position = "bottom")) +
    labs(size = "Years Since Remodeling", family = "LM Roman 10") +
    theme(
        text = element_text(family = "LM Roman 10"),
        axis.text.x = element_text(size = 17, family = "LM Roman 10"),
        axis.text.y = element_text(size = 17, family = "LM Roman 10"),
        axis.title = element_text(size = 20, family = "LM Roman 10"),
        legend.text = element_text(size = 16, family = "LM Roman 10")
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks())

ggsave(file = "supplement_appendix_Quality_yearssinceremodeling_sale.jpg")



# this is the hypothesis plot
scatter <- ggplot(df_, aes(y_since_rem, SalePrice))
scatter + geom_point() + geom_smooth(method = "lm", alpha = 0.4) + theme_set(theme_bw() + theme(legend.position = "bottom")) + labs(x = "Time Since last Remodeling at point of sale", y = "Sale Price in 1000s", fill = "Years Since Remodeling") + theme(
    text = element_text(family = "LM Roman 10"),
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17),
    axis.title = element_text(size = 20)
)
#+ ggtitle("Sale Price by Years Since Remodeling before sale year")
ggsave(file = "h3.jpg")


#' This shows that this variable is a great control for the quality of the house and its condition

# As such: Years since remodeling have a negative impact on sales price

# Zone on Building type

# the interesting question here is: are stand alone family homes more valuable? than multi-unit homes

df_$MSZoning
table(df_$BldgType)


bar <- ggplot(df_, aes(BldgType, SalePrice))
bar + stat_summary(
    fun = mean,
    geom = "bar",
    fill = "white",
    colour = "Black"
) + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + facet_wrap(~MSZoning) + labs(x = "BldgType", y = "Sale Price")










##############
# Regressions
###########

# create the dummies
OverallQual_grouped_dummies <- fastDummies::dummy_cols(df_$OverallQual)
# add index column to data frame
OverallQual_grouped_dummies <- tibble::rowid_to_column(OverallQual_grouped_dummies, "Id")


df_y <- df_[c("Id", "SalePrice")]


# now merge the Y and the dummies for this regression; so that the normal regression equation can be done
jointdataset <- merge(df_y, OverallQual_grouped_dummies, by = "Id")

# now run the regression compared to the lowest category and look whether there are natural steps between the variabels
quality_regression_checkup <-
    lm(
        SalePrice ~ 1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6 + .data_7 + .data_8 + .data_9 + .data_10,
        data = jointdataset
    )

stargazer(
    quality_regression_checkup,
    type = "text"
)

coef(summary(quality_regression_checkup))[, 1:2]





# create dummies
df_$low_density_zone <-
    ifelse(df_$MSZoning_gr == "Low Density", 1, 0)

df_$hig_med_density_zone <-
    ifelse(df_$MSZoning_gr == "Moderate2High Density", 1, 0)

df_$commercial_zone <-
    ifelse(df_$MSZoning_gr == "Commercial", 1, 0)

df_$floating_zone <-
    ifelse(df_$MSZoning_gr == "Floating Village", 1, 0)



df_$ln_SalePrice <- log(df_$SalePrice)
table(df_$MSZoning_gr)

# main model
main_multi_var_model <-
    lm(
        SalePrice ~ 1 + tot_liv_area + y_since_rem + low_density_zone + commercial_zone + floating_zone,
        data = df_
    )

main_multi_var_model.beta <- lm.beta(main_multi_var_model)



# With interaction adn quadrartic terms
mulit_var_model <-
    lm(
        SalePrice ~ 1 + tot_liv_area + y_since_rem + low_density_zone + commercial_zone + floating_zone + I(tot_liv_area^2) + tot_liv_area * low_density_zone + tot_liv_area * commercial_zone + tot_liv_area * floating_zone,
        data = df_
    )

mulit_var_model.beta <- lm.beta(mulit_var_model)


# with covariates and confounders and control
multi_var_model_w_confounders <-
    lm(
        SalePrice ~ 1 + tot_liv_area + y_since_rem + low_density_zone + commercial_zone + floating_zone + LotArea + YrSold + OverallQual_cat + Building_type,
        data = df_
    )
multi_var_model_w_confounders.beta <-
    lm.beta(multi_var_model_w_confounders)



# with interaction and covariates and confounders and control
# logscale SalePrice!
multi_var_model_w_confounders_ninteraction <-
    lm(
        ln_SalePrice ~ 1 + tot_liv_area + y_since_rem + low_density_zone + commercial_zone + floating_zone + I(tot_liv_area^
            2) + tot_liv_area * low_density_zone + tot_liv_area * commercial_zone + tot_liv_area * floating_zone + LotArea + YrSold + OverallQual_cat + Building_type + tot_liv_area * LotArea,
        data = df_
    )
multi_var_model_w_confounders_ninteraction.beta <-
    lm.beta(multi_var_model_w_confounders_ninteraction)


# not logscale the outcome but everythign else
multi_var_model_w_confounders_ninteraction_unscaled <-
    lm(
        SalePrice ~ 1 + tot_liv_area + y_since_rem + low_density_zone + commercial_zone + floating_zone + tot_liv_area * low_density_zone + tot_liv_area * commercial_zone + tot_liv_area * floating_zone + LotArea + YrSold + OverallQual_cat + Building_type + LotArea *
            tot_liv_area,
        data = df_
    )
multi_var_model_w_confounders_ninteraction_unscaled.beta <-
    lm.beta(multi_var_model_w_confounders_ninteraction_unscaled)






stargazer(
    multi_var_model_w_confounders,
    multi_var_model_w_confounders.beta,
    multi_var_model_w_confounders_ninteraction_unscaled,
    multi_var_model_w_confounders_ninteraction_unscaled.beta,
    multi_var_model_w_confounders_ninteraction,
    multi_var_model_w_confounders_ninteraction.beta,
    coef = list(
        multi_var_model_w_confounders$coefficients,
        multi_var_model_w_confounders.beta$standardized.coefficients,
        multi_var_model_w_confounders_ninteraction_unscaled$coefficients,
        multi_var_model_w_confounders_ninteraction_unscaled.beta$standardized.coefficients,
        multi_var_model_w_confounders_ninteraction$coefficients,
        multi_var_model_w_confounders_ninteraction.beta$standardized.coefficients
    ),
    p = list(
        coef(summary(multi_var_model_w_confounders))[, 4],
        coef(summary(multi_var_model_w_confounders.beta))[, 5],
        coef(
            summary(multi_var_model_w_confounders_ninteraction_unscaled)
        )[, 4],
        coef(
            summary(multi_var_model_w_confounders_ninteraction_unscaled.beta)
        )[, 5],
        coef(summary(
            multi_var_model_w_confounders_ninteraction
        ))[, 4],
        coef(summary(
            multi_var_model_w_confounders_ninteraction.beta
        ))[, 5]
    ),
    type = "text",
    omit = c(
        "YrSold2008",
        "YrSold2009",
        "YrSold2010",
        "OverallQual_cat3",
        "OverallQual_cat4",
        "OverallQual_cat5",
        "OverallQual_cat6",
        "OverallQual_cat7",
        "OverallQual_cat8",
        "OverallQual_cat9",
        "OverallQual_cat10",
        "Building_typeSingle Family Home "
    ),
    header = TRUE,
    # to get rid of r package output text
    single.row = FALSE,
    # to put coefficients and standard errors on same line
    # no.space = TRUE,
    # to remove the spaces after each line of coefficients
    column.sep.width = "3pt",

    # to reduce column width
    font.size = "small" # to make font size smaller
    # ,float.env = "sidewaystable"
) |> show_F_in_two_lines()








# A4
# pagan breusch test
lmtest::bptest(multi_var_model_w_confounders_ninteraction)






# standard error manual calcualtions
seBasic_full <- sqrt(diag(vcov(multi_var_model_w_confounders_ninteraction)))
seWhite_full <- sqrt(diag(vcovHC(multi_var_model_w_confounders_ninteraction, type = "HC0")))


#' why to cluster standard errrors?
#' maybe there are underlying reasons such that for instance
#' county actually influences these issues
seClust_full <- sqrt(diag(vcovHC(multi_var_model_w_confounders_ninteraction, cluster = "Neighborhood")))


stargazer(
    multi_var_model_w_confounders_ninteraction,
    multi_var_model_w_confounders_ninteraction,
    multi_var_model_w_confounders_ninteraction,
    se = list(seBasic_full, seWhite_full, seClust_full),
    type = "latex",
    omit = c(
        "YrSold2008",
        "YrSold2009",
        "YrSold2010",
        "OverallQual_cat3",
        "OverallQual_cat4",
        "OverallQual_cat5",
        "OverallQual_cat6",
        "OverallQual_cat7",
        "OverallQual_cat8",
        "OverallQual_cat9",
        "OverallQual_cat10",
        "Building_typeSingle Family Home "
    )
)









# A2
vif_ <- vif(multi_var_model_w_confounders_ninteraction)
stargazer(vif_, type = "text")

vif(multi_var_model_w_confounders_ninteraction)

stargazer(vif(multi_var_model_w_confounders_ninteraction))

stargazer(DAAG::vif(multi_var_model_w_confounders), type = "latex", omit = c(
    "YrSold2008",
    "YrSold2009",
    "YrSold2010",
    "OverallQual_cat3",
    "OverallQual_cat4",
    "OverallQual_cat5",
    "OverallQual_cat6",
    "OverallQual_cat7",
    "OverallQual_cat8",
    "OverallQual_cat9",
    "OverallQual_cat10",
    "Building_typeSingle Family Home "
))




# A3
ggplot(
    mapping = aes(
        multi_var_model_w_confounders_ninteraction$fitted.values,
        multi_var_model_w_confounders_ninteraction$residuals
    )
) +
    geom_point() +
    geom_smooth() +
    geom_smooth(
        method = "lm", color = "red", se =
            FALSE
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Fitted values", y = "Residuals")

ggsave("meanindepedence.jpg")








# A 6
df_n <-
    df_ %>% mutate(
        fitted = multi_var_model_w_confounders_ninteraction$fitted.values,
        resids = multi_var_model_w_confounders_ninteraction$residuals
    )
p1 <- ggplot(df_n, aes(x = resids)) +
    geom_histogram(color = "black", fill = "white") +
    theme_classic() +
    labs(x = "Residuals", y = "Frequency")
p2 <-
    qplot(sample = multi_var_model_w_confounders_ninteraction$residuals) + theme_classic()
grid.arrange(p1, p2, nrow = 1)

ggsave("nonnormailtyplot2s.jpg")


shapiro.test(multi_var_model_w_confounders_ninteraction$residuals)




df_n <-
    df_ %>% mutate(
        fitted = multi_var_model_w_confounders_ninteraction_unscaled$fitted.values,
        resids = multi_var_model_w_confounders_ninteraction_unscaled$residuals
    )
p1 <- ggplot(df_n, aes(x = resids)) +
    geom_histogram(color = "black", fill = "white") +
    theme_classic() +
    labs(x = "Residuals", y = "Frequency")
p2 <-
    qplot(sample = multi_var_model_w_confounders_ninteraction_unscaled$residuals) + theme_classic()
grid.arrange(p1, p2, nrow = 1)

# ggsave("nonnormailtyplot2s.jpg")


shapiro.test(multi_var_model_w_confounders_ninteraction_unscaled$residuals)






# Subsection analyiss
full_model_sub <-
    lm(
        SalePrice ~ 1 + tot_liv_area + y_since_rem + low_density_zone + commercial_zone + floating_zone + y_since_rem *
            OverallQual + tot_liv_area * low_density_zone + tot_liv_area * commercial_zone + tot_liv_area * floating_zone,
        data = df_
    )

full_model_sub.beta <- lm.beta(full_model_sub)
stargazer

stargazer(
    full_model_sub,
    full_model_sub.beta,
    coef = list(
        full_model_sub$coefficients,
        full_model_sub.beta$standardized.coefficients
    ),
    p = list(
        coef(summary(full_model_sub))[, 4],
        coef(summary(
            full_model_sub.beta
        ))[, 5]
    ),
    type = "latex"
)
