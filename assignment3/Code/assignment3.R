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

########################################
# PART 1

# Download selected data from the portal and store
# the data in dataframe dfTime2Export
dfTime2Export <-
  wb_data( indicator = c("IC.EXP.TMBC",
                         "NY.GDP.PCAP.KD", # higher GDP per capita means higher development of economy 
                         "NE.EXP.GNFS.ZS", # this is "Exports of goods and services (% of GDP)"
                         "NV.IND.TOTL.ZS"# "Industry (including construction)  value added (% of GDP)"
  ) ,
           country = "countries_only",
           start_date = 2014 ,
           end_date = 2021)

View(dfTime2Export)


save(dfTime2Export, file=paste0(dirData, "assignemt_part1.sav"))


# dfTime2Export <- load(file=paste0(dirData, "assignemt_part1.sav"))


# Q 1
colnames(dfTime2Export)[colnames(dfTime2Export) == "NY.GDP.PCAP.KD"]    <- "GDPcap"
colnames(dfTime2Export)[colnames(dfTime2Export) == "country"]           <- "Country"
colnames(dfTime2Export)[colnames(dfTime2Export) == "date"]              <- "Year"
colnames(dfTime2Export)[colnames(dfTime2Export) == "IC.EXP.TMBC"]    <- "TimeToExport"
colnames(dfTime2Export)[colnames(dfTime2Export) == "NE.EXP.GNFS.ZS"]    <- "Exports_pct_GDP"
colnames(dfTime2Export)[colnames(dfTime2Export) == "NV.IND.TOTL.ZS"]    <- "Industry_pct_GDP"

df <- dfTime2Export



#' timetoexport influcnece factors:
#' pctUrbPop --> Becasue more urbanzed population might suggest more developed country with more production to export


###################
# q2 Clean data into balanced
# remove missing values

df$iso2c <- NULL
df$iso3c <- NULL


# remove those outside the timerange of 2014 and 2019
# only keep complete cases 
df.sub <- df[complete.cases(df),]
# remove all outside the timerange
df.sub <- df.sub[(df.sub$Year >= 2014) & (df.sub$Year <= 2019),]

sum(is.na(df.sub))
is.na(df.sub)

View(df.sub)

df.sub %>% 
  summarise_all(~sum(is.na(.)))

df.avg <- 
  ddply(df.sub, .(Country), summarise,
        avg.TimeToExport   = mean(TimeToExport, na.rm=TRUE),
        avg.GDPcap   = mean(GDPcap, na.rm=TRUE),
        avg.Exports_pct_GDP = mean(Exports_pct_GDP, na.rm=TRUE),
        avg.Industry_pct_GDP = mean(Industry_pct_GDP, na.rm=TRUE),
        # for later checking that the right amount of instances are incldued
        numValid         = length(Country))





# merge 
df.sub <- merge(df.sub, df.avg, by="Country")



# here do the wihin substrationg - within mean transformation
attach(df.sub)
df.sub$diff.TimeToExport   <- TimeToExport   - avg.TimeToExport
df.sub$diff.Exports_pct_GDP     <- Exports_pct_GDP     - avg.Exports_pct_GDP
df.sub$diff.Industry_pct_GDP <- Industry_pct_GDP - avg.Industry_pct_GDP
df.sub$diff.GDPcap <- GDPcap - avg.GDPcap
detach(df.sub)


# select only those with 6 years
df.sub <- df.sub[df.sub$numValid == 6,]
df.avg <- df.avg[df.avg$numValid == 6,]



### Question 2: obtain summary statistics
df_temp <- df.sub %>%
  select((c("Year", "TimeToExport", "GDPcap", "Exports_pct_GDP", "Industry_pct_GDP")))

stargazer(
  df_temp,
  type = "text",
  omit.summary.stat = c("N"),
  summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
  title = "Descriptive Statistics Time to Export"
)

stargazer(
  df_temp,
  type = "latex",
  omit.summary.stat = c("N"),
  summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"),
  title = "Descriptive Statistics Time to Export"
)

skewness(df_temp$TimeToExport)
skewness(df_temp$GDPcap)
skewness(df_temp$Exports_pct_GDP)

#' arguments:
#' Exports_pct_GDP --> makes sense
#' Industry_pct_GDP --> more industry means more to export
#' GDPcap --> is an indicator of the development of the country ; how strong is the economy



###################
# check whether all worked fine!

#' firstly : all numvalid have length 6 (160 countries) --> should make 160 * 6 = 960
table(df.avg$numValid)

# check number of rows (both compliant!)
nrow(df.avg)
nrow(df.sub)

which.max(table(df.avg$numValid))
which.max(table(df.sub$numValid))


## now only select those with numvalid 6 just to make sure
df.sub <- df.sub[df.sub$numValid == 6,]
df.avg <- df.avg[df.avg$numValid == 6,]



##################################
# run the models 


# model formulation
mdlA <- TimeToExport ~ Exports_pct_GDP + Industry_pct_GDP + GDPcap 

# ind the variables of interest
mdlVars <- all.vars(mdlA)


# here we get the average vars and fdifferecne vars
mdlVars.avg  <- paste0("avg.", mdlVars)
mdlVars.diff <- paste0("diff.", mdlVars)

# select variables from the data frames
df.between <- df.avg[mdlVars.avg]
df.within  <- df.sub[mdlVars.diff]
# ... rename column names in order to make use of the 
# ... same model specification mdlA, and to conveniently
# ... merge the regresssion objects in stargazer
colnames(df.within) <- 
  gsub("diff\\.", "", colnames(df.within))
colnames(df.between) <- 
  gsub("avg\\.", "", colnames(df.between))



#------------------
# Estimation of the pooled model
#------------------
rsltPool <- lm (mdlA, data=df.sub)
summary(rsltPool)

stargazer(rsltPool,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE)

# Equivalently, using plm with option 'model="pooling"'
# rslt.Pooling <- plm(mdlA, data = df.sub, model = "pooling")
# summary(rslt.Pooling)




#------------------
# Estimation of the within and between grop models
#------------------
rsltWithin <- lm (mdlA, data=df.within)
summary(rsltWithin)

rsltBetween <- lm (mdlA, data=df.between)
summary(rsltBetween)

################ pooled
rsltPool.Country <- 
  plm(mdlA, data = df.sub, 
      index=c("Country", "Year"), model = "pooling")


############## between
rsltBetween.Country <- 
  plm(mdlA, data = df.sub, 
      index=c("Country", "Year"), model = "between")

###################' For FIXED EFFECT == WITHIN!
rsltFE.Country <- 
  plm(mdlA, data = df.sub, 
      index=c("Country", "Year"), model = "within")


######### RANDOM EFFECTS MODEL!
rsltRE.Country <- 
  plm(mdlA, data = df.sub, 
      index=c("Country", "Year"), model = "random")




# Tabulate the results
stargazer(rsltPool.Country, rsltBetween.Country, rsltFE.Country, rsltWithin, rsltRE.Country,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, type="text",report=("vc*p"))


stargazer(rsltPool.Country, rsltBetween.Country, rsltFE.Country, rsltRE.Country,
          # align=TRUE, 
          no.space=TRUE, intercept.bottom = FALSE, type="latex")









##############
#'IMPORTANT: the results have intercepts; but every entity (eg coutnry)has a different intercept
# Explore the estimated intercepts
summary(fixef(rsltFE.Country, type="dmean"))

#' TO see whether fixed effects 8within model) is justified: see whether the intercepts are different from each other
#' if the intercepts on an entity level are considerably different this impleis that FIXED EFFECTS MODEL IS RELEVANT!!
#' IMPORTNAT: SOME THE INTERCEPTS SHOULD ALSO BE SIGNIFICATN--> if all are insignifiacnt then the pooled
#' and the within model are going to be not too different!






###################################
# Question 3
#' finally do an F test!!! (or pooled test) between fixed effects model and the 
#' pooled model
#' So in order to know whether to use pooled or Fixed model use the partial F test!!!!


# Calculate for the fixed country effect model
R2.LSDV <- 1 - (var(residuals(rsltFE.Country))/
                  var(rsltFE.Country$model$GHGcap))
R2.LSDV


#' result: an inisgnificant test says that both models are consistent! 
#' a siginficat test regjects this in favour of fixed effect model
#' for fixed model we want significance!!




# Evaluate the fixed effects model versus the pooled 
# regression model 8same thing as partial f test again
pFtest(rsltFE.Country, rsltPool.Country)
pooltest(rsltFE.Country, rsltRE.Country)












