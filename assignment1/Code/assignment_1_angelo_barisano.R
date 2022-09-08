setwd("/home/angelo/Documents/Uni/Courses/Advanced Statistics and programming/Assignments/assignment1")



# Import the data
df <- read.csv("Data/train.csv")



View(df)

# Generally helpful variables
Number ofbedrooms and bathrooms
overall size
years since remodeling
years since build 
how many spots in the garage 

# ALSO! If a house was sold multiple times, this would imply a cluster by house; or possibly by area as well; so this is interesint wrt the standard error of the eestimated parameters !!!

# relevant variables:
SalePrice

# class of the building 
MSSubClass,
BldgType ---> eg one family home etc,

# area in city/ district
MSZoning,
Neighborhood: Physical locations within Ames city limits

# size of the property 
LotArea --> size of the overall lot/ property,


# overall quality contains the overall quality of the house and the condition together 
# IMPORTANT! Is the degree of finish of the house here/ i mean: is the house really finished and is this reflected in this variable?
OverallQual -- overall quality,
OverallCond --> overall condition of the house,

# age and need or remodeling of he house inpsulateded here 
YearBuilt,
YearRemodAdd,

# heating overall
HeatingQC
CentralAir
Electrical

# size of the house
1stFlrSF: First Floor square feet
2ndFlrSF: Second floor square feet

# nuber of bathrooms
BsmtFullBath: Basement full bathrooms
BsmtHalfBath: Basement half bathrooms
FullBath: Full bathrooms above grade
HalfBath: Half baths above grade

Bedroom: Number of bedrooms above basement level
Kitchen: Number of kitchens

Functional: Home functionality rating


GarageCars: Size of garage in car capacity
GarageArea: Size of garage in square feet

# sales features
MoSold: Month Sold
YrSold: Year Sold
SaleType: Type of sale
SaleCondition: Condition of sale





















# this is a saveguard as integer operation wont work on this column if i make a mistake
df$Id <- as.character(df$Id)

# MSSubClass is a CLASS of buildings
df$MSSubClass <- as.factor(df$MSSubClass)
df$MSZoning <- as.factor(df$MSZoning)
df$Street <- as.factor(df$Street)
df$Alley <- as.factor(df$Alley)
df$LotShape <- as.factor(df$LotShape)
df$LandContour <- as.factor(df$LandContour)
df$Utilities <- as.factor(df$Utilities)
df$LotConfig <- as.factor(df$LotConfig)
df$LandSlope <- as.factor(df$LandSlope)
df$Neighborhood <- as.factor(df$Neighborhood)
df$Condition1 <- as.factor(df$Condition1)
df$Condition2 <- as.factor(df$Condition2)
df$BldgType <- as.factor(df$BldgType)
df$HouseStyle <- as.factor(df$HouseStyle)

# question: is overall quality (OverallCond and OverallQual)interval of ordinal?
# df$OverallQual <- as.factor(df$OverallQual)
# df$HouseStyle <- as.factor(df$OverallCond)

# years built is an ordinal/ or nominal variable; so I will put into a factor
#NOTE: YEAR and timesince can function as a variable here! conidering the YearRemodAdd (remodel date)
df$YearBuilt <- as.factor(df$YearBuilt)
df$YearRemodAdd <- as.factor(df$YearRemodAdd)

df$RoofStyle <- as.factor(df$RoofStyle)
df$RoofMatl <- as.factor(df$RoofMatl)
df$Exterior1st <- as.factor(df$Exterior1st)
df$Exterior2nd <- as.factor(df$Exterior2nd)
df$MasVnrType <- as.factor(df$MasVnrType)
df$ExterQual <- as.factor(df$ExterQual)
df$ExterCond <- as.factor(df$ExterCond)
df$Foundation <- as.factor(df$Foundation)
df$ExterCond <- as.factor(df$ExterCond)
df$Foundation <- as.factor(df$Foundation)
df$BsmtQual <- as.factor(df$BsmtQual)
df$BsmtCond <- as.factor(df$BsmtCond)
df$BsmtExposure <- as.factor(df$BsmtExposure)
df$BsmtFinType1 <- as.factor(df$BsmtFinType1)
df$BsmtFinType2 <- as.factor(df$BsmtFinType2)

df$Heating <- as.factor(df$Heating)
df$HeatingQC <- as.factor(df$HeatingQC)
df$CentralAir <- as.factor(df$CentralAir)
df$KitchenQual <- as.factor(df$KitchenQual)
df$Functional <- as.factor(df$Functional)
df$FireplaceQu <- as.factor(df$FireplaceQu)
df$GarageType <- as.factor(df$GarageType)
df$GarageYrBlt <- as.factor(df$GarageYrBlt)
df$GarageFinish <- as.factor(df$GarageFinish)

# GARAGE TYPE MAY BE A GOOD VARIABLE AS THE SIZE MEANS MORE CARS AND MORE CARS MEANS RICHER PEOPLE!!!
df$GarageCars <- as.factor(df$GarageCars)

df$GarageQual <- as.factor(df$GarageQual)
df$GarageCond <- as.factor(df$GarageCond)
df$PavedDrive <- as.factor(df$PavedDrive)
df$PoolQC <- as.factor(df$PoolQC)


df$Fence <- as.factor(df$Fence)
df$MiscFeature <- as.character(df$MiscFeature)



# IMPORTANT FEATURES
# put month as factor; convert it properly to character type
df$MoSold <- as.factor(df$MoSold)
df$YrSold <- as.factor(df$YrSold)

# this one is interesting
df$SaleType <- as.factor(df$SaleType)
# this one just to make it clear
df$SalePrice <- as.integer(df$SalePrice)


