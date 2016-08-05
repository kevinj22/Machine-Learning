# Importing all Ratios
# Hit CTRL + ALT + R to run
# All packages must be installed from Install Package
# If you wish to clear everything and start from scratch type rm(list=ls()) in the console below

# Load packages

library(quantmod)
library(lubridate)
library(rpart)
library(randomForest)
library(class)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Load functions

source('importRatios.R')
source('PriceRatio.R')

# Importing Ratios

BNSratios<-importRatios('BNSratios.csv','BNS')
TDratios<-importRatios('TDratios.csv','TD')
CMratios<-importRatios('CMratios.csv','CM')
RYratios<-importRatios('RYratios.csv','RY')
BMOratios<-importRatios('BMOratios.csv','BMO')

# Getting Time Series Price Data

getSymbols('BNS')
getSymbols('TD')
getSymbols('CM')
getSymbols('RY')
getSymbols('BMO')

# Merging Ratios and Price data

BNSmerge<-Price_Ratio_Merge(BNS,BNSratios)
TDmerge<-Price_Ratio_Merge(TD,TDratios)
CMmerge<-Price_Ratio_Merge(CM,CMratios)
RYmerge<-Price_Ratio_Merge(RY,RYratios)
BMOmerge<-Price_Ratio_Merge(BMO,BMOratios)

View(BNSratios)
View(BNSmerge)

# Bind into one large set

PRDF<-rbind(BNSmerge,TDmerge,CMmerge,RYmerge,BMOmerge)
View(PRDF)

# Split data into training set and test set

# Set random seed. Don't remove this line.

set.seed(1)

# Shuffle the dataset, call the result shuffled

n<-nrow(PRDF)
shuffled <- PRDF[sample(n),]

# Split the data in train and test

train_indices <- 1:round(0.7*n)
train <- shuffled[train_indices,]
test_indices <- (round(0.7*n)+1):n
test <- shuffled[test_indices,]


# Print the structure of train and test
str(train)
str(test)

# Make a prediction
# Set random seed.
set.seed(1)

# Learn the model

tree<-rpart(Close~Volume+Earnings_Per_Share_CAD+Revenue_CAD_Mil+RoE+Dividends_CAD,train,method="anova")
fancyRpartPlot(tree)
predPRDFtree<- predict(tree,test)

# Make a random forest Variable Importance Plot

forest<-randomForest(Close~Volume+Earnings_Per_Share_CAD+Revenue_CAD_Mil+RoE+Dividends_CAD,train,ntree=500)
varImpPlot(forest)

# Prediction ARIMA

lm_model <- lm(Close ~ Volume+Revenue_CAD_Mil+Net_Income_CAD_Mil+Earnings_Per_Share_CAD+Dividends_CAD+Debt_to_Equity+Net_Marg+RoA+RoE,data=PRDF)

summary(lm_model)

# Need to make a data frame to make a prediction on
# Use means and median of past BNS to predict on

PRDFtest<-2016:2017
PRDFtest<-as.data.frame(PRDFtest)
names(PRDFtest)<-"Year"

for (i in 2:dim(BNSmerge)[2]){
  
  PRDFtest[1,i-1]<-mean(BNSmerge[,i])
  PRDFtest[2,i-1]<-median(BNSmerge[,i])
  names(PRDFtest)[i-1]<-names(BNSmerge)[i]
}

row.names(PRDFtest)<-c('Mean','Median')
predPRDF<-predict(lm_model,PRDFtest,interval="confidence")
predPRDF
