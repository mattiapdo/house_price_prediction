---
title: "R Notebook"
output: html_notebook
---

```{r}
# #install.packages('devtools') # Only needed if you dont have this installed.
library(devtools)
# #install_github('adam-m-mcelhinney/helpRFunctions')
library(helpRFunctions)
library(tidyverse)
```

```{r}
path.train = 'C:/Users/01/Desktop/Data_Science/FDS - Take 2/train.csv'
path.train
path.test = 'C:/Users/01/Desktop/Data_Science/FDS - Take 2/test.csv'
path.test
# stringAsFactors = F in order to avoid error 'invalid factor level, NA generated' when substituting NAs with a string

train = read.csv(file = path.train, stringsAsFactors = F)
test = read.csv(file = path.test, stringsAsFactors = F)

```

Let's divide the columns: some data are numerical, and some other data are characters

```{r}
na.summary= function(train, test){
  # store the predictors in train and test in a unique dataframe for preprocessing
  all.data = rbind(select(train, -SalePrice), test)
  #all.data = train
  
  var.types <- list.df.var.types(all.data)
  
  integer.names = var.types$integer
  integers = all.data[,integer.names]
  
  factor.names = var.types$character
  factors = all.data[,factor.names]
  
  nas = list(rep(NA, 2))
  
  a  = cbind("NA's" = sapply(factors, function(x) sum(is.na(x))))
  nas[[1]] = a[order(a[,1], decreasing = T),]
  
  a  = cbind("NA's" = sapply(integers, function(x) sum(is.na(x))))
  nas[[2]] = a[order(a[,1], decreasing = T),]
  
  print(nas)
}

na.summary(train, test)

```

## Missing Values

### Factors

Here we have a lot of NA's

1 ) PoolQC : reading from the data description, we see that if there are NA's there is no pool... we want to substitute NA with No 

```{r}
train['PoolQC'][is.na(train['PoolQC'])] = 'No'
test['PoolQC'][is.na(test['PoolQC'])] = 'No'
```

2) MiscFeature : similar as before.. NA means no miscellaneus feaure

```{r}
train['MiscFeature'][is.na(train['MiscFeature'])] = 'None'
test['MiscFeature'][is.na(test['MiscFeature'])] = 'None'
```

3) Alley: similar as before, NA means no alley access

```{r}
train['Alley'][is.na(train['Alley'])] = 'No'
test['Alley'][is.na(test['Alley'])] = 'No'
```

4) Fence: similar as before.. NA means no fence

```{r}
train['Fence'][is.na(train['Fence'])] = 'No'
test['Fence'][is.na(test['Fence'])] = 'No'
```

5) FireplaceQu: similar as before.. NA means no fireplace

```{r}
train['FireplaceQu'][is.na(train['FireplaceQu'])] = 'No'
test['FireplaceQu'][is.na(test['FireplaceQu'])] = 'No'
```

6) GarageType, GarageFinish, GarageQual, GarageCond: NA means no garage (we notice that there are 81 NA's... the same holds for the numerical variable GarageYrBlt)

```{r}
train[c('GarageType', 'GarageFinish', 'GarageQual', 'GarageCond') ][is.na(train[c('GarageType', 'GarageFinish', 'GarageQual', 'GarageCond')])] = 'No'
test[c('GarageType', 'GarageFinish', 'GarageQual', 'GarageCond') ][is.na(test[c('GarageType', 'GarageFinish', 'GarageQual', 'GarageCond')])] = 'No'
```

7) 'BsmtExposure', 'BsmtFinType2', 'BsmtQual', 'BsmtCond', 'BsmtFinType1': NA means no basament 

```{r}
train[c('BsmtExposure', 'BsmtFinType2', 'BsmtQual', 'BsmtCond', 'BsmtFinType1') ][is.na(train[c('BsmtExposure', 'BsmtFinType2', 'BsmtQual', 'BsmtCond', 'BsmtFinType1')])] = 'No'
test[c('BsmtExposure', 'BsmtFinType2', 'BsmtQual', 'BsmtCond', 'BsmtFinType1') ][is.na(test[c('BsmtExposure', 'BsmtFinType2', 'BsmtQual', 'BsmtCond', 'BsmtFinType1')])] = 'No'
```

8) MasVnrType: masonry veneer type.. there are just 8 missing values, let's replace it with 'None', that is the most common value

```{r}
#barplot(table(train$MasVnrType))
train['MasVnrType'][is.na(train['MasVnrType'])] = 'None'
test['MasVnrType'][is.na(test['MasVnrType'])] = 'None'
```

9) Electrical: Electrical system... there is just one missing values...
let's replace it with 'SBrkr' (Standard Circuit Breakers & Romex) that is the most common value

```{r}
#barplot(table(train$Electrical))
train['Electrical'][is.na(train['Electrical'])] = 'SBrkr'
test['Electrical'][is.na(test['Electrical'])] = 'SBrkr'
```

```{r}
#barplot(table(train$MSZoning), main = "MSZ")
train['MSZoning'][is.na(train['MSZoning'])] = 'RL'
test['MSZoning'][is.na(test['MSZoning'])] = 'RL'

#barplot(table(train$Utilities), main = "Uti")
train['Utilities'][is.na(train['Utilities'])] = 'AllPub'
test['Utilities'][is.na(test['Utilities'])] = 'AllPub'

#barplot(table(train$Functional), main = "Func")
train['Functional'][is.na(train['Functional'])] = 'Typ'
test['Functional'][is.na(test['Functional'])] = 'Typ'

#barplot(table(train$Exterior1st), main = "EX1", las = 2)
train['Exterior1st'][is.na(train['Exterior1st'])] = 'VinylSd'
test['Exterior1st'][is.na(test['Exterior1st'])] = 'VinylSd'

#barplot(table(train$Exterior2nd), main = "EX2", las = 2)
train['Exterior2nd'][is.na(train['Exterior2nd'])] = 'VinylSd'
test['Exterior2nd'][is.na(test['Exterior2nd'])] = 'VinylSd'

#barplot(table(train$KitchenQual), main = "KQUA")
train['KitchenQual'][is.na(train['KitchenQual'])] = 'TA'
test['KitchenQual'][is.na(test['KitchenQual'])] = 'TA'

#barplot(table(train$SaleType), main = "SALT")
train['SaleType'][is.na(train['SaleType'])] = 'WD'
test['SaleType'][is.na(test['SaleType'])] = 'WD'
```


```{r}
na.summary(train, test)
```


### Numerical Data

As we can see, there are only three numerical variables that contain missing values:

1) LotFrontage : linear feet of street connected to property - 259 NA's... we replace the NA's with the mean value for LotFrontage (computed removing the NA's)

Note that here we replace NAs in the test set with the mean value in the training set, in order to avod data leakage. 

https://machinelearningmastery.com/data-leakage-machine-learning/

```{r}
#hist(train$LotFrontage)
train['LotFrontage'][is.na(train['LotFrontage'])] = mean(train$LotFrontage, na.rm = T)
test['LotFrontage'][is.na(test['LotFrontage'])] = mean(train$LotFrontage, na.rm = T)
```


2) GarageYrBlt: Year garage was built - 81 NA's - this means no garage

3) MasVnrArea - 8 NA's - this is exactly the same amount of NA's of the categorical variable 'MasVnrType'... in thoose cases we assumed that NA's mean 'no masonry veneer type... so we replace theese NA's with 0

```{r}
#hist(train$MasVnrArea)
train['MasVnrArea'][is.na(train['MasVnrArea'])] = 0
test['MasVnrArea'][is.na(test['MasVnrArea'])] = 0
```

4) Other features that is reasonable to think they're missing because their actual value was zero

```{r}
train[c("BsmtFullBath","BsmtHalfBath","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","GarageCars","GarageArea") ][is.na(train[c("BsmtFullBath","BsmtHalfBath","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","GarageCars","GarageArea")])] = 0
test[c("BsmtFullBath","BsmtHalfBath","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","GarageCars","GarageArea") ][is.na(test[c("BsmtFullBath","BsmtHalfBath","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","GarageCars","GarageArea")])] = 0
```


```{r}
na.summary(train, test)
```

*GarageYrBlt still has NA's..decide whether whether dropping the GarageYrBlt column*

## Outliers

```{r}
var.types <- list.df.var.types(train)
var.types
integer.names = var.types$integer
integers = train[,integer.names]

plot(train$LotFrontage, train$SalePrice, xlab = "LotFrontage", ylab = 'Sale Price')
plot(train$MasVnrArea, train$SalePrice, xlab = "MasVnrArea", ylab = 'Sale Price')

# to be commented
for (i in 1:(ncol(integers))){
  plot(integers[,i], integers$SalePrice, xlab = paste(colnames(integers)[i]), ylab = 'Sale Price')
}
```

```{r}
train  = train[!train$LotFrontage > 300,]
train  = train[!train$LotArea > 200000,]     # try also 1000/1500/2000
train  = train[!train$BsmtFinSF1 > 5000,]
train  = train[!train$TotalBsmtSF > 6000,]
train  = train[!train$GrLivArea > 4500,]
```


## Some numerical variable are actually categorical

1) MSSubClass: identifies the type of dwelling involved in the sale

```{r}
train$MSSubClass <- as.factor(train$MSSubClass)
```

2) YrSold, MoSold must be treated like factors, because 

```{r}
range(train$YrSold)
```

and because 'December is not better tran January'.

Before casting YrSold as factor, let's create the 'Age' and 'AgeRemod' variables in the Feature Engineering section

## Feature Engineering

0) new variable 'TotArea' (total area)

```{r}
train$TotArea <- train$GrLivArea + train$TotalBsmtSF
test$TotArea <- test$GrLivArea + test$TotalBsmtSF

train$GrLivArea = test$GrLivArea = NULL # to avoid collinearity between TotArea and GrLivArea
train$TotalBsmtSF = test$TotalBsmtSF = NULL # to avoid collinearity between TotArea and TotalBsmtSF
```

1) new variable: 'Age' of the building . We expect a negative correlation with SalePrice

```{r}
train$Age = train$YrSold - train$YearBuilt
test$Age = test$YrSold - test$YearBuilt

train$YearBuilt = test$YearBuilt = NULL # to avoid collinearity between Age and YearBuilt
```

2) new variable: 'IsRemod' - factor variable... it indicates if the house has been remodelled in the last 5 years

```{r}
train$JustRemod = as.factor(ifelse(train$YrSold - train$YearRemodAdd <= 5, 1, 0))
test$JustRemod = as.factor(ifelse(test$YrSold - test$YearRemodAdd <= 5, 1, 0))

train$YearRemodAdd = test$YearRemdAdd = NULL # to avoid collinearity between JustRemod and YearRemod
```

cast YrSold and MoSold as factors, as announced

```{r}
# drop YrSold, YearBuilt, YearRemovedAdd
train$YrSold = train$YearBuilt = train$YearRemodAdd = NULL
test$YrSold = test$YearBuilt = test$YearRemodAdd = NULL

# Month Sold as Factor
train$MoSold <- as.factor(train$MoSold)
test$MoSold <- as.factor(test$MoSold)
```

3) new variable: 'TotBath' = 'FullBath' + 3/5 * 'HalfBath'

```{r}
train$TotBath = train$FullBath + 3/5 * train$HalfBath
test$TotBath = test$FullBath + 3/5 * test$HalfBath

train$FullBath = train$HalfBath = NULL
test$FullBath = test$HalfBath = NULL
```


### Multivariate Analysis
