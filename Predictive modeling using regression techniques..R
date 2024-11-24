install.packages("fpp3")
library("fpp3")


library(tidyverse)
library(dplyr)
library(remotes)
library(naniar)
library(outliers)
library(ggplot2)
library(mlbench)
library(robustbase)
library(car)
library(clusterSim)
library(EnvStats)
library(MASS)
library(Amelia)
library(VIM)
library(ggbiplot)
library(mice)
library(randomForest)
library(readr)
library(caret)
library(corrplot)
library(dplyr)
library(corrr)
library(reshape2)
library(lattice)



##############################################  1(a-i)  ######################################################################


##....................................................................................................................
##Pre-processing of the dataset- Missing value imputation

getwd()

house <- read.csv('housingData.csv')
house
str(house)
summary(house)


ncol(house) # number of total columns 


length(select_if(house,is.numeric)) # number of numeric columns 


numeric_column = select_if(house, is.numeric)  # shows the numeric columns 
View(numeric_column)



# Outlier processing

boxplot(numeric_column[,c('LotFrontage','LotArea','MasVnrArea','WoodDeckSF','GrLivArea','BsmtUnfSF')]) 

boxplot(numeric_column[,c('YearBuilt','TotalBsmtSF','GarageYrBlt','OverallCond','OverallQual')]) 

boxplot(numeric_column[,c('YearRemodAdd','BsmtFinSF1','BsmtFinSF2','X2ndFlrSF','GarageArea','OpenPorchSF','EncPorchSF')]) 


# replacing the outlier by 0

for (x in c('BsmtFinSF1','LotArea','OpenPorchSF','GarageArea'))
{
  value = numeric_column[,x][numeric_column[,x] %in% boxplot.stats(numeric_column[,x])$out]
  numeric_column[,x][numeric_column[,x] %in% value] = NA
} 

# shows the numeric column have missing values 

names(which(colSums(is.na(numeric_column))>0)) 

# mean value imputation on missing values & get a dataframe of numeric columns

df<-data.frame(numeric_column)

df$LotFrontage[is.na(df$LotFrontage)] <- mean(df$LotFrontage,na.rm = TRUE)
df$MasVnrArea[is.na(df$MasVnrArea)] <- mean(df$MasVnrArea,na.rm = TRUE)
df$GarageYrBlt[is.na(df$GarageYrBlt)] <- mean(df$GarageYrBlt,na.rm = TRUE)
df$BsmtFinSF1[is.na(df$BsmtFinSF1)] <- mean(df$BsmtFinSF1,na.rm = TRUE)
df$OpenPorchSF[is.na(df$OpenPorchSF)] <- mean(df$OpenPorchSF,na.rm = TRUE)
df$LotArea[is.na(df$LotArea)] <- mean(df$LotArea,na.rm = TRUE)
df$GarageArea[is.na(df$GarageArea)] <- mean(df$GarageArea,na.rm = TRUE)

names(which(colSums(is.na(df))>0))
is.na(df)

df  # mean value imputet numeric data frame 

sapply(df, function(x) sum(is.na(x)))  ## check if any missing value still remains 

# converting significant categorical variable column int numeric

cat1 = house$Neighborhood
cat2 = house$GarageType 
cat3 = house$HouseStyle
cat4 = house$HeatingQC
cat5 = house$KitchenQual


Neighbourhood_cat = as.numeric(factor(as.matrix(cat1)))
GarageType_cat = as.numeric(factor(as.matrix(cat2)))
HouseStyle_cat = as.numeric(factor(as.matrix(cat3)))
HeatingQC_cat = as.numeric(factor(as.matrix(cat4)))
KitchenQual_cat = as.numeric(factor(as.matrix(cat5)))

# taking log of the saleprice 

log_Sale_price = log(house$SalePrice)
log_Sale_price 


GarageType_cat
GarageType_cat[is.na(GarageType_cat)] <- mean(GarageType_cat,na.rm = TRUE) ## mean value imputation
GarageType_cat


# combining the final data-set 

install.packages("dplyr")
library(dplyr)

combined = data.frame(df, Neighbourhood_cat,GarageType_cat, HouseStyle_cat, HeatingQC_cat,KitchenQual_cat, log_Sale_price)
combined

final_data = combined %>%
  dplyr:: select(-MiscVal, -Id, -LowQualFinSF, -BsmtFullBath, -BsmtHalfBath, - KitchenAbvGr,- PoolArea, -SalePrice, -TotalBsmtSF, - EncPorchSF,- BsmtFinSF2 )


final_data

sapply(final_data, function(x) sum(is.na(x))) # check if missing value exist 

ncol(final_data)
nrow(final_data)






## Train data set of 1000 observation
trainrows = sample(rownames(final_data), dim(final_data)[1]*1) 
traindata = final_data[trainrows, ]
nrow(traindata)



#..............................................................................................................
# Build multiple linear OLS regression model using lm 



fit = lm(formula =  log_Sale_price ~   MSSubClass + LotFrontage + LotArea  + OverallQual  +  OverallCond+ YearBuilt+ YearRemodAdd+ MasVnrArea+
           BsmtFinSF1 + BsmtUnfSF +  X1stFlrSF +X2ndFlrSF+ GrLivArea +FullBath + HalfBath+ BedroomAbvGr+TotRmsAbvGrd+Fireplaces+GarageYrBlt+GarageCars+
           GarageArea+  WoodDeckSF + OpenPorchSF + MoSold+ YrSold + Neighbourhood_cat + GarageType_cat + HouseStyle_cat+ HeatingQC_cat, data= final_data, subset = trainrows)



summary(fit)

AIC(fit)
BIC(fit)


# removing all the variables which P value is less than 0.05;

#...............................................
fit1 = lm(formula =  log_Sale_price ~   MSSubClass + LotFrontage + LotArea  + OverallQual  +  OverallCond+ YearBuilt+  
            BsmtFinSF1 + BsmtUnfSF + GrLivArea + BedroomAbvGr+Fireplaces+GarageCars+
            WoodDeckSF   + Neighbourhood_cat  + HeatingQC_cat, data= final_data, subset = trainrows)


summary(fit1)

AIC(fit1)
BIC(fit1)

#.....................................

fit2 = lm(formula =  log_Sale_price ~  MSSubClass + LotFrontage , data= final_data, subset = trainrows)

summary(fit2)

AIC(fit2)
BIC(fit2)

#............................................

fit3 = lm(formula =  log_Sale_price ~  MSSubClass + LotFrontage + LotArea  + OverallQual , data= final_data, subset = trainrows)

summary(fit3)

AIC(fit3)
BIC(fit3)
#.................................................


fit4 = lm(formula =  log_Sale_price ~  MSSubClass + LotFrontage + LotArea  + OverallQual+OverallCond+ YearBuilt , data= final_data, subset = trainrows)

summary(fit4)

AIC(fit4)
BIC(fit4)
#.............................................................

fit5 = lm(formula =  log_Sale_price ~  MSSubClass + LotFrontage + LotArea  + OverallQual+OverallCond+ YearBuilt + BsmtFinSF1 + BsmtUnfSF , data= final_data, subset = trainrows)

summary(fit5)

AIC(fit5)
BIC(fit5)

#..................................................................

fit6 = lm(formula =  log_Sale_price ~  MSSubClass + LotFrontage + LotArea  + 
            OverallQual+OverallCond+ YearBuilt + BsmtFinSF1 + BsmtUnfSF + GrLivArea + BedroomAbvGr+Fireplaces+GarageCars, data= final_data, subset = trainrows)

summary(fit6)

AIC(fit6)
BIC(fit6)

#.....................................................................


fit7 = lm(formula =  log_Sale_price ~  MSSubClass + LotFrontage + LotArea  + 
            OverallQual+OverallCond+ YearBuilt + BsmtFinSF1 + BsmtUnfSF + GrLivArea +
            BedroomAbvGr+Fireplaces+GarageCars + WoodDeckSF + Neighbourhood_cat , data= final_data, subset = trainrows)

summary(fit7)

AIC(fit7)
BIC(fit7)

#...................................................
# Interaction of the variables with higher value of t


fit8 = lm(formula =  log_Sale_price ~   OverallQual  *  OverallCond * YearBuilt *  
            BsmtFinSF1 * GrLivArea,  data= final_data, subset = trainrows)

summary(fit8)

AIC(fit8)
BIC(fit8)

#........................................................
## values of fit-1
# Install car package if not installed
install.packages("car")
library(car)
a = vif(fit1)
View(a)
mean(vif(fit1))
b = fit1$coefficients
View(b)
fit1$residuals


RMSE = sqrt(sum(fit1$residuals^2)/1000)
RMSE


summary(fit1)$r.square  # R-square 


## values of fit-8 (Interaction)
# Install car package if not installed
install.packages("car")
library(car)
a = vif(fit8)
View(a)
mean(vif(fit8))
b = fit1$coefficients
View(b)
fit8$residuals


RMSE = sqrt(sum(fit8$residuals^2)/1000)
RMSE


summary(fit8)$r.square  # R-square 


##############################################  1(a-ii)  ######################################################################


## predict data 

pred = predict(fit1,newdata=final_data)
vres= data.frame(final_data$log_Sale_price, pred, residuals=final_data$log_Sale_price - pred)
head(vres)
d = head(vres)
View(d)


#.............................................................................................................

#Stepwise Regression & Residual pattern analysis
install.packages("MASS")
library(MASS)


ols.fit.Comp2 = stepAIC(fit1,direction = "both")
ols.fit.Comp2$anova
ols.fit.Comp2
summary(ols.fit.Comp2)
AIC(ols.fit.Comp2)
BIC(ols.fit.Comp2)
vif(ols.fit.Comp2)
mean(vif(ols.fit.Comp2))
ols.fit.Comp2$coefficients
RMSE2 = sqrt(sum(ols.fit.Comp2$residuals^2)/1000)
plot(ols.fit.Comp2)


#...........................................................................................................
# hat values vs index

plot(hatvalues(fit1)) # index plot of leverages
abline(abline(h=2*15/1000))

hatvalues(fit1)[hatvalues(fit1)>0.032]

#............................................................................................................

#residual plot

plot(fit1$fitted.values,fit1$residuals, col = "black", pch = 21, bg = "red") 
abline(h=0)


#histogram of residuals

par(mfrow=c(1,1))
hist(fit1$resid) 

install.packages("car")
library(car)
ncvTest(fit1)



##############################################  1(b)  ######################################################################
## PLS model

install.packages("elasticnet")
library(lattice)
library(caret)
library(dplyr)
library(elasticnet)
library(lars)


final_data   # main file

xtrain_data = subset(final_data, select = -c(log_Sale_price ))  # new train data by excluding response variable
View(xtrain_data)


ytest_data =  final_data$log_Sale_price  # test data is response variable 
View(ytest_data)




set.seed(100) # et up random seed and 10-folder cross-validation

ctrl = trainControl(method = "cv", number = 10)
plsTune = train(xtrain_data, ytest_data, 
                method = "pls", 
                
                tuneGrid = expand.grid(.ncomp = 1:30),      # set hyper-parameter tuning range
                trControl = ctrl)


plsTune



# relative importance of each variable during PLS model tuning process


plsImp <- varImp(plsTune, scale = FALSE)
plot(plsImp, top = 17, scales = list(y = list(cex = 0.95)))



##hyper-parameter tuning process for PLS

plsResamples <- plsTune$results
plsResamples$Model <- "PLS"


# Leverage xyplot() function from lattice library


xyplot(RMSE ~ ncomp, data = plsResamples, xlab = "Components", 
       ylab = "RMSE (Cross-Validation)", auto.key = list(columns = 2), 
       groups = Model, type = c("o", "g"))


#...........................................................................................................
# using the major seven variables for PLS model 


final_data    

train_control = trainControl(method="repeatedcv", number = 10,repeats=3)


pls.model = train(log_Sale_price~ OverallQual + OverallCond + Fireplaces + BedroomAbvGr + Neighbourhood_cat +
                    GarageType_cat +  KitchenQual_cat , data= final_data, method="pls", trControl=train_control, 
                  tuneLength = 10, metric = "RMSE")


pls.model

pls.model$resample$RMSE
mean(pls.model$resample$RMSE)

pls.model$resample$Rsquared
mean(pls.model$resample$Rsquared)

prediction1 = predict(pls.model, final_data)  # prediction using the model 

prediction1   ## predicted value of log(sales price)
final_data$log_Sale_price ## Actual observed log(sales price) 

vres1= data.frame(final_data$log_Sale_price, prediction1, residuals=final_data$log_Sale_price - prediction1)
head(vres1)

################################################  1(c)   ############################################################
## LASSO Model



final_data

y = final_data$log_Sale_price   # response variable

x = as.matrix(final_data[,1:30])  ## predictor variable in matrix form 

install.packages("glmnet")
library(glmnet)

#perform k-fold cross-validation to find optimal lambda value


cv_model <- cv.glmnet(x, y, alpha = 1)


#find optimal lambda value that minimizes test MSE


best_lambda <- cv_model$lambda.min

best_lambda

#produce plot of test MSE by lambda value

plot(cv_model)

# Analyze Final Model sing the minimum lemda value 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

coef(best_model)



# calculate the R-squared of the model on the training data 


#use fitted best model to make predictions

y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE

sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

## find RMSE 

install.packages("Metrics")
library(Metrics)
rmse(final_data$log_Sale_price, y_predicted)

################################################  1(d)   ############################################################


################################################ Ridge Model ########################################################

# load the data

y = final_data$log_Sale_price   # response variable

x = as.matrix(final_data[,1:30])  ## predictor variable in matrix form 

# Fit the Ridge Regression Model

library("glmnet")
model <- glmnet(x, y, alpha = 0)

summary(model)

#Choose an Optimal Value for Lambda

cv_model <- cv.glmnet(x, y, alpha = 0)

#find optimal lambda value that minimizes test MSE

best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model) 

# Analyze Final Model with optimum lamda value

best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)

#produce Ridge trace plot

plot(model, xvar = "lambda")

##calculate the R-squared of the model on the training data

#use fitted best model to make predictions

y_predicted_Ridge <- predict(model, s = best_lambda, newx = x)

#find SST and SSE

sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted_Ridge - y)^2)

#find R-Squared

rsq <- 1 - sse/sst
rsq

## find RMSE 

install.packages("Metrics")
library(Metrics)
rmse(final_data$log_Sale_price, y_predicted_Ridge)

################################################ PCR Model ########################################################


install.packages("pls")

library(pls)

head(final_data)


set.seed(1)  #make this example reproducible



#fit PCR model

model <- pcr(log_Sale_price ~   MSSubClass + LotFrontage + LotArea  + OverallQual  +  OverallCond+ YearBuilt+ YearRemodAdd+ MasVnrArea+
               BsmtFinSF1 + BsmtUnfSF +  X1stFlrSF +X2ndFlrSF+ GrLivArea +FullBath + HalfBath+ BedroomAbvGr+TotRmsAbvGrd+Fireplaces+GarageYrBlt+GarageCars+
               GarageArea+  WoodDeckSF + OpenPorchSF + MoSold+ YrSold + Neighbourhood_cat + GarageType_cat + HouseStyle_cat+ HeatingQC_cat, data= final_data, scale=TRUE, validation="CV")

# Choose the Number of Principal Components

summary(model)

#visualize cross-validation plots

validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")


# Final Model to Make Predictions
# Select the optimal number of components based on the validation results (for example, choosing 7 components based on MSEP)
ncomp_optimal <- 7

# Make predictions on the entire dataset using the chosen number of components
pcr_pred <- predict(model, final_data, ncomp = ncomp_optimal)

# Replace any missing predictions with the mean prediction
pcr_pred[is.na(pcr_pred)] <- mean(pcr_pred, na.rm = TRUE)

# Check accuracy of PCR predictions by calculating residuals
pcr_pred_check <- data.frame(actual = final_data$log_Sale_price, predicted = pcr_pred, residuals = final_data$log_Sale_price - pcr_pred)
head(pcr_pred_check)

# Calculate RMSE for the predictions
RMSE <- sqrt(mean((pcr_pred - final_data$log_Sale_price)^2))
print(paste("RMSE: ", RMSE))

# Calculate R-Squared
sst <- sum((final_data$log_Sale_price - mean(final_data$log_Sale_price))^2)  # Total Sum of Squares
sse <- sum((pcr_pred - final_data$log_Sale_price)^2)                        # Sum of Squared Errors
rsq <- 1 - sse/sst
print(paste("R-Squared: ", rsq))



################################################ MARS Model ########################################################
install.packages("ISLR")
install.packages("earth")
library(ISLR)      #contains Wage dataset
library(dplyr)     #data wrangling
library(ggplot2)   #plotting
library(earth)     #fitting MARS models
library(caret)     #tuning model parameters


head(final_data)


#create a tuning grid

hyper_grid <- expand.grid(degree = 1:3, nprune = seq(2, 50, length.out = 10) %>%
                            floor())



#make this example reproducible
set.seed(1)

#fit MARS model using k-fold cross-validation


x = final_data%>%
  select(-log_Sale_price)
x


y = final_data$log_Sale_price
y


cv_mars <- train(x,y,method = "earth",   metric = "RMSE",
                 trControl = trainControl(method = "cv", number = 10),
                 tuneGrid = hyper_grid)


#display model with lowest test RMSE

cv_mars$results %>%
  filter(nprune==cv_mars$bestTune$nprune, degree ==cv_mars$bestTune$degree) 

#display test RMSE by terms and degree
ggplot(cv_mars)

