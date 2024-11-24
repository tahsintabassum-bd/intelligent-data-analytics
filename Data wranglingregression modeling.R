


library(mice)
library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)
library(MASS)
library(dplyr)
library(car)
library(corrr)
library(car)
library(Metrics)
library(ggplot2)
library(earth)
library(rgl)
library(mlbench)
library(stats)

getwd()

train <- read.csv("Train.csv")
test <- read.csv("Test.csv")
str(train)
View(train)
view(test)


#########################################################################################################
## a) Preparation & Modeling 
# I)	 Data visualization
#########################################################################################################



# Traffic data dependency on ‘browser’ column 

ggplot(data=train %>% count(browser, sort=TRUE) %>% mutate(tier=row_number()) %>% filter(tier<6), 
       aes(factor(browser, level=browser), n)) +geom_col(fill="#f68060", alpha=0.8) +
  labs(title="Top 5 Browsers from Traffic Data", x="Browsers Used during traffic", y="Number of Traffic") +
  theme_classic()




#Traffic data dependency on Continent variable 

ggplot(data=train %>% count(continent, sort=TRUE), 
       aes(factor(continent,level=continent), n)) +geom_col(fill="#515151", alpha=0.8) +
  labs(title="Continents Sorted by the Number of Traffic", x="Continents", y="Number of Traffics") +
  theme_classic()


# Traffic data dependency on Bounces

train$bounces <- as.logical(train$bounces)
ggplot(data=train) + geom_bar(aes(channelGrouping, fill=bounces), alpha=0.8) +
  labs(title="Bounces according to channels", x="Channels", y="Number of traffics") +
  theme_classic()


# Density plot of ‘visitStarttime’ 

ggplot(data=train) +
  geom_density(aes(visitStartTime)) +
  labs(title="Density function of 'visitStartTime'", y="Density") +
  theme_classic()


# Variance analysis

shapiro.test(sample(train$revenue, 5000)) #normality test, revenue data doesn't follow the normality
kruskal.test(revenue~deviceCategory, data=train) #therefore, we run kruskal wallis test on the data





#########################################################################################################
#(ii)
#missing value imputation 
#########################################################################################################

# replacing blank by NA and estimating missing value percentage 
train <- replace(train, train=="", NA)

z <- data_frame("Variables"=colnames(train), 
                "Missing Values (%)"=paste(round(as.numeric(colSums(is.na(train)))/nrow(train)*100, 2), "%"))
z




#deleting 'columns' having higher missing values 

train$sessionId <- NULL
train$subContinent <- NULL
train$country <- NULL
train$region <- NULL
train$metro <- NULL
train$city <- NULL
train$networkDomain <- NULL
train$campaign <- NULL
train$keyword <- NULL
train$adContent <- NULL
train$adwordsClickInfo.page <- NULL
train$adwordsClickInfo.slot <- NULL
train$adwordsClickInfo.gclId <- NULL
train$adwordsClickInfo.adNetworkType <- NULL
train$adwordsClickInfo.isVideoAd <- NULL
train$referralPath <- NULL
train$topLevelDomain <- NULL


## processing the column data


# bounces: convert the column including NA into logical (TRUE/FALSE)
train$bounces <- replace_na(train$bounces, 0)



# newVisits: Processing: convert the column including NA into logical (TRUE/FALSE)
train$newVisits <- replace_na(train$newVisits, 0)


#visitStartTime : Change the format into number & replace the original column

library(lubridate)
time <- lubridate::as_datetime(train$visitStartTime)
timetonumber <- hour(time)
train[["visitStartTime"]] <- timetonumber


# date: Changing Date to month

datetomonth <- train$date
month_number <- month(datetomonth)
train[["date"]] <- month_number

##timeSincelastVisit:: Changing from seconds to weeks

last_visit <- train$timeSinceLastVisit/604800
train[["timeSinceLastVisit"]] <- last_visit




#source:: creating new categories based on source

train %>% dplyr::select(source) %>% count(source, sort=T)

train <- train %>%
  mutate(source1 = case_when(
    str_detect(source, "google") ~ "google",
    str_detect(source, "youtube")  ~ "youtube"))

train$source1 <- replace_na(train$source1, "others")

train %>% dplyr::select(source1) %>% count(source1, sort=T)

train$source <- NULL

#browser 

train %>% dplyr::select(browser) %>% count(browser, sort=T)


train <- train %>%
  mutate(browser1 = case_when(
    str_detect(browser, "Safari") ~ "safari",
    str_detect(browser, "iPhone") ~ "safari",
    str_detect(browser, "Chrome")  ~ "chrome",
  ))

train$browser <- NULL

train$browser1 <- replace_na(train$browser1, "others")


#operatingSyste ::Creating sub-categories


train <- train %>%
  mutate(operatingSystem1 = case_when(
    str_detect(operatingSystem, "Macintosh") ~ "mac",
    str_detect(operatingSystem, "Windows") ~ "windows",
    str_detect(operatingSystem, "Windows Phone") ~ "windows",
    str_detect(operatingSystem, "Android")  ~ "android",
    str_detect(operatingSystem, "Samsung") ~ "android",
    str_detect(operatingSystem, "iOS") ~ "ios",
  ))
train$operatingSystem <- NULL
train$operatingSystem1 <- replace_na(train$operatingSystem1, "others")
train$operatingSystem1 <- as.factor(train$operatingSystem1)




#turning the categorical/character variable into factors

train$channelGrouping <- as.factor(train$channelGrouping)
train$browser1 <- as.factor(train$browser1)
train$operatingSystem <- as.factor(train$operatingSystem)
train$deviceCategory <- as.factor(train$deviceCategory)
train$continent <- as.factor(train$continent)
train$source1 <- as.factor(train$source1)
train$medium <- as.factor(train$medium)


#aggregating the dataset into the customers (using aggregate function)

x <- train %>% dplyr::select(custId, revenue)
x <- aggregate(x, by=list(x$custId), sum)
x$custId <- NULL
x <- x %>% rename(custId=Group.1, custRevenue=revenue)
x$targetRevenue <- log(x$custRevenue + 1)


#channelGrouping :: into distinct number of channels

x1 <- unique(train %>% dplyr::select(custId, channelGrouping)) %>% 
  mutate(numOfCh=1) %>% dplyr::select(custId, numOfCh)
x1 <- aggregate(x1, list(x1$custId), sum)
x1$custId <- NULL
x1 <- x1 %>% rename(custId=Group.1)
x <- left_join(x, x1)


#visitNumber :: into maximum visit number

x1 <- train %>% dplyr::select(custId, visitNumber)
x1 <- aggregate(x1, list(x1$custId), max)
x1$custId <- NULL
x1 <- x1 %>% rename(custId=Group.1, maxVisitNum=visitNumber)
x <- left_join(x, x1)



#browser :: the first occurrence for that customer

x1 <- unique(train %>% dplyr::select(custId, browser1))
x1 <- x1[!duplicated(x1$custId), ]
x <- left_join(x, x1)
x <- x %>% rename(browser=browser1)
x$browser <- as.factor(x$browser)

#operatingSystem :: the first occurrence for that customer

x1 <- unique(train %>% dplyr::select(custId, operatingSystem1))
x1 <- x1[!duplicated(x1$custId), ]
x <- left_join(x, x1)
x <- x %>% rename(os=operatingSystem1)

#continent
x1 <- unique(train %>% dplyr::select(custId, continent))
x1 <- x1[!duplicated(x1$custId), ]
x <- left_join(x, x1)


#source :: the maximum occurrence for that customer

x1 <- unique(train %>% dplyr::select(custId, source1))
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
x1 <- aggregate(x1, list(x1$custId), Mode)
x1$Group.1 <- NULL
x <- left_join(x, x1)

##isTrueDirect:: total number of direct channels  

x1 <- train %>% dplyr::select(custId, isTrueDirect)
x1 <- aggregate(x1, list(x1$custId), sum)
x1$custId <- NULL
x1 <- x1 %>% rename(custId=Group.1, numOfDirect=isTrueDirect)
x <- left_join(x, x1)

#pageviews ::  total number of bounces by each customer  

x1 <- train %>% dplyr::select(custId, pageviews)
x1 <- aggregate(x1, list(x1$custId), sum)
x1$custId <- NULL
x1 <- x1 %>% rename(custId=Group.1)
x <- left_join(x, x1)


#bounces :: total number of bounces by each customer  
x1 <- train %>% dplyr::select(custId, bounces)
x1 <- aggregate(x1, list(x1$custId), sum)
x1$custId <- NULL
x1 <- x1 %>% rename(custId=Group.1)
x <- left_join(x, x1)

#newVisits  :: mean of new visit for each customers 

x1 <- train %>% dplyr::select(custId, newVisits)
x1 <- aggregate(x1, list(x1$custId), mean)
x1$newVisits <- ifelse(x1$newVisits == 1, TRUE, FALSE)
x1$Group.1 <- NULL
x <- left_join(x, x1)


#date ::  Most frequent date for each customers 

x1 <- train %>% dplyr::select(custId, date)
x1 <- aggregate(x1, list(x1$custId), Mode)
x1$Group.1 <- NULL
x <- left_join(x, x1)

#visitStartTime :: Most frequent visit time for each customers 

x1 <- train %>% dplyr::select(custId, visitStartTime)
x1 <- aggregate(x1, list(x1$custId), Mode)
x1$Group.1 <- NULL
x <- left_join(x, x1)

#timeSinceLastVisit  :: Average time each customer spent  
x1 <- train %>% dplyr::select(custId, timeSinceLastVisit)
x1 <- aggregate(x1, list(x1$custId), mean)
x1$Group.1 <- NULL
x <- left_join(x, x1)


str(x)


##################################
#missing value imputations
##################################



# pageviews :: mean value imputation 
sum(is.na(x$pageviews)) #there's missing values in 'pageviews' column. 

x$pageviews[is.na(x$pageviews)] <- mean(x$pageviews, na.rm=T)


# continent ::  mode value imputation


sum(is.na(x$continent))
x$continent[is.na(x$continent)] <- Mode(x$continent)

#################################


#outlier/skewness optimization of the numeric variables 


#pageviews

library(EnvStats)
#boxcox(x$pageviews, optimize=TRUE)
x$pageviews <- (x$pageviews^-0.3240505-1)/-0.3240505



#maxVisitNum

#boxcox(x$maxVisitNum, optimize=TRUE)
x$maxVisitNum <- (x$maxVisitNum^-0.7696944-1)/-0.7696944


#isTrueDirect 
#boxcox(x$numOfDirect, optimize=TRUE)
x$numOfDirect <- ((x$numOfDirect+1)^-0.6076567)/-0.6076567


#################################################################

#preparation on test
test = read.csv("Test.csv")
test

test <- replace(test, test=="", NA)



##deleting 'columns' that has higher missing values 
test$sessionId <- NULL
test$subContinent <- NULL
test$country <- NULL
test$region <- NULL
test$metro <- NULL
test$city <- NULL
test$networkDomain <- NULL
test$campaign <- NULL
test$keyword <- NULL
test$adContent <- NULL
test$adwordsClickInfo.page <- NULL
test$adwordsClickInfo.slot <- NULL
test$adwordsClickInfo.gclId <- NULL
test$adwordsClickInfo.adNetworkType <- NULL
test$adwordsClickInfo.isVideoAd <- NULL
test$referralPath <- NULL
test$topLevelDomain <- NULL



## processing the column data

# bounces  :: replace NA by 0

test$bounces <- replace_na(test$bounces, 0)



# newVisits :: replace NA by 0

test$newVisits <- replace_na(test$newVisits, 0)





#source :: creating new categories based on source



test %>% dplyr:: select(source) %>% count(source, sort=T)

test <- test %>%
  mutate(source1 = case_when(
    str_detect(source, "google") ~ "google",
    str_detect(source, "youtube")  ~ "youtube"))

test$source1 <- replace_na(test$source1, "others")

test %>% dplyr::select(source1) %>% count(source1, sort=T)

test$source <- NULL



#visitStartTime :: Change the format into number & replace the original column


library(lubridate)
time <- lubridate::as_datetime(test$visitStartTime)
timetonumber <- hour(time)
test[["visitStartTime"]] <- timetonumber


#date  :: Changing Date to month

datetomonth <- test$date
month_number <- month(datetomonth)
test[["date"]] <- month_number


#timeSinceLastVisit  :: Changing Lastvisittime from seconds to weeks
last_visit <- test$timeSinceLastVisit/604800
test[["timeSinceLastVisit"]] <- last_visit




#browser 
test <- test %>%
  mutate(browser1 = case_when(
    str_detect(browser, "Safari") ~ "safari",
    str_detect(browser, "iPhone") ~ "safari",
    str_detect(browser, "Chrome")  ~ "chrome",
  ))

test$browser <- NULL

test$browser1 <- replace_na(test$browser1, "others")



##operatingSystem :: Creating sub-categories




test <- test %>%
  mutate(operatingSystem1 = case_when(
    str_detect(operatingSystem, "Macintosh") ~ "mac",
    str_detect(operatingSystem, "Windows") ~ "windows",
    str_detect(operatingSystem, "Windows Phone") ~ "windows",
    str_detect(operatingSystem, "Android")  ~ "android",
    str_detect(operatingSystem, "Samsung") ~ "android",
    str_detect(operatingSystem, "iOS") ~ "ios",
  ))
test$operatingSystem <- NULL
test$operatingSystem1 <- replace_na(test$operatingSystem1, "others")
test$operatingSystem1 <- as.factor(test$operatingSystem1)



##turning the categorical/character variable into factors

test$channelGrouping <- as.factor(test$channelGrouping)
test$browser1 <- as.factor(test$browser1)
test$operatingSystem <- as.factor(test$operatingSystem)
test$deviceCategory <- as.factor(test$deviceCategory)
test$continent <- as.factor(test$continent)
test$source1 <- as.factor(test$source1)
test$medium <- as.factor(test$medium)

##aggregating the dataset into the customers (using aggregate function): New test data set (y)

y <- unique(test %>% dplyr::select(custId))

# #channelGrouping :: Segregate into number of channels & add with y



y1 <- unique(test %>% dplyr::select(custId, channelGrouping)) %>% mutate(numOfCh=1) %>% dplyr::select(custId, numOfCh)
y1 <- aggregate(y1, list(y1$custId), sum)
y1$custId <- NULL
y1 <- y1 %>% rename(custId=Group.1)
y <- left_join(y, y1)


##visitNumber :: into maximum visit number



y1 <- test %>% dplyr::select(custId, visitNumber)
y1 <- aggregate(y1, list(y1$custId), max)
y1$custId <- NULL
y1 <- y1 %>% rename(custId=Group.1, maxVisitNum=visitNumber)
y <- left_join(y, y1)

#browser 
y1 <- unique(test %>% dplyr::select(custId, browser1))
y1 <- y1[!duplicated(y1$custId), ]
y <- left_join(y, y1)
y <- y %>% rename(browser=browser1)

#operatingSystem
y1 <- unique(test %>% dplyr::select(custId, operatingSystem1))
y1 <- y1[!duplicated(y1$custId), ]
y <- left_join(y, y1)




y <- y %>% rename(os=operatingSystem1)

#continent
y1 <- unique(test %>% dplyr::select(custId, continent))
y1 <- y1[!duplicated(y1$custId), ]
y <- left_join(y, y1)




#source
y1 <- unique(test %>% dplyr::select(custId, source1))
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
y1 <- aggregate(y1, list(y1$custId), Mode)
y1$Group.1 <- NULL
y <- left_join(y, y1)



#isTrueDirect

y1 <- test %>% dplyr:: select(custId, isTrueDirect)
y1 <- aggregate(y1, list(y1$custId), sum)
y1$custId <- NULL
y1 <- y1 %>% rename(custId=Group.1, numOfDirect=isTrueDirect)
y <- left_join(y, y1)

#pageviews
y1 <- test %>% dplyr::select(custId, pageviews)
y1 <- aggregate(y1, list(y1$custId), sum)
y1$custId <- NULL
y1 <- y1 %>% rename(custId=Group.1)
y <- left_join(y, y1)

#bounces
y1 <- test %>% dplyr::select(custId, bounces)
y1 <- aggregate(y1, list(y1$custId), sum)
y1$custId <- NULL
y1 <- y1 %>% rename(custId=Group.1)
y <- left_join(y, y1)

#newVisits

y1 <- test %>% dplyr::select(custId, newVisits)
y1 <- aggregate(y1, list(y1$custId), mean)
y1$newVisits <- ifelse(y1$newVisits == 1, TRUE, FALSE)
y1$Group.1 <- NULL
y <- left_join(y, y1)

#date

y1 <- test %>% dplyr::select(custId, date)
y1 <- aggregate(y1, list(y1$custId), Mode)
y1$Group.1 <- NULL
y <- left_join(y, y1)

#visitStartTime

y1 <- test %>% dplyr:: select(custId, visitStartTime)
y1 <- aggregate(y1, list(y1$custId), Mode)
y1$Group.1 <- NULL
y <- left_join(y, y1)

#timeSinceLastVisit

y1 <- test %>% dplyr::select(custId, timeSinceLastVisit)
y1 <- aggregate(y1, list(y1$custId), mean)
y1$Group.1 <- NULL
y <- left_join(y, y1)


summary(y)

#missing value imputations

# pageviews
sum(is.na(y$pageviews)) 
y$pageviews[is.na(y$pageviews)] <- mean(y$pageviews, na.rm=T) 


# continent

sum(is.na(y$continent))
y$continent[is.na(y$continent)] <- Mode(y$continent)

################################################

#outlier/skewness


#pageviews

#boxcox(y$pageviews, optimize=TRUE)
y$pageviews <- (y$pageviews^-0.3313906-1)/-0.3313906


#maxvisitnum

#boxcox(y$maxVisitNum, optimize=TRUE)
y$maxVisitNum <- (y$maxVisitNum^-0.7828123-1)/-0.7828123


#numofDirect

#boxcox(y$numOfDirect+1, optimize=TRUE)
y$numOfDirect <- ((y$numOfDirect+1)^-0.6161303)/-0.6161303

summary(x)
summary(y)

nrow(x)
nrow(y)

###################################################################################
## model
###################################################################################



ncol(x)
ncol(y)


# making the homogeneous columns for train & test data

x$custId <- NULL
x$custRevenue <- NULL




###################################
###################################
##################################

#OLS Model

library(MASS)
fit <- lm(targetRevenue ~ ., data=x)
summary(fit)

fit1 <- stepAIC(fit, directon="both")

fit1

summary(fit1)


library(qpcR)
RMSE(fit1)



lmpred <- predict(fit1, y)

submission_ols <- data.frame(custId=y$custId, predRevenue=lmpred)

view(submission_ols)

library(here)
write.csv(submission_ols, here::here("OLS.csv"), row.names = FALSE)

xx <- read.csv("OLS.csv")

xx <- xx %>%
  mutate(predRevenue = ifelse(predRevenue < 0, 0, predRevenue))

# Export the result to CSV
write.csv(xx, "OLS2.csv", row.names = FALSE)


#penalized models

library(fastDummies)
library(caret)
library(elasticnet)


nrow(x)

xx <- x
xx <- dummy_cols(xx)
xx <- xx %>% dplyr::select(-colnames(xx %>% select_if(is.factor)))
xx$newVisits <- ifelse(xx$newVisits == TRUE, 1, 0)

yy <- y
yy <- dummy_cols(yy)
yy <- yy %>% dplyr::select(-colnames(yy %>% select_if(is.factor)))
yy$newVisits <- ifelse(yy$newVisits == TRUE, 1, 0)

nrow(yy)

#ridge Model
ridge_fit <- train(targetRevenue ~ ., data=x, method="ridge")
ridge_fit
plot(ridge_fit)
ridge_model <- enet(x=as.matrix(xx[, -1]), y=xx[, 1], lambda=0.0001)
ridgePred <- predict(ridge_model, newx=as.matrix(yy[, -1]), s=1, mode="fraction", type="fit")
submission_ridge <- data.frame(custId=y$custId, predRevenue=ridgePred$fit)

view(submission_ridge)
summary(submission_ridge)


######################################################## Corrected MARS##########################

# mars

view(x)
view(y)


x2 = x[1:47247,]

view (x2)




xx <- x2
xx <- dummy_cols(xx)
xx <- xx %>% dplyr::select(-colnames(xx %>% select_if(is.factor)))

xx$newVisits <- ifelse(xx$newVisits == TRUE, 1, 0)

yy <- y
yy <- dummy_cols(yy)
yy <- yy %>% dplyr::select(-colnames(yy %>% select_if(is.factor)))
yy$newVisits <- ifelse(yy$newVisits == TRUE, 1, 0)

nrow(xx)

marsFit <- earth(targetRevenue~ .,
                 data=x2,
                 degree=3,nk=50,pmethod="cv",nfold=5,ncross=5)


summary(marsFit)

mars1_model <- earth(x=as.matrix(xx[, -1]), y=xx[, 1])

marsPred1 <- predict(mars1_model, newx=as.matrix(yy[, -1]), s=0.45, mode="fraction")


submission_mars <- data.frame(custId=y$custId, predRevenue=marsPred1)


view(submission_mars)
summary(submission_mars)
write.csv(submission_mars, here::here("mars.csv"), row.names = FALSE)
plotmo(marsFit)
plot(marsFit)


#######################################################################################33


#lasso Model
fitControl <- trainControl(method="cv",number=5)
lasso_fit <- train(targetRevenue ~ ., data=x, method="lasso", trControl=fitControl)
lasso_fit
plot(lasso_fit)

lasso_model <- enet(x=as.matrix(xx[, -1]), y=xx[, 1], lambda=0, normalize=TRUE)

lassoPred <- predict(lasso_model, newx = as.matrix(yy[, -1]), s=0.9, mode = "fraction", type="fit")

submission_lasso <- data.frame(custId=y$custId, predRevenue=lassoPred$fit)


view(submission_lasso)
#write.csv(submission_lasso, row.names=FALSE, "~/Desktop/submission_lasso.csv")
write.csv(submission_lasso, here::here("LASSO.csv"), row.names = FALSE)

#enet model

enetGrid <- expand.grid(lambda=seq(0,.15,length=5),fraction=seq(0.45,.9,length=30))
enet_fit <- train(targetRevenue~.*., data=x, method="enet", trControl=fitControl, tuneGrid=enetGrid)
summary(enet_fit)


view(enet_fit)

enet_model <- enet(x=as.matrix(xx[, -1]), y=xx[, 1], lambda=0, normalize=TRUE)
enetPred <- predict(enet_model, newx=as.matrix(yy[, -1]), s=0.45, mode="fraction", type="fit")
submission_enet <- data.frame(custId=y$custId, predRevenue=enetPred$fit)

view(submission_enet)  
write.csv(submission_enet, here::here("ENET.csv"), row.names = FALSE)

#######################################################################################33

#SVR Model

# Load necessary library
library(e1071)

# Prepare the train and test data for SVR
#xx <- dummy_cols(xx)
xx <- xx %>% dplyr::select(-colnames(xx %>% select_if(is.factor)))
xx$newVisits <- ifelse(xx$newVisits == TRUE, 1, 0)

#yy <- dummy_cols(yy)
yy <- yy %>% dplyr::select(-colnames(yy %>% select_if(is.factor)))
yy$newVisits <- ifelse(yy$newVisits == TRUE, 1, 0)

# SVR model training
svr_model <- svm(targetRevenue ~ ., data=xx, kernel="radial")

# Prediction on test data
svr_pred <- predict(svr_model, newdata=as.matrix(yy[, -1]))

# Prepare the submission file
submission_svr <- data.frame(custId=y$custId, predRevenue=svr_pred)

# View the result
View(submission_svr)

# Optionally, save the results to a CSV file
write.csv(submission_svr, here::here("SVR.csv"), row.names = FALSE)

yy <- read.csv("OLS.csv")

yy <- xx %>%
  mutate(predRevenue = ifelse(predRevenue < 0, 0, predRevenue))


# Export the result to CSV
write.csv(yy, "OLS2.csv", row.names = FALSE)






