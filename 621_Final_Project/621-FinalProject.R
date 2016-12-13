kcdata <- read.csv('kc_house_data.csv')

#Exploration
summary(kcdata)
head(kcdata)

# no missing observations
sum(!complete.cases(kcdata))
#check Year Renovated for missing observations that are set to 0
#20699. Replace with NA's to support lm family of functions
sum(kcdata$yr_renovated=='0')
is.na(kcdata$yr_renovated) <- !kcdata$yr_renovated

#Transformation
#dropping the lat and long, geospatial not needed with zipcode
kcdata <- kcdata[c(-18,-19)]
#transform the date field
kcdata$date <- gsub('T000000','', kcdata$date)
kcdata$date <- as.Date(kcdata$date, format="%Y%m%d")

#Exploration plots
par(mfrow=c(4,4))
plot(kcdata$date, kcdata$price, xlab='date', ylab='price', main='Date')
# 33 bedrooms is nuts, and probably an outlier that should be removed
plot(kcdata$bedrooms, kcdata$price, xlab='bedrooms', ylab='price', main='Bedrooms')
plot(kcdata$bathrooms, kcdata$price, xlab='bathrooms', ylab='price', main='Bathrooms')
plot(kcdata$sqft_living, kcdata$price, xlab='sqft_living', ylab='price', main='Sqft_Living')
plot(kcdata$sqft_lot, kcdata$price, xlab='sqft_lot', ylab='price', main='Sqft_Lot')
plot(kcdata$floors, kcdata$price, xlab='floors', ylab='price', main='Floors')
plot(kcdata$view, kcdata$price, xlab='view', ylab='price', main='View')
plot(kcdata$condition, kcdata$price, xlab='condition', ylab='price', main='Condition')
plot(kcdata$grade, kcdata$price, xlab='grade', ylab='price', main='Grade')
plot(kcdata$sqft_above, kcdata$price, xlab='sqft_above', ylab='price', main='Sqft_Above')
plot(kcdata$sqft_basement, kcdata$price, xlab='sqft_basement', ylab='price', main='Sqft_Basement')
plot(kcdata$yr_built, kcdata$price, xlab='yr_built', ylab='price', main='Year Built')
plot(kcdata$yr_renovated, kcdata$price, xlab='yr_renovated', ylab='price', main='Year Renovated')
plot(kcdata$zipcode, kcdata$price, xlab='zipcode', ylab='price', main='Zipcode')
plot(kcdata$sqft_living15, kcdata$price, xlab='sqft_living15', ylab='price', main='Sqft_Living15')
plot(kcdata$sqft_lot15, kcdata$price, xlab='sqft_lot15', ylab='price', main='Sqft_Lot15')

#segment into training and evaluation data frames
library(caret)
set.seed(101)
kctrain <- createDataPartition(y=kcdata$price, p=.75, list=FALSE)
if (!(19453 %in% kctrain)){
  kctrain = rbind(kctrain,19453)
}
training <- kcdata[kctrain,]
evaluation <- kcdata[-kctrain,]
attach(training)

#get fit and variables using caret
olsFit <- train(price ~ ., data=training, method="pls")
importance <- varImp(olsFit, scale=FALSE)
print(importance)

#models
library(moments)
library(MASS)
ols1 <- lm(price ~ sqft_living)
#Multiple R-squared:  0.4914,  Adjusted R-squared:  0.4914 
summary(ols1)
par(mfrow=c(2,2))
plot(ols1)
#additional residual plot
ols1residual1 <- resid(ols1)
hist(ols1residual1)
#some positive skew, 2.955704
skewness(ols1residual1)
#aply box-cox
bc1 <- boxcox(ols1)
trans <- bc1$x[which.max(bc1$y)]
#apply transform and replot
ols1t <- lm(price^-0.02 ~ sqft_living)
summary(ols1t)
par(mfrow=c(2,2))
plot(ols1t)
ols1residual1t <- resid(ols1t)
hist(ols1residual1t)
skewness(ols1residual1t)

ols2 <- lm(price ~ sqft_living + sqft_above + sqft_living15)
#Multiple R-squared:  0.5005,  Adjusted R-squared:  0.5004 
summary(ols2)
par(mfrow=c(2,2))
plot(ols2)
#additional residual plot
olsresidual2 <- resid(ols2)
hist(olsresidual2)
#positive skew, 3.14
skewness(olsresidual2)
#aply box-cox
bc2 <- boxcox(ols2)
trans2 <- bc2$x[which.max(bc2$y)]
#apply transform and replot
ols2t <- lm(price^-0.02 ~  sqft_living + sqft_above + sqft_living15)
#Multiple R-squared:  0.5054,  Adjusted R-squared:  0.5053
summary(ols2t)
par(mfrow=c(2,2))
plot(ols2t)
olsresidual2t <- resid(ols2t)
hist(olsresidual2t)
skewness(olsresidual2t)
#The fit values aren't that bad from the diagnostic plots, all things considered


#compare these to ridge regression, lasso, regression trees, random forests, boosting, etc

library(glmnet)

#change NA's in yr_renovated back to 0's for sparse matrix support
training$yr_renovated[is.na(training$yr_renovated)] <- 0
attach(training)
X <- sparse.model.matrix(~.,training[c(-1,-2,-3)])
#lasso
lassofit <- glmnet(X, price, alpha=1, family='gaussian')
plot(lassofit, xvar="lambda", label=TRUE)
print(lassofit)
summary(lassofit)
coef(lassofit)[,10]
# use the package to select the optimal
lassofit.cv <- cv.glmnet(X, price)
plot(lassofit.cv)
# gives the coefficient for the best model
coef(lassofit.cv)

#ridge
ridgefit <- glmnet(X, price, alpha=0, family='gaussian')
plot(ridgefit, xvar="lambda", label=TRUE)
print(ridgefit)
coef(ridgefit)[,10]
ridgefit.cv <- cv.glmnet(X, price)
plot(ridgefit.cv)
coef(ridgefit.cv)




####Prediction
predict_ols2=predict(ols2,evaluation)
sqrt(mean((predict_ols2 - evaluation$price)^2))
predict_ols2t=predict(ols2t,evaluation)^(-1/.02)
sqrt(mean((predict_ols2t - evaluation$price)^2))

#Write Prediction
prediction_ols2=NULL
prediction_ols2$id=evaluation$id
prediction_ols2$price=predict_ols2
write.csv(prediction,"predictions_ols2.csv",row.names=F)

prediction_ols2t=NULL
prediction_ols2t$id=evaluation$id
prediction_ols2t$price=predict_ols2t
write.csv(prediction,"predictions_ols2t.csv",row.names=F)

#predict for glmnet
evaluation$yr_renovated[is.na(evaluation$yr_renovated)] <- 0
attach(evaluation)
evalX <- sparse.model.matrix(~.,evaluation[c(-1,-2,-3)])
lassoprediction <- predict(lassofit.cv, evalX)
ridgeprediction <- predict(ridgefit.cv, evalX)

sqrt(mean((lassoprediction - evaluation$price)^2))
sqrt(mean((ridgeprediction - evaluation$price)^2))
