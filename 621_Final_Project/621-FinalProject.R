kcdata <- read.csv('kc_house_data.csv')

#Exploration
summary(kcdata)
head(kcdata)

# no missing observations
sum(!complete.cases(kcdata))

#Transformation
#dropping the lat and long, geospatial not needed with zipcode
kcdata <- kcdata[c(-18,-19)]

#segment into training and evaluation data frames
library(caret)
kctrain <- createDataPartition(y=kcdata$price, p=.75, list=FALSE)
training <- kcdata[kctrain,]
evaluation <- kcdata[-kctrain,]
attach(training)

#Exploratory plots
plot(sqft_living, price)
plot(sqft_above, price)
plot(sqft_living15, price)

#get fit and variables using caret
olsFit <- train(price ~ ., data=training, method="pls")
importance <- varImp(olsFit, scale=FALSE)
print(importance)

#models
ols1 <- lm(price ~ sqft_living)
#Multiple R-squared:  0.4914,  Adjusted R-squared:  0.4914 
summary(ols1)
par(mfrow=c(2,2))
plot(ols1)
#additional residual plot
ols1residual1 <- resid(ols1)
plot(sqft_living, ols1residual1)


ols2 <- lm(price ~ sqft_living + sqft_above + sqft_living15)
#Multiple R-squared:  0.5005,  Adjusted R-squared:  0.5004 
summary(ols2)
par(mfrow=c(2,2))
plot(ols2)
#additional residual plot
ols1residual2 <- resid(ols2)
plot(sqft_living + sqft_above + sqft_living15, ols1residual2)

#The fit values aren't that bad from the diagnostic plots, all things considered


#compare these to ridge regression, lasso, regression trees, random forests, boosting, etc
#other categorical variables should prove interesting (bedrooms and bathrooms)
library(glmnet)

#lasso
X <- sparse.model.matrix(~.,training[c(-1,-2,-3)])
lassofit <- glmnet(X, price, alpha=1, family='gaussian')
#ridge
ridgefit <- glmnet(X, price, alpha=0, family='gaussian')
#plots
plot(lassofit, xvar="lambda", label=TRUE)
plot(ridgefit, xvar="lambda", label=TRUE)
print(lassofit)
print(ridgefit)
coef(lassofit)[,10]
coef(ridgefit)[,10]

