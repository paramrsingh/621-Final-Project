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

#get fit and variables using caret
olsFit <- train(price ~ ., data=training, method="pls")
importance <- varImp(olsFit, scale=FALSE)
print(importance)

#models
ols1 <- lm(price ~ sqft_living)
#Multiple R-squared:  0.4914,  Adjusted R-squared:  0.4914 
summary(ols1)

ols2 <- lm(price ~ sqft_living + sqft_above + sqft_living15)
#Multiple R-squared:  0.5005,  Adjusted R-squared:  0.5004 
summary(ols2)

#compare these to ridge regression, lasso, regression trees, random forests, boosting, etc
#other categorical variables should prove interesting (bedrooms and bathrooms)