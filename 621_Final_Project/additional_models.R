ols3 <- lm(price ~  sqft_living + sqft_above + sqft_living15 + grade)
summary(ols3)
AIC(ols3)
prediction_ols3 = predict(ols3,evaluation)
sqrt(mean((prediction_ols3 - evaluation$price)^2))


bc3 <- boxcox(ols3)
trans <- bc3$x[which.max(bc3$y)]
print(trans)

ols3t <- lm(price^-0.02 ~  sqft_living + sqft_above + sqft_living15 + grade)
summary(ols3t)
prediction_ols3t=predict(ols2tg,evaluation)^(-1/.02)
sqrt(mean((prediction_ols3t - evaluation$price)^2))


training$waterfront=as.factor(training$waterfront)
training$condition=as.factor(training$condition)
training$view=as.factor(training$view)

evaluation$waterfront=as.factor(evaluation$waterfront)
evaluation$condition=as.factor(evaluation$condition)
evaluation$view=as.factor(evaluation$view)


ols4 = lm(price ~  sqft_living + sqft_above + sqft_living15 + grade + 
            waterfront + view + condition,data=training )
summary(ols4)
prediction_ols4=predict(ols4,newdata=evaluation)
sqrt(mean((prediction_ols4 - evaluation$price)^2))
bc4 <- boxcox(ols4)
trans <- bc4$x[which.max(bc4$y)]
print(trans)


ols4t = lm(price^.02 ~  sqft_living + sqft_above + sqft_living15 + grade + 
             waterfront + view + condition,data=training )
summary(ols4t)
prediction_ols4t=predict(ols4t,newdata=evaluation)^(1/.02)
sqrt(mean((prediction_ols4t - evaluation$price)^2))
AIC(ols4t)


kctrain <- createDataPartition(y=kcdata$price, p=.75, list=FALSE)
if (!(19453 %in% kctrain)){
  kctrain = rbind(kctrain,19453)
}
training <- kcdata[kctrain,]
evaluation <- kcdata[-kctrain,]

training$type=training$price<450000
glm1=glm(type ~.-price-id,data=training,family=binomial)



ols3_t1=lm(price^-.02 ~  sqft_living + sqft_above + sqft_living15 + grade,
   data=training[glm_type$fitted.values<0.5,])
ols3_t2=lm(price^-.02 ~  sqft_living + sqft_above + sqft_living15 + grade,
            data=training[glm_type$fitted.values>0.5,])

eval_type=predict(glm1,newdata = evaluation,type="response")
prediction_ols3_t1t2=p1
t1=predict(ols3_t1,newdata = evaluation[eval_type<.5,])^(-1/.02)
t2=predict(ols3_t2,newdata = evaluation[!eval_type<.5,])^(-1/.02)
prediction_ols3_t1t2[eval_type<.5]=t1
prediction_ols3_t1t2[!eval_type<.5]=t2
sqrt(mean((prediction_ols3_t1t2 - evaluation$price)^2))


xgb1 = xgboost(data = data.matrix(training[,c("sqft_living",
                                              "sqft_above","sqft_living15","grade")]),
                         label = data.matrix(training$price), 
                         max.depth = 1, eta = 1, nround = 50,
                         nthread = 2, objective = "reg:linear")
prediction_xgb1=predict(xgb1,data.matrix(evaluation[,c("sqft_living",
                              "sqft_above","sqft_living15","grade")]))
sqrt(mean((prediction_xgb1 - evaluation$price)^2))
