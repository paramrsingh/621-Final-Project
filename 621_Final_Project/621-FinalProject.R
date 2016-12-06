kcdata <- read.csv('kc_house_data.csv')

#Exploration
summary(kcdata)
head(kcdata)

# no missing observations
sum(!complete.cases(kcdata))



#Transformation
#dropping the lat and long, geospatial not needed


#segment into training and evaluation data frames