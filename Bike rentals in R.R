install.packages("ggplot2")
library(ggplot2)
library(corrgram)
library(Amelia)
library(dplyr)
library(caTools) # sample.split is in this library

setwd("C:/Users/Mahesh/Desktop/Final Project/New folder")
bike <- read.csv("day.csv")
bike <- select(bike,-instant)
sum(is.na(bike)) # = 0 , which indicates no missing values.
missmap(bike,main="Bike Count", col = c("yellow","black"),legend = FALSE)
str(bike)
#Conversion of numeric columns with discrete values into factors
#weathersit has 4 values i.e. 1,2,3,4 whose details is given below.I'll factorize it.
#1: Clear, Few clouds, Partly cloudy, Partly cloudy
#2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
#3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered
#clouds.
#4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog.
bike$weathersit <- as.factor(bike$weathersit)
#Season : 1=spring,2=summer,3=fall,4=winter
#spring runs from march to may , summer from may to august , fall from september to november , winter from november  to feb
bike$season <- as.factor(bike$season)  

bike$yr <- factor(bike$yr , levels = c(0,1) , labels("2011","2012"))
#Now onto the weekday column - range exists from  0 to 6 where 0 represents Sunday and 6 Saturday and the numbers in between represents Monday to 
#Friday in chronicle manner.
#I'll factorize it 
bike$weekday <- factor(bike$weekday , labels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
#Now , coming onto the workingday attribute : 0 represents no working day and 1 represents working day.
#And for holiday : 0 represents weekdays(including the weekends) and 1 represents holiday

#For humidity , we'll multiply it with the hum column,and windspeed , we'll multiply with 67 
bike$hum <- bike$hum * 100 
bike$windspeed <- bike$windspeed * 67 
#####Creating a copy
df <- bike

####Exploratory Analysis

ggplot(bike,aes(as.POSIXct(dteday),cnt)) + geom_point(aes(color=temp)) + scale_color_continuous(low='#55D8CE',high='#FF6E2E') +theme_bw() 
#From the above graph , we can observe that the count increases with year in general 
#And is maximum steadily during the fall region and falls down during Winter and continues with the same trend only with increased count 



ggplot(bike, aes(season, cnt)) + geom_boxplot(aes(color=season))
#Observations : bike count is least during the spring , and peaks during the season of fall , also no outliers which explains the normal distribution.
#Also , the other facet of the data-set is that average count or average cnt is steady throughout the season 2 , 3 and 4


ggplot(bike, aes(temp,cnt)) + geom_point(aes(color=temp)) + scale_color_continuous(low='#55D8CE',high='#FF6E2E') + xlab("Temperature")+ylab("Count")
#The graph shows that there is a linear relationship between temperature and bikecount
#The count increases steadily with temp till a certain temperature and then starts decreasing.

ggplot(bike, aes(weathersit, cnt)) + geom_boxplot(aes(color=weathersit))
#Bike count is maximum for weather 1 i.e. the most comfortable temperature and decreases as 
#weathersit or weather becomes more harsh as seen with bikecount decreasing with increasing harshness of the weather conditions which makes sense.

ggplot(bike,aes(weekday,cnt)) + geom_boxplot(aes(color=as.factor(weekday)))
#The maximum no bikecount belongs to the Fridays and Saturdays - makes sense as it's on the weekend.
#But at the sametime , we can also see that the average count is uniform throughout the days which implies that the maybe
#relation between workingday and count  isn't too strong

ggplot(bike,aes(mnth,cnt))+geom_boxplot() + xlab("Month") + ylab("Count")
#The plot shows that the average count increases linearly with month until July(month 7 ) , and decreases linearly from September to December 

ggplot(bike , aes(windspeed,cnt)) + geom_point()

#Correlation Analysis
num.cols <- sapply(bike, is.numeric)
cor.data <- cor(bike[,c('cnt','windspeed')])
cor.data

corrgram(bike,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)
#atemp is highly co-related with temp in a positive fashion , and both of them yields a similar relationship , so therefore I'll remove atemp
#Further conclusions off of Correlation matrix : 
#1. variable temp and yr looks highly promising feature for predicting cnt
#2. Variable casual and registered directly contributes to cnt , therefore they have been removed
#And although the cnt isn't heavily related with many other independent varibale , we'll still keep these predictor variables 
#since correlation analysis only depicts the linear relationship between variables


bike <- select(bike,-atemp,-registered,-casual)
#bike <- select(bike,-dteday)

bike <- bike[,-2]#removing a dteday variable
#Building a linear regression model 

library(caTools) # sample.split is in this library
set.seed(101)
sample <- sample.split(bike$cnt, SplitRatio = 0.75)
train  <- subset(bike,sample==TRUE)
test <- subset(bike , sample == FALSE)
model <- lm(cnt ~ ., train )
summary(model)
#(From the model displayed , it can be observed that the no of bike on rents is heavily dependent on the seasonality and environment conditions
#R Squared value turns out to be 56.39%
prediction_LR = predict(model,test[,1:10])
prediction_LR = as.data.frame(prediction_LR)

#Cross Validation technique
modelLr <- train(cnt ~., bike , method = "lm", trControl=trainControl(method= "cv",number=10,verboseIter=TRUE))
modelLr

###Decision Tree Model 
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)
library(MASS)
fit = rpart(cnt ~ ., data = train , method = "anova")
prp(fit)



prediction_DT = predict(fit,test[-11])
modelDT <- train(cnt ~., bike, method = "rpart", trControl=trainControl(method= "cv",number=10,verboseIter=TRUE))



#Building a random forest regressor 
install.packages("randomForest")
library(randomForest)
modelRf <- randomForest(train$cnt ~., data = train)
importance(modelRf)


predictions_RF <- as.data.frame(predict(modelRf,test[-11]))

modelRF <- train(cnt ~., bike , method = "rf", trControl=trainControl(method= "cv",number=10,verboseIter=TRUE))
modelRF

#As the value of RMSE , MAE is minimum for Random Forest , we'll go for Random Forest for our model selection
