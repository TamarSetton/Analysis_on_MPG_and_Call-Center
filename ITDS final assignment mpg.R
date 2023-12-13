#analysis of MPG

#installation of all necessary packages
install.packages("tidyverse")
library(tidyverse)

mpg <- read.csv("/Users/tamarsetton/Downloads/auto-mpg.csv")

str(mpg)
mpg
glimpse(mpg)
summary(mpg)

#to install graphing packages
install.packages("ggplot2")
library("ggplot2")

install.packages("ggthemes")
library(ggthemes)

#A bar graph of the acceleration. Interestingly, it is normally distributed
ggplot(mpg, aes(x = acceleration)) +
  geom_bar()

#scatter plot of model year to mpg
ggplot(
  data = mpg,
  mapping = aes(x = model.year, y = mpg)
) +
  geom_point()
  #positive correlation between model year and mpg. This runs parallel to previous Tableau observations. 


#A scatter plot comparing number of cylinders to mpg
ggplot(mpg, aes(x = cylinder, y = mpg)) +
  geom_point()
  #we do see some sort of negative correlation between cylender and mpg. 
  #This is confirmed by the output here,-0.7753963 
cor(mpg$cylinder, mpg$mpg)


#While running lines, I found that the horsepower column was saved as charecters. Since this is something i would like to work with, I changed it to numeric.
mpg$horsepower<- as.numeric(mpg$horsepower)
  #to check that it worked
glimpse(mpg)


#to create new data frame with only first 300 values
mpg300 <- head(mpg, 300)
  #to make sure the numeric status of horsepower carried through
glimpse(mpg300)
  #to check that it is 300 rows
nrow(mpg300)

#linear regression
linear_r <- lm(mpg ~ horsepower, data = mpg300)
summary(linear_r)
# Multiple R-squared:0.6408
# Adjusted R-squared: 0.6396 
# Complete Linear Regression equation: mpg = -0.125105(horsepower) + 34.794687
#overall a decent model

linear2 <- lm(mpg ~ model.year, data = mpg300)
summary(linear2)
#Multiple R-squared:  0.07966
#Adjusted R-squared:  0.07657
# Complete Linear Regression equation: mpg = 0.6388(model.year) - 26.7513 
# pretty good model, although at first I accidentally ran it on the entire data set mpg and the R squares werent so good, so I can guess it wont predict well.


#multiple linear regressions
mlr1 <- lm(mpg ~ displacement + cylinder, data = mpg300)
summary(mlr1)
#Multiple R-squared:  0.7161
#Adjusted R-squared:  0.7142
# Complete Linear Regression equation: mpg = -0.039992(displacement) − 0.567677(cylinder) + 32.683612
#displacement is a good predictor, cylinder not so much. 


mlr2 <- lm(mpg ~ horsepower + weight, data = mpg300)
summary(mlr2)
#Multiple R-squared:  0.782
#Adjusted R-squared:  0.7805
# Complete Linear Regression equation: mpg = -0.0264219(horsepower) -0.0052317(weight) + 40.1577216
#pretty good model


mlr3 <- lm(mpg ~ horsepower + acceleration, data = mpg300)
summary(mlr3)
#Multiple R-squared:  0.6709
#Adjusted R-squared:  0.6686 
# Complete Linear Regression equation: mpg = -0.153786(horsepower) -0.580520(acceleration) + 46.856115
#overall a decent model


mlr4 <- lm(mpg ~ ., data = mpg300)
summary(mlr4)
#Multiple R-squared:  0.9789
#Adjusted R-squared:  0.9022
#really just did this experimentally. I forgot to take into account that car name is a character. the P values are so high I would not use this. 


mlr5 <- lm(mpg ~ horsepower + acceleration + cylinder + displacement + weight + model.year + origin, data = mpg300)
summary(mlr5)
#Multiple R-squared:  0.823
#Adjusted R-squared:  0.8187
#Complete Linear Regression equation: mpg = (horsepower)-0.0172493 + (acceleration)-0.0278409 + (cylinder)-0.4562389 + (displacement)0.0101272 + (weight)-0.0053282 + (model.year)0.4439943  + (origin)0.9931335 + 5.8118427
#everything but car name, although it is possible car name has a significant correlation with mpg, that is beyond the scope of this report. 
# the R squares are good but the P values are not, we'll test this soon. 

library(MASS) # for stepwise selection

mlr6 <- stepAIC(mlr5, direction = "forward", scope = list(lower = ~1, upper = ~ .))
summary(mlr6)
#Multiple R-squared:  0.823
#Adjusted R-squared:  0.8187 
#very close to mlr5, barely worth noting. 


mlr7 <- stepAIC(mlr5, direction = "backward")
summary(mlr7)
#Multiple R-squared:  0.8212
#Adjusted R-squared:  0.8184
#Complete Linear Regression equation: mpg = (horsepower)0.015202 + (cylinder)-0.448111 + (displacement)0.010352 + (weight)-0.005406 + (model.year)0.444528 + (origin)0.996595 - 5.263056
#debatable better than mlr5 with similar issues. possibly an over fit though. 


#based on the models output, I chose these models to perform further predictions: 
linear2
mlr2
mlr5
mlr7

# In order to take the last 98 remaining samples:
nrow(mpg) #just to make sure there are 398
mpg98 <- tail(mpg, 98)
glimpse(mpg98) #to check over


################
# Making predictions on linear2 that predict mpg  based on model.year
compare_linear2 <- data.frame(
  model.year = mpg98$model.year,
  Original_mpg = mpg98$mpg,
  Predicted_mpg = predict(linear2, newdata = mpg98),
  Residuals = mpg98$mpg - predict(linear2, newdata = mpg98)
)
print(compare_linear2)
#from just glimpsing this new dataframe, I can see that my predicted values are lower than the real values.


#residual plot.
#residuals of predicted on the training data:
plot(fitted.values(linear2), residuals(linear2))
#residuals of predicted on testing data:
plot(compare_linear2$Predicted_mpg, compare_linear2$Residuals)
#I found there are a few methods to do this. I chose to put the predicted values on the x axis and the Residuals on the y axis.
#the plots shows a random scatter of points, which is desirable as it shows there is no trend in errors. 
#my one issue is that the predicted values range from 23 to 26. 


# Create a histogram comparing original and predicted values
ggplot(compare_linear2, aes(x = Original_mpg)) +
  geom_histogram(fill = "blue")+ 
  (ggplot(compare_linear2, aes(x = Predicted_mpg)) +
  geom_histogram(fill = "red"))

#putting these 2 side by side shows that the model is not good in predicting the next few values. the mpg in mpg98 ranges from 20 to 60, whereas my model is showing only from 23 to 26.
#I tried to see why this happened. I believe it is because the mpg goes up exponentially not linearly. 

#I dont think this is a very good model to use.  


#########################
# Making predictions on mlr2 that predicts mpg based on the horsepower an weight
compare_mlr2 <- data.frame(
  Original_mpg = mpg98$mpg,
  Predicted_mpg = predict(mlr2, newdata = mpg98),
  Residuals = mpg98$mpg - predict(mlr2, newdata = mpg98)
)
print(compare_mlr2)
#No striking observations yet

#residual plot:
#residuals of predicted on the training data:
plot(fitted.values(mlr2), residuals(mlr2))

#residuals of predicted on testing data:
plot(compare_mlr2$Predicted_mpg, compare_mlr2$Residuals)

#the plots shows a slight trend in the residuals on the training data, and less of a trend on the testing data. I cant make any conclusions on the model yet.


# Create a histogram comparing original and predicted values
ggplot(compare_mlr2, aes(x = Original_mpg)) +
  geom_histogram(fill = "blue")+ 
  (ggplot(compare_mlr2, aes(x = Predicted_mpg)) +
     geom_histogram(fill = "red"))
#The histograms show the model is far better than linear2, but still not perfect. This would be a good starting point in a model though. 



####################
# Making predictions on mlr5 that predict mpg  based on all values except car name
compare_mlr5 <- data.frame(
  Original_mpg = mpg98$mpg,
  Predicted_mpg = predict(mlr5, newdata = mpg98),
  Residuals = mpg98$mpg - predict(mlr5, newdata = mpg98)
)
print(compare_mlr5)
#No striking observations yet. some residuals are very high and others very low.

#residual plot:
#residuals of predicted on the training data:
plot(fitted.values(mlr5), residuals(mlr5))

#residuals of predicted on testing data:
plot(compare_mlr5$Predicted_mpg, compare_mlr5$Residuals)

#the plots shows a slight trend in the residuals on the training data, and no trend on the testing data. I cant make any conclusions on the model yet.


# Create a histogram comparing original and predicted values
ggplot(compare_mlr5, aes(x = Original_mpg)) +
  geom_histogram(fill = "blue")+ 
  (ggplot(compare_mlr5, aes(x = Predicted_mpg)) +
     geom_histogram(fill = "red"))
#the model predictions seem to undershoot here as well, but its not so bad. Seemingly a decent model


#########################
# Making predictions on mlr7 that took mlr5 and did a backward selection
compare_mlr7 <- data.frame(
  Original_mpg = mpg98$mpg,
  Predicted_mpg = predict(mlr7, newdata = mpg98),
  Residuals = mpg98$mpg - predict(mlr7, newdata = mpg98)
)
print(compare_mlr7)
#No striking observations yet

#residual plot:
#residuals of predicted on the training data:
plot(fitted.values(mlr7), residuals(mlr7))

#residuals of predicted on testing data:
plot(compare_mlr7$Predicted_mpg, compare_mlr7$Residuals)

#the first plot shows a slight trend in the residuals on the training data, and no trend on the testing data. I cant make any conclusions on the model yet.

# Create a histogram comparing original and predicted values
ggplot(compare_mlr7, aes(x = Original_mpg)) +
  geom_histogram(fill = "blue")+ 
  (ggplot(compare_mlr7, aes(x = Predicted_mpg)) +
     geom_histogram(fill = "red"))
#The histograms show the model is still a little far from perfect, but arguably better than mlr5. This would be a good starting point in a model. 

knitr::stitch('ITDS final assignment mpg.r')



