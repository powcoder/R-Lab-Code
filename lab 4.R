# open cigarette data

# The first thing we want to do is look at the data, including scatterplots
# and histograms.
library(car)
scatterplotMatrix(mydata, diagnoal="histogram",  
                  main="Simple Scatterplot Matrix",
                  smooth=FALSE)

pairs(~sales+income+price,data=mydata, 
      main="Simple Scatterplot Matrix")

# This is the famous "smoking data."  We're interested in predicting the sales of cigarettes,
# based on a number of demographic variables.  

# Specifically, we want to know if sales can be predicted by income and price.
# However, for this exercise, we'll do the multiple regression in pieces rather than simultaneously.

# First, predict sales based on income.
mod1<-lm(mydata$sales~mydata$income)
summary(mod1)

# Calculate the predictions for sales, based solely on income.
pre1<-mod1$coefficients[1]+mod1$coefficients[2]*mydata$income
head(cbind(pre1,mydata$sales))

# Calculate the residuals for the predicted sales (based only on income).
res1<-mydata$sales - pre1

# Income should be uncorrelated with the residuals.
cor(mydata$income,res1)

# Now predict PRICE based on income.
mod2<-lm(mydata$price~mydata$income)
summary(mod2)

# Calculate the predicted values...
pre2<-mod2$coefficients[1]+mod2$coefficients[2]*mydata$income

# Calculate what's not explained in the price by income (i.e., residuals),
res2<-mydata$price - pre2

# Income should be uncorrelated with the residuals from model 2.
cor(mydata$income, res2)


# look at the residuals, while we're at it.
hist(res1,breaks=10)
hist(res2,breaks=10)

plot(1:length(res1),res1)
plot(1:length(res2),res2)

# Now predict the first set of residuals from the second set.  It's helpful
# to look at a diagram to see what's happening here.  We're predicting the
# part of sales that's not explained by income, but is explained by price.
mod3<-lm(res1~res2)

# Look at the slope from model 3.
mod3$coefficients[2]

# Now run the full model.  
mod4<-lm(mydata$sales~mydata$income+mydata$price)
mod4

# Our coefficient from model 3 matches our partial coefficient from model 4.







