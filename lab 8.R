
library(MASS)

#we have to make up some data
set.seed(1)
#create a covariance matrix
mysigma<-matrix(c(4,2,1,
                  2,4,2,
                  1,2,5),nrow=3)

#generate data
mydata<-mvrnorm(1000,mu=c(10,10,12),Sigma=mysigma)

#Create Y as a function of the other variables
Y=20+
  1*mydata[,1]+
  3*mydata[,2]+
  1*mydata[,1]*mydata[,2]+
  rnorm(nrow(mydata),0,sd=30)

#Create an interaction model
mod1<-lm(Y~mydata[,1]*mydata[,2])
summary(lm(Y~mydata[,1]*mydata[,2]))

# make it all into one data set
fulldata<-cbind(Y,mydata)

#plot the data
plot(fulldata[,2],fulldata[,1], pch="J")
plot(fulldata[,3],fulldata[,1])

#compare differences between interaction and no interaction
summary(lm(Y~mydata[,1]+mydata[,2]))
summary(lm(Y~mydata[,1]*mydata[,2]))

moda<-lm(Y~mydata[,1]+mydata[,2])
modb<-lm(Y~mydata[,1]*mydata[,2])

anova(modb,moda)

#generate a 3-way interaction
Y=10+
  1*mydata[,1]+
  2*mydata[,2]+
  3*mydata[,3]+
  1*mydata[,1]*mydata[,2]+
  2*mydata[,1]*mydata[,3]+
  3*mydata[,2]*mydata[,3]+
  4*mydata[,1]*mydata[,2]*mydata[,3]+
  rnorm(nrow(mydata),0,sd=200)

summary(lm(Y~mydata[,1]*mydata[,2]*mydata[,3]))

# notice that the three way interaction is significant, so include all relevant variables

# Now let's do some plotting and hypothesis testing

library(car)

#this package allows us to plot the different slopes
install.packages("interplot")

library(interplot)

#let's use the mtcars data set.
#wt = weight
#cyl = number of cylinders
#mpg = miles per galon

data(mtcars)
head(mtcars)

mod1 <- lm(mpg ~ wt, data = mtcars)
summary(mod1)

mod2 <- lm(mpg ~ wt + cyl, data = mtcars)
summary(mod2)
plot(mod2) #nonlinearity!


anova(mod2,mod1)


plot(mtcars[,c(1,2,6)])

mod3<-lm(mpg ~ wt + cyl + wt*cyl, data=mtcars)
summary(mod3)
plot(mod3) #nonlinearity corrected

anova(mod3,mod2)

mod3<-lm(mpg~wt*cyl,data=mtcars) #just another way of writing this.
summary(mod3)

#inspect the actual versus predicted.
plot(mtcars$mpg,predict(mod2,data=mtcars))
plot(mtcars$mpg,predict(mod3,data=mtcars))

#look at the slopes of cyl for each value of wt:
interplot(m = mod3, var1 = "cyl", var2 = "wt")

#look at the slopes of wt for each value of cyl:
interplot(m = mod3, var1 = "wt", var2 = "cyl")

#we can also make the first plot discrete if we want to:
interplot(m = mod3, var1 = "cyl", var2 = "wt", point = T) +
  theme(axis.text.x  = element_text(angle=90))


#now do it with centering

#just use the important variables, give dataset a new name
mycars<-mtcars[,c(1,2,6)]

names(mycars)

#center the variables
mycars$cyl <- mycars$cyl - mean(mycars$cyl)
mycars$wt <- mycars$wt - mean(mycars$wt)

#run the interaction model
mod4<-lm(mpg~wt*cyl,data=mycars)
summary(mod4)

#plot the results

interplot(m = mod4, var1 = "cyl", var2 = "wt")
summary(mod4)

#for a car of average weight (0), cyl has a negative impact

interplot(m = mod4, var1 = "cyl", var2 = "wt", point = T) +
  theme(axis.text.x  = element_text(angle=90))

# Let's calculate the simple slopes from model 3

# What are the simple slopes for model 3?
summary(mod3)

# Simple slope for weight when car has 4 cylinders?
-8.6556*(1)+0.8084*(4*1)
# Which is the same as...
mod3$coefficients[2]+mod3$coefficients[4]*4

# Simple slope for weight when car has 6 cylinders?
-8.6556*(1)+0.8084*(6*1)

# Simple slope for weight when car has 8 cylinders?
-8.6556*(1)+0.8084*(8*1)

# Simple slope for cylinders when car weighs 3?
-3.8032*(1)+0.8084*(1*3)

# To do the hypothesis testing
covs<-vcov(mod3)

# SE of B for weight when car has 4 cylinders
# Page 278 of Cohen et al
SEB.Z<-sqrt(covs[2,2]+2*4*covs[2,4]+4^2*covs[4,4])

# So slope is...
SLP.Z=-8.6556*(1)+0.8084*(4*1)

# And SEB.Z is...
SEB.Z

# And t test is slope/SE, so...
t<-SLP.Z/SEB.Z
t

# We can check this against (n-k-1) degrees of freedom (32-3-1)

crit.t<-qt(c(0.025,0.975), 28)


# And calculate a confidence interval:
our.CI<-SLP.Z+crit.t*SEB.Z


#Compare this result against this graph:

interplot(m = mod3, var1 = "wt", var2 = "cyl")






