

#load the lab7 data
load("C:/Users/jgrochowalski/Google Drive/NYU/2017 Fall - Regression and Multivariate Stats/shared/Lab 7/lab7.RData")

#set a seed for simulating curves
set.seed(1)

#How many cases do we want? "my N"
myn<-100

#Load MASS
library(MASS)

#Generate X as normal with mean 10 and sd 1.5
X<-rnorm(myn,mean=10,sd=1.5)

#Generate Y as a function of X; as X^5.  Add some error
Y<-X^5+rnorm(myn,mean=0,sd=1)


#View
plot(X,Y)
scatter.smooth(Y ~ X)

#Fit a simple linear model
mod1<-lm(Y~X)
summary(mod1)
plot(mod1)
plot(X,Y)

abline(mod1)

#Increase X
mod2 <- lm(Y~I(X^2))
summary(mod2)
plot(X^2,Y)

#Increase X more!
mod3 <- lm(Y~I(X^3))
summary(mod3)
plot(X^3,Y)

#MORE!
mod4 <- lm(Y~I(X^5))
summary(mod4)
plot(X^5,Y)

# Another curve
X<-rnorm(myn,mean=4,sd=1)
Y<-exp(1/X)+rnorm(myn,mean=0,sd=.05) # increase the value of the SD to see the curve disappear
plot(X,Y)

# Decrease X
plot(sqrt(X),Y)
plot(log(X),Y)
plot(1/X,Y)
plot(exp(1/X),Y) #this is obviously best


# Real data
scatter.smooth(A ~ B)
mod4<-lm(A~B)
plot(B,A)
abline(mod4)
plot(mod4)

mod5<-lm(A~B+I(B^2))
summary(mod5)
plot(B,A)
curve(mod5$coefficients[1] + mod5$coefficients[2]*x + 
        mod5$coefficients[3]*x^2, add=T, col="red")
plot(mod5)

mod6<-lm(A~B+I(B^2)+I(B^3))
summary(mod6)
plot(B,A)
curve(mod6$coefficients[1] + 
        mod6$coefficients[2]*x + 
        mod6$coefficients[3]*x^2 +
        mod6$coefficients[4]*x^3, add=T, col="red")
plot(mod6)

anova(mod5,mod6)


mod7<-lm(A~B+I(B^2)+I(B^3)+I(B^4))
summary(mod7)
plot(B,A)
curve(mod7$coefficients[1] + mod7$coefficients[2]*x + 
        mod7$coefficients[3]*x^2 +mod7$coefficients[4]*x^3 +
        mod7$coefficients[5]*x^4, add=T, col="red")
plot(mod7)

anova(mod6,mod7)

#Trees and forests
library(partykit)

load("C:\\Users\\jgrochowalski\\Google Drive\\NYU\\2017 Fall - Regression and Multivariate Stats\\shared\\Lab 3\\miscar+_2.RData")

mydata<-as.data.frame(mydata)
mydata<-na.omit(mydata)


ct1 = ctree(as.factor(Marital)~Age+as.factor(RaceCode)+
              as.factor(PubAsst)+as.factor(EdCode)+
              CESD1, data = mydata)
plot(ct1, main="Conditional Inference Tree")

ct2 = ctree(CESD2~Age+as.factor(RaceCode)+
              as.factor(Marital)+as.factor(PubAsst)+
              as.factor(EdCode)+CESD1, data = mydata)
plot(ct2, main="Conditional Inference Tree")

ct2b = ctree(CESD1~Age+as.factor(RaceCode)+
              as.factor(Marital)+as.factor(PubAsst)+
              as.factor(EdCode), data = mydata)
plot(ct2b, main="Conditional Inference Tree")

ct2c = ctree(CESD2~CESD1+as.factor(PubAsst), data = mydata)
plot(ct2c, main="Conditional Inference Tree")



mycontrol = ctree_control(mincriterion = .5)

ct3 = ctree(CESD2~Age+as.factor(RaceCode)+
              as.factor(Marital)+
              as.factor(PubAsst)+
              as.factor(EdCode)+CESD1, 
            data = mydata, control=mycontrol)
plot(ct3, main="Conditional Inference Tree")

ct4 = cforest(CESD2~Age+
                as.factor(RaceCode)+
                as.factor(Marital)+
                as.factor(PubAsst)+
                as.factor(EdCode)+
                CESD1, data=mydata, ntree=1000)
plot(ct4, main="Conditional Inference Tree") # Won't plot!

yhat.ct4 <- predict(ct4, newdata = mydata[1:5,], 
                    type = "response")
yhat.ct4 #prediction of CESD2 for case 1
mydata$CESD2[1:5] #actual values

error<-sum((mydata$CESD2[1:5]-yhat.ct4)^2)/
  length(mydata$CESD2)

sqrt(error) #SEE


# now make all predictions

yhat.ct5 <- predict(ct4, newdata = mydata, 
                    type = "response")

#calculate standard error
(error<-sqrt(sum((mydata$CESD2-yhat.ct5)^2)/
  length(mydata$CESD2)))
         
# Compare to standard error of OLS
summary(lm(CESD2~Age+
             as.factor(RaceCode)+
             as.factor(Marital)+
             as.factor(PubAsst)+
             as.factor(EdCode)+
             CESD1, data=mydata))





















