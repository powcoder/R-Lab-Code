set.seed(111)

#moderation, continuous variables
load("C:/Users/jgrochowalski/Google Drive/NYU/2019 Fall - Regression and Multivariate Stats/shared/Lab 9/lab9.RData")

# Lookit the data
head(mod1.data)

# Research question: Is the number of books read
# for class a constant predictor based on attendance?
# Or does attendance moderate the effect/impact of 
# reading books?

# Non-interaction
mod1.1<-lm(GRADE~BOOKS+ATTEND,data=mod1.data)

# Interaction
mod1.2<-lm(GRADE~BOOKS*ATTEND,data=mod1.data)
summary(mod1.2)

# Centering, if necessary
#mod1.data$BOOKSC<-scale(mod1.data$BOOKS,center=T,scale=F)
mod1.data$ATTENDC<-scale(mod1.data$ATTEND,center=T,scale=F)

# re-run with centered data
mod1.2<-lm(GRADE~BOOKS*ATTENDC,data=mod1.data)
summary(mod1.2)

# Which model is better?  Should we include the interaction?
anova(mod1.2,mod1.1)

#moderation, one continuous (EVENTS) one categorical (STATUS)
# Scenario: Level of stress is predicted by number of
# tragic life events; does marriage status change how
# events affect stress?

# This data has a categorical predictor
head(mod2.data)

mod2.1<-lm(STRESS~EVENTS+STATUS,data=mod2.data)

mod2.2<-lm(STRESS~EVENTS*STATUS,data=mod2.data)

anova(mod2.2,mod2.1)

#mediation

head(mod3.data)
# Y = READ Books
# X = ENJOY Content
# Z = BUY (mediator) Books


# Calculate path C (this is the direct relationship between X and Y)
mod3.1<-lm(READ~ENJOY, data=mod3.data)
summary(mod3.1)

# Calculate path a, the direct path between X and Z
mod3.2<-lm(BUY~ENJOY,data=mod3.data)
summary(mod3.2)

# Calculate the direct path b, the relationship between Z and Y
mod3.3<-lm(READ~BUY,data=mod3.data)
summary(mod3.3)

# Calculate the prediction of Y using X and Z
mod3.4<-lm(READ~BUY+ENJOY,data=mod3.data)
summary(mod3.4)

# z-value = a*b/SQRT(b2*sa2 + a2*sb2)

# Pull the coefficients from the models above that estimate a and b, and name them a and b
a<-mod3.2$coefficients[2]
b<-mod3.3$coefficients[2]

#calculate the indirect path coefficient
ab<-a*b

# Pull the squared standard errors from the variance/covariance matrices
sa2<-vcov(mod3.2)[2,2]
sb2<-vcov(mod3.3)[2,2]

# Calculate the sobel Z test for significance of inderect effect
sobel.z<-(a*b)/sqrt(b^2*sa2 + a^2*sb2)

# Compare the above value to +/- 1.96, the Z value corresponding to a 95% CI 

#extent of mediation:

confint(mod3.1) #original c
confint(mod3.4) #new c


#Bootstrapping

# set up an empty variable to hold all of the bootstrap values
myab<-vector()

# Do the bootstrap procedure 1000 times
for(i in 1:10000){
  
  rand<-sample(1:nrow(mod3.data),
               nrow(mod3.data),
               replace=T)
  modbs.data<-mod3.data[rand,]
  
  modbs.2<-lm(BUY~ENJOY,data=modbs.data)
  
  modbs.3<-lm(READ~BUY,data=modbs.data)
  
  bsa<-modbs.2$coefficients[2]
  bsb<-modbs.3$coefficients[2]
  
  bsab<-bsa*bsb
  
  myab<-c(myab,bsab)
}


# View a histogram of the bootstrapped indirect effects
hist(myab,breaks=60)

#Create confidence interval:

myabsorted<-sort(myab)

myabsorted[round(.025*length(myab))]
myabsorted[round(.975*length(myab))]


#Compare the bootstrap interval above to the theoretical interval based on Sobel SE:
SE.sobel<-sqrt(b^2*sa2 + a^2*sb2)
ab-1.96*SE.sobel
ab+1.96*SE.sobel

