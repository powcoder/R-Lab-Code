#lab6

###################################
load("C:/Users/jgrochowalski/Google Drive/NYU/2019 Fall - Regression and Multivariate Stats/shared/Lab 6/lab5 data.RData")

# Let's build the best model.
# First, use the intercept only model.
mod0<-lm(ips~1,data=use.data)
summary(mod0)

# Because there are no predictors, the intercept is the same as
# the mean.
mean(use.data$ips,na.rm=T)

# Model 1 has the predictor "history" (hx)
mod1<-lm(ips~hx,data=use.data)
summary(mod1)

# Model 2 has the predictor "physical exam" (px)
mod2<-lm(ips~px,data=use.data)
summary(mod2)

# model 3 has both physical exam and history taking
mod3<-lm(ips~px+hx,data=use.data)
summary(mod3)


anova(mod0,mod1) # Intercept only versus model 1 (check model 1 F value--this result should be the same)
anova(mod0,mod2) # Intercept only versus model 2 (check model 2 F value--this result should be the same)
anova(mod1,mod3) # Does adding physical AFTER entering history make a significant contribution to prediction?
anova(mod2,mod3)
anova(mod0,mod3) # Does adding BOTH physical AND history improve over the intercept alone?

######################

# Bootstrapping

######################

# Take numbers randomly from a set of numbers the size of our sample N
rand.samp<-sample(1:nrow(use.data),nrow(use.data),replace=T)

# Create a sample using the random numbers above
bs.use.data<-use.data[rand.samp,]

# Run a regression using the sample
mod.bs<-lm(ips~px+hx,data=bs.use.data)

# Compare the bootstrap sample to the actual sample
summary(mod.bs)
summary(mod3)

# Save the coefficients from the BS sample
bs.coef.ic<-mod.bs$coefficients[1]
bs.coef.ic

bs.coef.px<-mod.bs$coefficients[2]

bs.coef.hx<-mod.bs$coefficients[3]

#do it again!

rand.samp<-sample(1:nrow(use.data),nrow(use.data),replace=T)
bs.use.data<-use.data[rand.samp,]

mod.bs<-lm(ips~px+hx,data=bs.use.data)

bs.coef.ic<-c(bs.coef.ic, mod.bs$coefficients[1])

bs.coef.px<-c(bs.coef.px, mod.bs$coefficients[2])

bs.coef.hx<-c(bs.coef.hx, mod.bs$coefficients[3])

# Now look.  We have two coefficients, each from the two samples. 

bs.coef.ic
bs.coef.hx
bs.coef.px

# Now do it 100 times!  We can create a distribution of all
# of the coefficients we get from randomly sampling from our sample

for(i in 1:100){
  
  rand.samp<-sample(1:nrow(use.data),nrow(use.data),replace=T)
  bs.use.data<-use.data[rand.samp,]
  
  mod.bs<-lm(ips~px+hx,data=bs.use.data)
  
  bs.coef.ic<-c(bs.coef.ic, mod.bs$coefficients[1])
  
  bs.coef.px<-c(bs.coef.px, mod.bs$coefficients[2])
  
  bs.coef.hx<-c(bs.coef.hx, mod.bs$coefficients[3])
  
}

# Look at the distribution of each of the coefficients
hist(bs.coef.ic,breaks=20)
hist(bs.coef.px,breaks=20)
hist(bs.coef.hx,breaks=20)

# Get the mean and SD of the coefficients
mean(bs.coef.ic)
sd(bs.coef.ic)

mean(bs.coef.px)
sd(bs.coef.px)

mean(bs.coef.hx)
sd(bs.coef.hx)

# Compare to our classical estimate
summary(mod3)

# Now 1,000!!!

# First, clear the containers we have
bs.coef.ic<-NULL
bs.coef.px<-NULL
bs.coef.hx<-NULL

#Then iterate through the same process 1000 times instead of 100

for(i in 1:1000){
  
  rand.samp<-sample(1:nrow(use.data),nrow(use.data),replace=T)
  bs.use.data<-use.data[rand.samp,]
  
  mod.bs<-lm(ips~px+hx,data=bs.use.data)
  
  bs.coef.ic<-c(bs.coef.ic, mod.bs$coefficients[1])
  
  bs.coef.px<-c(bs.coef.px, mod.bs$coefficients[2])
  
  bs.coef.hx<-c(bs.coef.hx, mod.bs$coefficients[3])
  
}

hist(bs.coef.ic,breaks=20)
hist(bs.coef.px,breaks=20)
hist(bs.coef.hx,breaks=20)

mean(bs.coef.ic)
sd(bs.coef.ic)

mean(bs.coef.px)
sd(bs.coef.px)

mean(bs.coef.hx)
sd(bs.coef.hx)

# Compare to our classical estimate
summary(mod3)

# Sort the coefficients in order of magnitude
sorted.bs.coef.px<-sort(bs.coef.px)
head(sorted.bs.coef.px)
tail(sorted.bs.coef.px)

# How many observations do we have?
length(sorted.bs.coef.px)

# Take the 25th observation, which corresponds to the lower value in a 95% CI
sorted.bs.coef.px[.025*length(sorted.bs.coef.px)]

# Take the 975th observation, which corresponds to the upper value in a 95% CI
sorted.bs.coef.px[975]

# Compare the bootstrap CI to the classical CI
confint(mod3)

hist(bs.coef.px,breaks=20)

############
# R^2 Change
############

# We can follow the same process for the R^2 change

dif.r2<-NULL

for(i in 1:1000){
  
  # Take our random sample as before
  rand.samp<-sample(1:nrow(use.data),nrow(use.data),replace=T)
  bs.use.data<-use.data[rand.samp,]
  
  # Calculate the two models (the full and reduced/restricted)
  mod.bs.1<-lm(ips~hx,data=bs.use.data)
  mod.bs.2<-lm(ips~hx+px,data=bs.use.data)  
  
  # Calculate the difference between the R^2 values
  dif.r2<-c(dif.r2,summary(mod.bs.2)$r.squared-summary(mod.bs.1)$r.squared)
  
}

# Look at the classlical R^2 difference:
summary(mod3)$r.squared-summary(mod1)$r.squared

# Was it sig?  Can't remember:
anova(mod3,mod1) #nope

# Now create a bootstrap CI
sorted.dif.r2<-sort(dif.r2)
sorted.dif.r2[25]
sorted.dif.r2[975]

hist(sorted.dif.r2)

# The empirical confidence interval contains 0.  The BS result is also not sig.

# Bayesian:

#install.packages("MCMCpack")

library(MCMCpack)
breg <- MCMCregress(ips~px+hx,data=use.data)
summary(breg)
plot(breg)

# Now compare to classical:

confint(mod3)
