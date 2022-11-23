#lab5

#load "use.data"

load("C:\\Users\\jgrochowalski\\Google Drive\\NYU\\2017 Fall - Regression and Multivariate Stats\\shared\\Lab 5\\lab5 data.RData")


# In medical schools, students are required to take a test in which they meet with 
# patients.  The patients have known issues, and it's up to the student to identify, 
# diagnose, and treat the issue.  After the test, the patient fills out a questionnaire,
# and the student is scored based on the patient's responses.  The patient answers questions
# about the physical exam (i.e., did the student examine everything correctly, like 
# feeling a thyroid gland when someone complains of lethargy, etc.), and the patient answers
# questions about the student's history taking.  In history taking, the student is supposed
# to ask questions about the patient's history to help diagnose and treat the issue.  
# Finally, the patient is asked about the student's interpersonal skills (i.e., bedside manner). 
# The IPS questionnaire ask about whether the patient was comfortable, if the student physician
# was clear, if the patient understood the diagnosis and treatment, and if the student was
# easy to work with.  At the end of the test, the student has a score for IPS (bedside manner),
# PX (physical exam score) and HX (history taking).  All scores are between 0 and 1, where 1
# is a perfect score and 0 is total failure. This data is collected annually, and the data
# set has 7 years of data.


# We're interested in predicting ips (interpersonal skills scores) using history taking 
# and physical exam scores.  We're trying to predict bedside manner based on skills as a physician.


# First, look at the data.
# We're mostly concerned with the relationship between IPS (the IV) and the HX, PX (DVs)
plot(use.data)

# Then run the model.
mod<-lm(ips~px+hx,data=use.data)
summary(mod)

head(mod$residuals) # the mod object contains the computed residuals.  Residuals are important for
# regression diagnostics.
  
hist(mod$residuals) # residuals must have a mean of zero, and should be symmetrically distributed


# here we create an index plot of residuals.  One assumption is that residuals independent.
# If residuals are independent, then there should be no patterns in the residuals when 
# we plot them against the case collection order.  The x-axis here is the order the cases
# were collected, such as 1 is the first case, etc.  We can see that there are clusters;
# cases 200 to 300 seem to have higher residuals than other cases, and this is likely
# a high-scoring year.  This is a violation of the assumption of independence, and might
# mean that we need an additional variable to account for year.  The smoothed line tells
# us where the average is, the red line tells us where it should be (at 0).

scatter.smooth(1:length(mod$residuals),
               mod$residuals)
abline(h=0, col="red")

# Another way of looking at the same issue.  It makes it clear where there are clusters
# in the collection order.
smoothScatter(1:length(mod$residuals),
              mod$residuals)

# here's what a totally random scatter would look like:
smoothScatter(1:1400,rnorm(1400,mean=0,sd=1))

# next we can run diagnostics for our regression analsis to examine other assumptions.
plot(mod)

  
# the first plot is y-hat as x-axis versus residuals on Y-axis.  We can identify
  # whether we violate linearity with this.

  # second plot is q-q plot, which plots the residual's theoretical quantiles with the observed 
  # quantiles, based on the normal distribution.  This helps us determine if the residuals
  # are normal.

  # the third plot, scale-location, provides similar information to the first plot.  Use
  # this plot to inspect for homoscedasticity of variance.  The spread of the residuals should be
  # constant

  # The fourth plot, resid vs leverage, is for detecting outliers and influential points.  We can
  # ignore this for now, because we haven't covered it yet.
  

# as far as linearity goes, what does the lowess line look like?
scatter.smooth(use.data$px,use.data$ips)

# if we flip the axes, we can see that the relationship is "flat" and then "accelerates"
scatter.smooth(use.data$ips,use.data$px) 


# Look at cases 150:250.  This is just a way of inspecting the data to see if 
# there's something observable in the data.  in this case, we can see the values
# are greater, but there's nothing in here that tells us why.
use.data[c(170:350,420:500),]

# Probably another variable is affecting those cases,
# and it has to do with the order the data was collected/entered.
# Maybe a rater was very lenient for those cases?
# Maybe those cases/questions/scenarios were easier?
  
# Is the model fully specified?  One assumption of regression is that ALL relevant 
# variables are included in the model.  Here we add gender.
mod2<-lm(ips~px+hx+gender,data=use.data)
summary(mod2)

# gender seems to have a meaningful effect.  This means that our previous model
# was likely misspecified.  Did this fix our problem(s)?

plot(mod2)

# Can we fix the model?  Let's focus on the cluster.
mod3<-lm(ips~px+hx,data=use.data[-c(170:350,420:500),]) #this fits the model, minus that group
summary(mod3)
hist(mod3$residuals)
scatter.smooth(1:length(mod3$residuals),
               mod3$residuals) # better, I guess.
abline(h=0)
smoothScatter(1:length(mod3$residuals),mod3$residuals)  # I still see two dark clusters as well.

plot(mod3)

# The problem could be:

# In this REAL data set, we saw...
# not rectilinear
# not normal residuals
# questionable homogeneity of variance
# violation of independence of errors
# model misspecification

# ...which means it violated every assumption of regression.

#How to make CI for coefficients

# Load the "use.data" file from the Lab 5 folder and run the following model:
mod<-lm(ips~px+hx,data=use.data)

# We're interested in getting the 95% confidence intervals for the coefficients
# (which are intercept., slope for px, and slope for hx).  The 95% CI will be 
# the 2.5% and 97.5% extremes of the distribution of sampling values.

# To calculate the CI, we need the coefficient, the SE for the coefficient,
# and the t value that corresponds to 95% confidence (based on degrees of freedom)

# The calculation is [COEFFICIENT - T*SE] for the lower band, and [COEFFICIENT + T*SE]
# for the upper band, where T is the value of t corresponding to the model df.

# So let's calculate by hand:

# First, get the coefficient from the results.  We'll use the px from the
# model for our example:

coef.px <- mod$coefficients[2]

# We can get our df from the model:

our.df <- mod$df.residual

# Note that our df is not equal to nrow(use.data)-2.  This is because R automatically
# removes the missing data when doing the analysis. 

# Now look up the t value.  We can use the qt() function.

qt(.975, df=our.df)

# this is the t value we'll use to multiple in our equation.  We can
# be more efficient if we just ask for both t values at the same time.
# Both t values will be the same, just one will be positive and one 
# will be negative.  This makes it easier to create the CI later.

t <- qt(c(.025, .975), df=our.df)

# Get the standard error for the coefficient:
our.se <- coef(summary(mod))[2,2]

# and then calculate the interval:

CI <- coef.px + t*our.se


# look at it.
CI

# There is a much easier function for this:
confint(mod,level=0.95)








