#Cohen's D and our author are the same.

# We're going to generate some random data.  If we set a seed, 
# the data generated will always be the same as long as we use
# the same seed.

set.seed(1)

# We're going to generate data for two variables.  We'll use them
# to run a regression analysis.

# To generate the data, we'll need to determine what the means
# are of our two variables.

means<-c(100,30)

# We also need to generate variances and a covariances.  Each
# variable will have a variance, and each pair of variables 
# will have one covariance.  We only have one pair, so we have
# one covariance.  We can put these values into a matrix:

# Variance1  Covariance
# Covariance Variance2

# We create a matrix with our variances and covariance:

varns<-matrix(c(15,4,
                4,4),
              nrow=2)

# We'll rely on a handy function from the MASS package
# to generate the data.  We put in the means, variances,
# and covariances, and it draws data randomly from the
# multivariate normal distribtuion.  We'll create 100 cases.

library(MASS)

mydata<-mvrnorm(100,
        means,
        varns)


#Now we can analyze the data.
hist(mydata[,1],breaks=20)
hist(mydata[,2],breaks=20)

plot(mydata[,1],mydata[,2],main="Title",
     xlab="X axis",
     ylab="Y axis")

# Run the regression predicting the second variable
# from the first variable.

mymodel<-lm(mydata[,2]~mydata[,1])


# The summary function often gives more useful details
# about the analysis.  This is where all of our hypothesis
# testing is hidden.

summary(mymodel)

# We can create a plot of our data and add a regression line
# using the abline() function.

# To create the regression line, we need the coefficients.
# We can get them by...

attributes(mymodel) # Allows us to see what variable holds the coeffecients
mymodel$coefficients # shows us the coefficients
mymodel$coefficients[1] # we can get the coefficients by index

# So then we create a new plot and add the line:

plot(mydata[,1],mydata[,2])
abline(mymodel$coefficients[1],mymodel$coefficients[2])

# Our intercept doesn't look right.  Why not?

# We can fix this by forcing the axes to have exact ranges:

plot(mydata[,1],mydata[,2],ylim=c(0,50),xlim=c(0,120))
abline(mymodel)

# Getting the correlation is easy.

mycor<-cor(mydata[,1],mydata[,2])
mycor
mycor^2  # This is the R^2, or proportion of variance in Y explained by X

# Y = (intercept) + (slope)X
# slope = r*(sdy/sdx)
# inter = mean(y) + slope(mean(x))

# We can calculate predictions using our formula.  See lecture slides for formulas.

slope = mycor*(sd(mydata[,2])/sd(mydata[,1]))
slope

intercept = mean(mydata[,2])-(slope*(mean(mydata[,1])))
intercept


# This is our set of predictions:
y.hat<-intercept+slope*mydata[,1]

# We can calculate the residuals:
resid<-mydata[,2]-y.hat

# And this is the standard error for our estimates
SEE<-sqrt(sum(resid^2)/(length(resid)-2))

# Standard error for the slope
SES<-(sd(mydata[,2])/sd(mydata[,1]))*sqrt((1-mycor^2)/(length(resid)-2))

# Standard error for the intercept
SEI<-SEE*(sqrt((1/length(resid))+(mean(mydata[,1])^2/((length(resid)-1)*var(mydata[,1])))))

# Now let's do something interesting.  Let's force a restriction of range problem.  To
# restrict the range, we need to sort the data by one variable, and then "chop" all of 
# values above a certain level.  

# First, sort the data:
mydata.sorted<-mydata[order(mydata[,1]),]

# After the data is sorted, take only the first twenty rows.
mydata.restrict<-mydata.sorted[1:20,]

# Run the regression on the reduced data
mymodel2<-lm(mydata.restrict[,2]~mydata.restrict[,1])

# Now let's compare the slopes.  Remember, restriction of range problem
# says that the slope (and/or intercept) can change as a result of the
# truncation, which is why we always want to use a full range of data.

# We can test this by comparing the two slopes from the full data and
# the truncated data.  We just calculate a two-way one-sample t-test:

#are the slopes the same?
ourt<-(mymodel2$coefficients[2]-mymodel$coefficients[2])/sqrt(vcov(mymodel2)[1,1])

ourt

# Then use this convenient function to see if our observed t ("ourt") is
# outside of the 95% confidence interval:


# power analysis
install.packages("pwr")
library(pwr)

# For correlation
pwr.r.test(n = 120, 
           r = NULL, 
           sig.level = .05, 
           power = .8)

# For linear models
pwr.f2.test(u = 1, # numerator DF
            v = 98, # denom df
            f2 = .15, # effect size F2=R^2/(1-R^2)
            sig.level = .001 , 
            power = NULL)



# To import homework data:

install.packages("xlsx")
library("xlsx")
mydata<-read.xlsx(file="Path\\with\\two\\slashes.xls", sheetName="Sheet1")

mydata<-read.xlsx(file="C:\\Users\\jgrochowalski\\Google Drive\\NYU\\2017 Fall - Regression and Multivariate Stats\\shared\\Lab 3\\Miscar+_2.xlsx",
                  sheetName="Sheet1", colClasses = rep("numeric",8))

# OR

load("C:\\Users\\jgrochowalski\\Google Drive\\NYU\\2018 Fall - Regression and Multivariate Stats\\shared\\Lab 3\\miscar+_2.RData")




####################
# Clustering
####################

# We like to use the "Iris" data for clustering examples.

library(datasets)

# Look at the iris data
head(iris)

# Plot the iris data
plot(iris)


# If we don't want to constantly use the data$var naming convention,
# we can "attach" the data and then just refer to the variables
# by name.

attach(iris)

# Now plot just the two we're interested in.
# See any clusters?
plot(Petal.Length,Petal.Width)

# We secretly know the group/cluster assignments.
unique(Species)

# Let's do a k-means cluster analysis of the data.
set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster

# Plot the kmeans clustering result
plot(Petal.Length,Petal.Width, 
     pch=irisCluster$cluster)

# Check the kmeans accuracy as a table.  All off-diagonals should be zero
table(irisCluster$cluster,
      iris$Species)

# Plot the true classifications to compare with the k-means
plot(Petal.Length,Petal.Width, 
     pch=as.numeric(Species))


# Now let's try it with a hierarchical clustering method
clusters <- hclust(dist(iris[, 3:4]))  # the dist is a euclidean distance matrix
plot(clusters)

clusterCut <- cutree(clusters, 3) #3 is the desired number of groups
table(clusterCut, iris$Species)

# compare the true plot (first) to the hierarchical cut (second)
plot(Petal.Length,Petal.Width, pch=as.numeric(Species))
plot(Petal.Length,Petal.Width, pch=clusterCut)


# There are a few ways to determine how close two clusters are:
#   
# Complete linkage clustering: Find the maximum possible distance between points belonging to two different clusters.
# Single linkage clustering: Find the minimum possible distance between points belonging to two different clusters.
# Mean linkage clustering: Find all possible pairwise distances for points belonging to two different clusters and then calculate the average.
# Centroid linkage clustering: Find the centroid of each cluster and calculate the distance between centroids of two clusters.

clusters <- hclust(dist(iris[, 3:4]), 
                   method = 'average')
plot(clusters)

clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)

#Now compare
plot(Petal.Length,Petal.Width, pch=as.numeric(Species))
plot(Petal.Length,Petal.Width, pch=clusterCut)






