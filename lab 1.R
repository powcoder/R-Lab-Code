
# create a variable named 'one', assign it the value of 1
one<-1

# see the contents of the variable
one

# do the same for 'two' and 2
two<-2

# Operations work as you expect them to.
# We can either add...

1+2

# or sum the two variables...
one+two

# or we can create a third variable using an operation
three<-one+two

# see the contents of the variable
three

# create a vector of data
moredata<-c(1,2,3,5,7,9)

# view the vector
moredata


# import data

# instal package
install.packages("xlsx")

# call the library
library(xlsx)

# look at the function
?read.xlsx

# read in the data, name it "mydata"
mydata = read.xlsx("C:\\Users\\jgrochowalski\\Google Drive\\NYU\\2017 Fall - Regression and Multivariate Stats\\shared\\Lab 1\\Sample_Dataset_2014.xlsx",
                   sheetName="Sample_Dataset_2014")

# look at the first six rows of data
head(mydata)


# look at the height variable
mydata$Height

height<-mydata$Height

# height happens to be a character variable.  let's make it numeric.
height<-as.numeric(as.character(height))

# remove missing data.  Missing data is usually coded "NA"
height<-na.omit(height)

# look at a histogram of height.  And various other descriptives.
hist(height)
hist(height, breaks=30)
mean(height)
var(height)
heightvar<-var(height)
heightsd<-sqrt(var(height))
sd(height)

# Run a one-sample t-test.
# The t.test function requires two arguments:
# the data ("height"), and the hypothesized 
# population mean ("mu")
t.test(height, mu=66.5)

# Analyze running data
mysprint <-as.numeric(as.character(mydata$Sprint))
# Remove missing
mysprint<-na.omit(mysprint)

# Make a histogram
hist(mysprint)
# Add more breaks to the histogram
hist(mysprint, breaks=30)

# Do another t-test, save it as an object
mytest2<-t.test(mysprint,mu=6.68)
mytest2

# Do the same analysis with weight
weight<-as.numeric(as.character(mydata$Weight))
weight<-na.omit(weight)

# Indexing
small.height<-height[1:376]

# Regression
myreg<-lm(weight~small.height)







