
# Packages are R programs written by regular users like you and I.  They typically
# calculate specialized statistics that aren't available in programs like SAS or SPSS.
# Be careful though, because that also means there's no quality control!

# The psych package is a very popular package for calculating statistics needed
# for quantitative analysis in psychology.  Sometimes packages contain practice
# data sets, and the psych package has a useful data set for us to use:  bfi

# To access the psych package, first we need to install the psych package:

install.packages("psych")

# Once it's installed, you "open" the package by calling it from the library.
# The functions and datasets from the package won't be available until you 
# use the library call:

library(psych)

# We're going to use the bfi data set.  Once the psych package is loaded,
# all we do is refer to the data set by its name: bfi.

# Let's peak at the top six rows of the data set:

head(bfi)

# How many cases are in the data?  We can get this by using the number of rows function:

nrow(bfi)

# How many columns?

ncol(bfi)

# We can get both the rows and the columns using the dimension function:

dim(bfi)

# The bfi data is a personality questionnaire with 25 items based on the five-factor
# theory of personality.

# We don't want all of the variables.  For example, we don't care about the demographic 
# variables.  So we can create a new dataset that has only the variables of interest.

# We can do this many ways:

OCEAN<-bfi[,1:25]

OCEAN<-bfi[,-c(26,27,28)]

OCEAN<-bfi[,c("A1","A2","A3","A4","A5",
              "C1","C2","C3","C4","C5",
              "E1","E2","E3","E4","E5",
              "N1","N2","N3","N4","N5",
              "O1","O2","O3","O4","O5")]

OCEAN<-cbind(bfi$A1,bfi$A2, bfi$A3, bfi$A4, bfi$A5,
             bfi$C1,bfi$C2, bfi$C3, bfi$C4, bfi$C5,
             bfi$E1,bfi$E2, bfi$E3, bfi$E4, bfi$E5,
             bfi$N1,bfi$N2, bfi$N3, bfi$N4, bfi$N5,
             bfi$O1,bfi$O2, bfi$O3, bfi$O4, bfi$O5)

# Note, however, if we use the cbind() "column bind" function, we lose the column names.
# We can put the column names back using the colnames() funciton:

colnames(OCEAN)<-c("A1","A2","A3","A4","A5",
                   "C1","C2","C3","C4","C5",
                   "E1","E2","E3","E4","E5",
                   "N1","N2","N3","N4","N5",
                   "O1","O2","O3","O4","O5")

# The psych package has some useful funcitons, like "calculate Cronbach's Alpha":

alpha(OCEAN)

# Only calc alpha for the first construct "A":
alpha(OCEAN[,c(1:5)],check.keys = TRUE)

myalpha<-alpha(OCEAN)

attributes(myalpha)

# This would be a lot of work by hand.  And certainly no other program (SPSS, SAS, etc.) will do this for
# you with such highly customized informaiton.

# We can create our own functions!  This makes life a lot easier if we have to do many analyses,
# or if we have to repeat the same analysis over and over again, etc.  Having a function reduces
# computation time, and we can use it to format output and so on.

# First, let's remove the NA (missing) rows from the data.  This function, na.omit() removes 
# any row (person) that has at least one NA in their record.  This is called case-wise deletion.

OCEAN.clean<-na.omit(OCEAN)

# How many cases did we lose?

dim(OCEAN.clean)

# Now let's write a function that calculates the mean, sd, max, and min values for a variable.

# First, we'll look at the basic structure of a function.

basicfunc1<-function(){ # we name the function, and then tell R that we're creating a function
  print("Hello world!") # this is what the function should do.
}

# After we teach R the function, we have to call it:
basicfunc1()

# We can also put input into our function:
basicfunc2<-function(anumber){ # Now we put an argument in the funciton() function.  R will use it inside:
  newnumber<-anumber+1
  print(newnumber)
}

basicfunc2(54)

# So, using this information, let's create our function that calculates basic descriptives:

sumfunc<-function(thedata){
  
  output.mean <-mean(thedata)
  output.sd   <-sd(thedata)
  output.max  <-max(thedata)
  output.min  <-min(thedata)
  
  all.output<-c(output.mean,   # the c() (combine) function takes several pieces of information and
                output.sd,     # turns them into one "string" or vector of information.  Functions
                output.max,    # like to return only one "thing".
                output.min)
  
  # print("Calculations complete!")
  
  print(all.output)
  
}

# Let's shove a variable in there and see what it does!

sumfunc(OCEAN.clean[,1]) # Recall that the first value in the straight brackets is the row, 
                         # the second value is the column.  When one of the values is blank,
                         # it returns all values.  In this case, all rows will be used,
                         # but only the first column.

# We can't put all of the variables in, though:

sumfunc(OCEAN.clean[,1:25])

# It returns the overall mean and the overal sd for all 25 variables.

# If we want to see this for all variables, we have to loop.

# First, a practice loop:

for(j in 1:10){ # this is a for loop.  "For" each of the items in 1:10 (1,2,3,...,9,10), the 
  print(j)      # loop will do whatever's in the curly brackets.
}

# So we can shove our function within a loop:

for(i in 1:ncol(OCEAN.clean)){  # the indexing is now dynamic.
  sumfunc(OCEAN.clean[,i])      # For each iteration, the i index will advance to the next column of the data.
}



# But we can't tell which line is which variable.

for(i in 1:ncol(OCEAN.clean)){
  print(colnames(OCEAN.clean)[i])  # Note that colnames is a vector, so we don't put a comma in the straight brackets.
  sumfunc(OCEAN.clean[,i])
  
  #print(paste(colnames(OCEAN.clean)[i], sumfunc(OCEAN.clean[,i])))
  
}

colSums(OCEAN.clean[,1:5])
rowSums(OCEAN.clean[,1:5])

rowsums<-rowSums(OCEAN.clean[,1:5])

colavg<-apply(OCEAN.clean[,1:5],1,mean) 
  # apply(DATA, row or column, function)
  # apply(data, (row=1, column=2), function)

#get the sums for A and O
A.sum<-rowSums(OCEAN.clean[,1:5])
O.sum<-rowSums(OCEAN.clean[,21:25])

#get the correlation between A and O scores
cor(A.sum,O.sum)

#apply the "sumfunc" descriptives function to all columns in the OCEAN data
apply(OCEAN.clean,2,sumfunc)

# a simple regression, predicting A scores based on O scores
myline<-lm(A.sum~O.sum)
attributes(myline)

# Take a random sample from OCEAN

set.seed(1)

OCEAN.sample<-sample(1:nrow(OCEAN.clean), 
                     100,
                     replace=FALSE)

OCEAN.sample<-OCEAN.clean[OCEAN.sample,]

# CLT

draw1<-rnorm(10,mean=100,sd=15)
draw2<-rnorm(10,mean=100,sd=15)

# create a vector to collect our results
mymeans<-vector()

# loop through 5000 hypothetical samples
for(i in 1:5000){
  draw<-mean(rnorm(8)) # draw 8 observations from Z
  mymeans<-c(mymeans,draw) # add it to the vector
}

# Look at the distribrution
hist(mymeans,breaks=100)


# let's try it with a non-normal distribution
hist(rbeta(1000,15,3))

# if 15 successes and 3 failures, the expected
# success rate is 15/(15+3)=.83333

mymeans2<-vector()
for(i in 1:50000){
  
  draw<-mean(rbeta(8,15,3))
  mymeans2<-c(mymeans2,draw)
  
}

hist(mymeans2,breaks=100)























