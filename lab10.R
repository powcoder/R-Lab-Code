#ips.full<-read.table("clipboard",header=T)

# First, look at the distributions.  What are we looking for?
boxplot(ips.full$ips~ips.full$type)

# R will automatically dummy code for you.
mod1<-lm(ips~type,data=ips.full)
summary(mod1)

# If we want to dummy code ourselves, we have to do some stuff manually.  We'll have to know
# what are the names of our categories, and in what order R has them stored.  This function
# gives the categories in order
levels(ips.full$type)

# Let's make a new data set so we don't ruin our original data.  We'll add the dummy coding here.
ips.dum.dpc<-ips.full

# We have four categories, so we'll need three dummy variables.  Let's name them "SURdum" when surgery is 
# the dummy variable, etc.  In this case, DPC (direct patient care) is the reference category.
ips.dum.dpc$SURdum<-ips.full$type=="SUR"
ips.dum.dpc$IPCdum<-ips.full$type=="IPC"
ips.dum.dpc$NPCdum<-ips.full$type=="NPC"

# Run the model.
mod2<-lm(ips~SURdum+IPCdum+NPCdum, data=ips.dum.dpc)
summary(mod2)

# Let's do it again.  This time, we'll categorize surgery as the reference variable.  This way,
# We can compare each area to surgery, which may be more interesting.
ips.dum.sur<-ips.full

ips.dum.sur$DPCdum<-ips.full$type=="DPC"
ips.dum.sur$IPCdum<-ips.full$type=="IPC"
ips.dum.sur$NPCdum<-ips.full$type=="NPC"

mod3<-lm(ips~DPCdum+IPCdum+NPCdum, data=ips.dum.sur)
summary(mod3)

#####
# Now let's try using R's built-in feature for coding.  This function will return
# dummy coding for four variables:
contr.treatment(4)

newcontrast<-matrix(c(1,0,0,
                      0,1,0,
                      0,0,0,
                      0,0,1),nrow=4,byrow=T)
colnames(newcontrast)<-c("DPC","IPC","SUR")

# We can tell R to apply this coding to our variable of interest
contrasts(ips.full$type)=contr.treatment(4)
contrasts(ips.full$type)=newcontrast


# When we run the model, R automatically applies the dummy coding for us.  Note that this is a little
# more difficult to figure out, because the labels in the output don't directly correspond with the
# order of the categories.
summary(lm(ips~type, data=ips.full))

################################
#Unweighted effects coding

# Again, look at our categories
levels(ips.full$type)

# This function will give us basic unweighted effects coding
contr.sum(4)

# Like before, we can tell R to use this by applying it to our variable
contrasts(ips.full$type)=contr.sum(4)

mod4<-lm(ips~type,data=ips.full)
summary(mod4)

# The intercept in the above model is now the unweighted mean.  This is going to differ from our 
# observed mean.
mean(na.omit(ips.full$ips))

# We can get the coefficients from the model....
mod4$coefficients

# And we can calcluate our expected means by multiplying
# The coefficients by the effects codes.  
# Note that we hvae to include the first "1" so that it always includes the
# intercept (the unweighted mean).
mod4$coefficients*c(1,0,0,1)

sum(mod4$coefficients*c(1,0,0,1)) #type3 = NPC
mean(na.omit(ips.full$ips[ips.full$type=="NPC"]))

sum(mod4$coefficients*c(1,-1,-1,-1)) #type4 = SUR
mean(na.omit(ips.full$ips[ips.full$type=="SUR"]))

# And we can get confidence intervals.  These reflect the
# 95%CI for the distance of each group's mean from the overall mean.
confint(mod4)


#If we want to change the effects coding, we can just make a matrix
# and apply it in the same  manner as before.

uw.NPC<-matrix(c(1,0,0,
                 0,1,0,
                 -1,-1,-1,
                 0,0,1),nrow=4,byrow=T)
colnames(uw.NPC)<-c("DPC", "IPC", "SUR")
contrasts(ips.full$type)=uw.NPC

summary(lm(ips~type, data=ips.full))

#### Weighted effects coding

# Recall that in weighted effects coding, we're interested in preserving the
# group proportions in the outcome (mean).  We use a special matrix for this.

w.SUR<-contr.sum(4)
w.SUR

# the w.SUR is the usual output from the UNweighted effects coding.  
# We'll multiply the last column by some sample sizes to force the 
# outcome to be weighted, turning it into WEIGHTED effects coding. 

# Get the counts of each category
n.type<-table(ips.full$type)
n.type

# multiply the LAST row (row 4) of the w.SUR matrix by the 
# category sample sizes divided by the "reference" category sample
# size.  In this case, we divide the first three categories' sample sizes 
# (n.type[1:3]) by the fourth category's sample size (n.type[4])
w.SUR[4,]<-w.SUR[4,]*n.type[1:3]/n.type[4]
w.SUR

# Then, like before, we apply the matrix to the data and R does the rest.
contrasts(ips.full$type)=w.SUR


mod5<-lm(ips~type, data=ips.full)
summary(mod5)


w.DPC<-matrix(c(-1,-1,-1,
                1,0,0,
                 0,1,0,
                 0,0,1),nrow=4,byrow=T)

w.DPC[1,]<-w.DPC[1,]*n.type[2:4]/n.type[1]
w.DPC


contrasts(ips.full$type)=w.DPC

mod6<-lm(ips~type, data=ips.full)
summary(mod6)


mod6$coefficients

mod6$coefficients*c(1,0,1,0)

sum(mod6$coefficients*c(1,0,1,0)) #type3 = NPC
mean(na.omit(ips.full$ips[ips.full$type=="NPC"]))

# For weighted effects coding, wehen we calcluate the mean for the 
# "reference" group, we have to use the weights we put in the
# last row of the effects coding matrix.
# We also have to keep the 1, so we use c(1,w.SUR[4,])

sum(mod6$coefficients*c(1,w.DPC[4,]))
mean(na.omit(ips.full$ips[ips.full$type=="SUR"]))

confint(mod5)


