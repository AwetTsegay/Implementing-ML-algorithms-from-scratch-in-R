# Task 2
#====================================================>
# Load the Auto data set and create a new variable high that takes values;
# 1 if mpg >= 23 or 0 otherwise.
# Apply your program to the Auto data set and new variable.  
# To predict high given horsepower, weight, year, and origin. 
# (In other words, high is the label and horsepower, weight, year, and origin are the attributes.)
# Since origin is a qualitative variable, you will have to create appropriate dummy variables. 
# Normalize the attributes.
#====================================================>

# Loading the Auto data set
Auto <- read.table("Auto.data", header=T, na.strings="?")
dim(Auto)
# Omitting the rows that contain ?
Auto <- na.omit(Auto)
dim(Auto)
names(Auto)

attach(Auto)
names(Auto)
# creating a new variable high
high <- ifelse(mpg >= 23,1,0)
Auto <- data.frame(Auto,high)

# Selecting only 5 attributes.
Auto = subset(Auto, select = c(horsepower, weight, year, origin, high))
names(Auto)

# creating appropriate dummy variables
# and Normalizing the 2 attributes.
Dummy.Auto = subset(Auto, select = -c(origin,high))
Auto = cbind(scale(Dummy.Auto),subset(Auto,select=c(origin,high)))
# Removing the name of the column
rownames(Auto) <- c()
head(Auto, 12)

