# Task 3
#====================================================>
# Split the data set randomly into two equal parts, 
# which will serve as the training set and the test set.
# Use your birthday (in the format MMDD) as the seed for the pseudo-random number generator. 
# The same training and test sets should be used throughout this assignment.
#====================================================>
# October, 06
set.seed(1006) 
# Splitting the data set randomly into two equal parts
new.data <- sample(1:nrow(Auto), nrow(Auto)/2) 
# the training set and the test set that will be used throughout this assignment.
auto.train <- Auto[new.data,]
auto.test <- Auto[-new.data,]

# Adding 2 new columns for the Intercepts
auto.train <- cbind(rep(1,196),auto.train)
auto.test <- cbind(rep(1,196),auto.test)

# Naming the columns
names(auto.train)[1] <- "Intercept"
names(auto.test)[1] <- "Intercept"

# Removing the names
rownames(auto.train) <- c()
rownames(auto.test) <- c()

# Separating the attributes and labels of the training set and the test set
train.X <- as.matrix(auto.train[-ncol(auto.train)])
test.X <- as.matrix(auto.test[-ncol(auto.test)])
train.Y <- as.matrix(auto.train$high)
test.Y <- as.matrix(auto.test$high)
