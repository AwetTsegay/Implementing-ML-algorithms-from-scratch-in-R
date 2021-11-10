# Task 4
#====================================================>
# Train your algorithm on the training set 
# using independent random numbers in the range [-0.7, 0.7] as the initial weights.
# Find the Error Rate of the trained model on the test set
# Try different values of the learning rate and of the number of training steps.
# Give a small table of test and training ER in your report.
#====================================================>

# To check the outcomes, comparing gradient descent with the build-in function.
train.model<- glm(high ~ horsepower+ weight + year + origin, 
                  data=auto.train, family=binomial)
# Model training
Train.GD <- GD(train.X, train.Y, 0.01, 4)

print(train.model$coefficients)
print(t(Train.GD$coefficients))

# To check the outcomes, comparing gradient descent with the build-in function.
test.model<- glm(high ~ horsepower+ weight + year + origin, 
                 data=auto.test,family=binomial)
Test.GD <- GD(test.X, test.Y, 0.01, 4)

print(test.model$coefficients)
print(t(Test.GD$coefficients))

# ER (Error Rate) of the trained model on the test set.
test.x = test.X[,2:5]
test.coef = (t(Test.GD$coefficients)[,2:5])
test.predicted <- Prediction(test.x, test.coef)
test.er <- mean(test.predicted != test.Y)
cat("Test ER : ", test.er)

test.er.model = Test.GD$ER
print(test.er.model)

# Different values of the learning rate and 
# of the number of training steps.
# let your stopping rule be to stop after a given number of steps.
Test.ER<-c()
Train.ER<-c()
Learning.Rates<-c()
for (lr  in seq(0.000001,0.1,0.001)){
  for(i in c(1:10)){
    test.gd <- GD(test.X, test.Y, lr, 10, i)
    train.gd <- GD(train.X, train.Y, lr, 10, i)
    Test.ER <- c(Test.ER, test.gd$ER)
    Train.ER <- c(Train.ER, train.gd$RE)
    Learning.Rates <- c(Learning.Rates, lr)
  }
}

# Table of test and training ER
table <- cbind(Learning.Rates, Train.ER, Test.ER)
head(table, 12)
