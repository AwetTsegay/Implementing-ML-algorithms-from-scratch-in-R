# Task 1

#====================================================>
# Implement the logistic regression algorithm using gradient descent in R. 
# And, the objective function, which the program should minimize, 
# is the negative log-likelihood.
#====================================================>

# Sigmoid function takes the value of linear model
sigmoid <- function(t){
  return (1/(1 + exp(-t)))
}

# Returns the predicted values
Prediction <- function(X, w1){
  model <- w1 %*% t(X)
  p <- sigmoid(model)
  f.hat <- c()
  for (j in 1:length(p[1,])){
    if (p[1,j] >= 0.5) 
      f.hat[j] <- 1
    else 
      f.hat[j] <- 0 
  }
  return (f.hat)
}

# The gradient descent returns the final intercept and coefficients values.
# X is Attributes.
# Y is Labels.
# lr is Learning rate.
# s is Stopping rule.
# n is number of steps.
# w0 is Initial weight.
# w1 is New weight.
# MSE is Mean squared error.
GD <- function(X, Y, lr, s, n=NULL){
  
  w0 <- runif(ncol(X), -0.7, 0.7)
  w0 <- as.matrix(runif(ncol(X), -0.7, 0.7))

  w1 <- w0 - lr * t(X) %*% (sigmoid(X %*% w0) - Y)
  if (is.null(n)) {
    while(sum(w1 == w0) != s)
    {
      w0 <- w1
      w1 <- w0 - lr * t(X) %*% (sigmoid(X %*% w0) - Y)
    }
  }
  y.pred <- Prediction(X[,2:5],t(w1)[,2:5])
  er <- sum((Y - y.pred)^2)/(nrow(X))
  return(list(coefficients=w1, ER=er))
}
  
