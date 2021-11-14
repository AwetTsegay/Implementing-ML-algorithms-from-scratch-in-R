
# https://www.youtube.com/watch?v=xsAfedKOf-A

library(ISLR)
data(Smarket)
names(Smarket)
head(Smarket)

model <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
             data =Smarket,
             family = binomial)

model
summary(model)
model$coefficients

# sigmoid = (1/(1 + exp(-z)))
# where z = X %*% w

# p(y=1|w,X) = sigmoid(X %*% w)
# p(y=0|w,X) = 1 - sigmoid(X %*% w)

# p(y|w,X) = [sigmoid(X %*% w)^y] * [(1 - sigmoid(X %*% w))^(1-y)]

# likelihood = multiply i to n: [sigmoid(X %*% w)^y_i] * [(1 - sigmoid(X %*% w))^(1-y_i)]

# negative log likelihood = - sum i to n: y_i * log(sigmoid(X %*% w)) + (1-y_i) * log(1 - sigmoid(X %*% w))

# denote sigmoid(X %*% w) as H
# then negative log likelihood = - sum i to n: y_i * log(H) + (1-y_i) * log(1-H)

# d -logL/dw = - sum i to n: y_i * H'/H - (1-y_i) * H'/(1-H)
# where H' = H * (1-H)*x_i

# gradient = -sum i to n: y_i * (1-H) * x_i - (1-y_i)*H*x_i
# gradient = sum i to n: (H - y_i) * x_i


X <- as.matrix(Smarket[,2:7])
X <- cbind(rep(1,1250),X)
Y <- as.numeric(Smarket[,9])

Y[Y==1] <- 0; Y[Y==2] <- 1
Y

sigmoid <- function(z) 1/(1 + exp(-z))

?runif
w0 <- runif(7)
w1 <- w0 - 0.001 * t(X) %*% (sigmoid(X %*% w0) - Y)

while (sum(w1 == w0) != 7) {
  w0 <- w1
  w1 <- w0 - 0.001 * t(X) %*% (sigmoid(X %*% w0) - Y)
}

model$Coefficients
c(w1)






