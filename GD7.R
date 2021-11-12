# Task 7
#====================================================>
# Redo the experiments in items 4-5 modifying 
# the training procedure as follows. 
# Train F(X, lam) 4 times using gradient descent 
# with different values of the initial weights 
# and then choose the prediction rule with the best training objective.
#====================================================>
ER.Four <- c()
coef1 <- c()
coef2 <- c()
coef3 <- c()
coef4 <- c()
for (j in c(1:4)) {
  train.model = GD(train.X, train.Y, 0.01, 3, j)
  if (j == 1){
    coef1 <- c(coef1, train.model)}
  if (j == 2){
    coef2 <- c(coef2, train.model)}
  if (j == 3){
    coef3 <- c(coef3, train.model)}
  if (j == 4){
    coef4 <- c(coef4, train.model)}
  ER.Four <- c(ER.Four, train.model$ER)
}

print(ER.Four)

Best.ER <- min(ER.Four)
print(Best.ER)

axi = match(Best.ER, ER.Four)
put = gsub(" ", "", paste('coef',axi))
print(put)

Test.X = test.X[,2:5]
Best.weight = (t(eval(parse(text = put))$coefficients)[,2:5])
Test.Pred <- Prediction(Test.X, Best.weight)
head(Test.Pred, 40)


