# Task 5
#====================================================>
# Try different stopping rules, 
# such as: stop when the value of the objective function does not change 
# by more than 1% of its initial value over the last 10 training steps.
#====================================================>

j = 0
Total.ER <- c()
while (TRUE) {
  value <- c()
  j = j + 1
  ER.1 <- GD(train.X, train.Y, 0.01, 10, j)
  Total.ER <- c(Total.ER, ER.1$ER)
  ER.0 = Total.ER[1]*100
  num = 0
  if(j > 10){
    value <- c()
    for(k in c(j-10:j)){
      value = c(value,(ER.0 - (Total.ER[k]*100)))
    }
    for (n in tail(value, 10)) {
      if(abs(n) <= 1.00){
        num = num +1
      }
    }
  if (num >= 10){
    break
  }
  }
  if(j > 100){
    break
  }
}

head(Total.ER, 12)
print(num)
print(ER.0)

