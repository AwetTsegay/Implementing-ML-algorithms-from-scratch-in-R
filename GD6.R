# Task 6
#====================================================>
# Run your logistic regression program for a fixed value of lr
# and for a fixed stopping rule 100 times
# (producing reasonable results in your experiments so far)
# for different values of the initial weights 
# (produced as above,as independent random numbers in [-0.7, 0.7]). 
# In each of the 100 cases compute the test ER 
# and show it in your report as a box-plot.
#====================================================>
test.ER.fixed <- c()
for (l in c(1:100)){
  test.fixed = GD(test.X, test.Y, 0.01, 10, l)
  train.fixed = GD(train.X, train.Y, 0.01, 10, l)
  test.ER.fixed <- c(test.ER.fixed, test.fixed$ER)
}
# box plot
boxplot(test.ER.fixed, width = 2, varwidth = TRUE,
        col = 'red',
        main='Boxplot of test ERs',
        xlab='The test ERs for 100 cases',
        ylab='ERs')
