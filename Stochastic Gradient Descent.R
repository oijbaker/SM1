library(ggplot2)

# make classification dataset:

x_0 = -3
y_0 = -3

x_1 = 3
y_1 = 3

sigma = 1
n = 200

sample_class_0 <- function(n) {
  return(cbind(rnorm(n, x_0, sigma), rnorm(n, y_0, sigma)))
}

sample_class_1 <- function(n) {
  return(cbind(rnorm(n, x_1, sigma), rnorm(n, y_1, sigma)))
}

class_0 = cbind(rnorm(n,x_0,sigma), rnorm(n,y_0,sigma))
class_1 = cbind(rnorm(n,x_1,sigma), rnorm(n,y_1,sigma))

X = rbind(class_0, class_1)
data = cbind(c(rep(-1,n), rep(1,n)), X)
data <- data[sample(1:nrow(data)),]
data
y = data[,1]
X = data[,c(2,3)]
X

pl = ggplot()
pl <- pl + geom_point(aes(class_0[,1], class_0[,2]), col='red') + geom_point(aes(class_1[,1], class_1[,2]), col='blue')

# Stochastic Gradient Descent

f <- function(x, w) {
  return(t(w)%*%c(x,1))
}

max_iter = 100
eta_0 = 0.1

# initialise random w
w = rnorm(ncol(X)+1)

for (iter in 1:max_iter) {
  step = eta_0/sqrt(iter)
  for (i in 1:nrow(data)) {
    if (y[i]*f(X[i,], w) <= 0) {
      w <- w+eta_0*y[i]*c(X[i,],1)
    }
  }
}

print(w)

pl <- pl + geom_abline(intercept=-w[3]/w[2], slope=-w[1]/w[2])
pl


# test predictions:
test_size = 100
test_size <- test_size%/%2
class_0_X_test = sample_class_0(test_size)
class_1_X_test = sample_class_1(test_size)
X_test = rbind(class_0_X_test, class_1_X_test)
y_test = c(rep(-1,test_size), rep(1,test_size))
test = cbind(c(rep(-1,test_size), rep(1,test_size)), X_test)

correct = 0
correct_index = c()
incorrect_index = c()
for (i in 1:nrow(X_test)) {
  if (y[i]*f(X_test[i,], w) >= 0) {
    correct <- correct + 1
    correct_index <- c(correct_index, i)
  } else {
    incorrect_index <- c(incorrect_index, i)
  }
}
print(incorrect_index)
print(correct/nrow(X_test)*100)

pl_correct = ggplot()
pl_correct <- pl_correct + geom_point(aes(X_test[correct_index,][,1], X_test[correct_index,][,2]), col='green')
pl_correct <- pl_correct + geom_point(aes(X_test[incorrect_index,][,1], X_test[incorrect_index,][,2]), col='red')
pl_correct

pl <- pl + geom_point(aes(class_0[,1], class_0[,2]), col='red') + geom_point(aes(class_1[,1], class_1[,2]), col='blue')
