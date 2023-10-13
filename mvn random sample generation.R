#* Generating Multivariate Normal samples from only the univariate normal
#* After a coordinate transform, MVN becomes a product of univariate normal dsitributions
library(ellipse)

d = 2
# change the value in c(,) to change the mean
mu = matrix(c(2,1), nrow=d, ncol=1)

A <- matrix(runif(d^2)*2-1, ncol=d)
covariance <- t(A) %*% A
# comment this line out for a random covariance, or edit it to specify a covariance
covariance <- matrix(c(1,0.5,0.5,1),nrow=2,ncol=2)

sample_MVN <- function(n, mu, covariance) {
  ev = eigen(solve(covariance))
  U = ev$vectors

  sample = matrix(nrow=length(mu), ncol=0)
  for (j in 1:n) {
    y = matrix(nrow=0,ncol=1)
    for (i in 1:length(mu)) {
      y[i] <- rnorm(1, 0, sqrt(eigen(covariance)$values[-i]))
    }
    sample <- cbind(sample,U%*%y+mu)
  }
  return(sample)
}

count_confidence <- function(points_, mu, covariance) {
  total = 0
  for (i in 1:ncol(points_)) {
    if (t(points_[,i]-mu)%*%solve(covariance)%*%(points_[,i]-mu) <= 6) {
      total <- total+1
    }
  }
  return(total)
}

sample = sample_MVN(10000, mu, covariance)
plot(t(sample), col='slateblue1', main=paste(100*count_confidence(sample, mu, covariance)/10000,"% of points inside 95% confidence interval"))
print(covariance)
points(2,1, col='yellow')
lines(ellipse(covariance, centre=mu), col='red')

