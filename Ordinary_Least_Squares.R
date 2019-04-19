####### Linear Regression with "Ordinary least squares" method.  #######

#Generate data with linear function y = ax + b
x <- 1:100
y <- 3*x + 2
ye <- y + rnorm(100, 1 * x^2, 20)

#Generate matrix X. 
#Y_hat = beta_0 + beta_1*X_1 + ... + beta_p * X_p
#In this case: X_1 = x
X <- cbind(1, x)

#Solve betahat. Equal to: solve(t(x) %*% X) %*% t(x) %*% y
betahat_X <- solve( crossprod(X) ) %*% crossprod(X, ye)
betahat_X

#Compute linear Regression
y_X <- betahat_X[2]*x + betahat_X[1]


par(mfrow=c(1,2))
plot(x, ye)
lines(x, y, col = "red")
lines(x, y_X, col = "green")

m = lm(ye ~ x)
plot(resid(m) ~ predict(m))
abline(h=0)





par(mfrow=c(2,2))
x <- 1:100
y <- rnorm(100, mean=5 * x)
m <- lm(y ~ x)
qqnorm(resid(m), main = "Normal Distribution")
abline(a=0, b=1, lty="dotted")


x <- 1:100
y <- rpois(100, lambda=x)
m <- lm(y ~ x)
qqnorm(resid(m), main = "Poisson Distribution")
abline(a=0, b=1, lty="dotted")


x <- 1:100
y <- runif(100)
m <- lm(y ~ x)
qqnorm(resid(m), main = "Uniform Distribution")
abline(a=0, b=1, lty="dotted")

x <- 1:100
y <- rnorm(100, mean=5 * x^2)
m <- lm(y ~ x)
qqnorm(resid(m), main = "Exponential Distribution")
abline(a=0, b=1, lty="dotted")

