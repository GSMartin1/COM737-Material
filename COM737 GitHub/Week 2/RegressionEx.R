iris <- read.csv("C:/Users/se16000329/Downloads/iris.csv", stringsAsFactors = TRUE)

str(iris)
iris$class1 <- as.numeric(iris$class)

covariance <- cov(iris$sepal_length, iris$class1)

variance <- var(iris$sepal_length)

b <- covariance / variance

a <- mean(iris$class1) - b * mean(iris$sepal_length)

r <- covariance / (sd(iris$sepal_length) * sd(iris$class1))

reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  beta <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(beta) <- "estimate" 
  print(beta) 
}
reg(y = iris$class1, x = iris[1])

reg(y = iris$class1, x = iris[1:4])


RegEx <- read.csv("C:/Users/se16000329/Downloads/RegEx1.csv", stringsAsFactors = TRUE)  

str(RegEx)
x <- RegEx$x
y <- RegEx$y
all = c(x,y)
range = c(min(all), max(all))
plot(x, y, xlim=range, ylim=range)

abline(lm(RegEx$y ~ RegEx$x))
model1 <- lm(y ~ x)

newx <- data.frame(x = seq(-10, 10, 0.5))
newy <- predict(model1, newx)
newy <- as.numeric(newy)

covariance <- cov(RegEx$x, RegEx$y)

variance <- var(RegEx$x)

b <- covariance / variance

a <- mean(RegEx$y) - b * mean(RegEx$x)

newz <- a + b*newx

newz <- as.numeric(newz$x)
all.equal(newy, newz)

r <- covariance / (sd(RegEx$x) * sd(RegEx$y))

reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  beta <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(beta) <- "estimate" 
  print(beta) 
}
reg(y = RegEx$y, x = RegEx$x)

RegEx2 <- read.csv("C:/Users/se16000329/Downloads/RegEx2.csv", stringsAsFactors = TRUE)  
reg(y = RegEx2$y, x = RegEx2[1:2])
model <- lm(y~ x1+x2, data = RegEx2)
summary(model)


