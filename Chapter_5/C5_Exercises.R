library(ISLR2)
library(boot)

#Question 5
set.seed(1)
glm.fit <- glm(default~income+balance, data = Default, 
               family = binomial)

train <- sample(10000,5000)
Default$default <- as.factor(Default$default)
glm.fit <- glm(data = Default,formula = default~income+balance,
               family = binomial, subset = train)
glm.probs <- predict(glm.fit,newdata = Default,type = "response")[-train]
glm.pred <- rep("No",5000)
glm.pred[glm.probs>0.5] <- "YES"
mean(glm.pred!=Default[-train,]$default)

set.seed(2)
train <- sample(10000,5000)
glm.fit <- glm(data = Default,formula = default~income+balance,
               family = binomial, subset = train)
glm.probs <- predict(glm.fit,newdata = Default,type = "response")[-train]
glm.pred <- rep("No",5000)
glm.pred[glm.probs>0.5] <- "YES"
mean(glm.pred!=Default[-train,]$default)

set.seed(5)
train <- sample(10000,5000)
glm.fit <- glm(data = Default,formula = default~income+balance,
               family = binomial, subset = train)
glm.probs <- predict(glm.fit,newdata = Default,type = "response")[-train]
glm.pred <- rep("No",5000)
glm.pred[glm.probs>0.5] <- "YES"
mean(glm.pred!=Default[-train,]$default)

train <- sample(10000,5000)
glm.fit <- glm(data = Default,formula = default~income+balance+student,
               family = binomial, subset = train)
glm.probs <- predict(glm.fit,newdata = Default,type = "response")[-train]
glm.pred <- rep("No",5000)
glm.pred[glm.probs>0.5] <- "YES"
mean(glm.pred!=Default[-train,]$default)


#Question 6

set.seed(42)
glm.fit <- glm(data = Default,formula = default~income+balance,
               family = binomial)
summary(glm.fit)

boot.fn <- function(data,index){
  coefficients(glm(default~income+balance, data = data, family = binomial, subset = index))
}
boot(Default, boot.fn, R = 1000)

# Question 7

attach(Weekly)
glm.fit <- glm(Direction~Lag1+Lag2, data = Weekly, family = binomial)
glm.fit <- glm(Direction~Lag1+Lag2, data = Weekly[-1,], family = binomial)
predict.glm(glm.fit,newdata = Weekly[1,],type = "response")>0.5

cv.error <- rep(0,dim(Weekly)[1])
for (i in 1:dim(Weekly)[1]){
  glm.fit <- glm(Direction~Lag1+Lag2, data = Weekly[-i,], family = binomial)
  prob <- predict.glm(glm.fit, newdata = Weekly[i,],type = "response")
  pred <- ifelse(prob>0.5,"Up","Down")
  cv.error[i] <- ifelse(pred==Weekly[i,]$Direction,0,1)
}
mean(cv.error)

# Question 8

set.seed(1)
y = rnorm(100)
x = rnorm(100)
y <- x-2*x+rnorm(100)
plot(x,y)

cv.error <- rep(0,4)
for (i in 1:4){
  temp.error <- rep(0,100)
  df <- data.frame(x,y)
  for (j in 1:100){
    lm.fit <- lm(y~poly(x,i),data = df[-j,])
    temp.error[j] <- (df[j,]$y - predict.lm(lm.fit, newdata = df[j,]))**2
  }
  cv.error[i] <- mean(temp.error)
}
cv.error

set.seed(5)
y = rnorm(100)
x = rnorm(100)
y <- x-2*x+rnorm(100)
plot(x,y)

cv.error <- rep(0,4)
for (i in 1:4){
  temp.error <- rep(0,100)
  df <- data.frame(x,y)
  for (j in 1:100){
    lm.fit <- lm(y~poly(x,i),data = df[-j,])
    temp.error[j] <- (df[j,]$y - predict.lm(lm.fit, newdata = df[j,]))**2
  }
  cv.error[i] <- mean(temp.error)
}
cv.error

# Question 9

library(MASS)
mean(Boston$medv)
nobs <- dim(Boston)[1]
serr <- sd(Boston$medv) / (nobs**0.5)

boot.fn <- function(data,i){
  data <- data[i,]
  mean(data$medv)
}
mu <- boot(Boston,boot.fn,R = 1000)$t0
conf <- mu + c(-2,2) * serr
t.test(Boston$medv)

median(Boston$medv)
boot.fn <- function(data,i){
  data <- data[i,]
  median(data$medv)
}
boot(Boston,boot.fn,R = 1000)

quantile(Boston$medv,0.1)
boot.fn <- function(data,i){
  data <- data[i,]
  quantile(data$medv,0.1)
}
boot(Boston,boot.fn,R = 1000)