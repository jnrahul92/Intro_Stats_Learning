library(ISLR2)
library(MASS)
library(car)

#Question 8

data("Auto")
names(Auto)
fit_Q8 <- lm(mpg~horsepower,data = Auto)
summary(fit_Q8)
predict(fit_Q8,data.frame(horsepower=c(98)),interval="confidence")
predict(fit_Q8,data.frame(horsepower=c(98)),interval="prediction")

par(mfrow=c(1,1))
plot(Auto$mpg~Auto$horsepower)
abline(fit_Q8,col="red",lwd=3)

par(mfrow=c(2,2))
plot(fit_Q8)

# Question 9

pairs(Auto)
?cor
cor(Auto[,c(-9)])

fit_q9 <- lm(mpg~.-name,data = Auto)
summary(fit_q9)
plot(fit_q9)

fit_q9_2 <- lm(mpg~.-name+cylinders*displacement,data = Auto)
summary(fit_q9_2)

fit_q9_3 <- lm(mpg~.-name+cylinders*displacement*weight,data = Auto)
summary(fit_q9_3)

fit_q9_4 <- lm(mpg~.-name+cylinders*displacement+cylinders*weight,data = Auto)
summary(fit_q9_4)

vif(fit_q9_4)

fit_q9_5 <- lm(mpg~horsepower+I(horsepower^2),data = Auto)
summary(fit_q9_5)

fit_q9_6 <- lm(mpg~poly(horsepower,5),data = Auto)
summary(fit_q9_6)

fit_q9_7 <- lm(mpg~log(horsepower),data = Auto)
summary(fit_q9_7)

fit_q9_8 <- lm(mpg~horsepower+I(horsepower^0.5),data = Auto)
summary(fit_q9_8)

fit_q9_9 <- lm(mpg~weight+I(weight^0.5)+horsepower+year+origin,data = Auto)
summary(fit_q9_9)

anova(fit_q9_8,fit_q9_9)

# Question 10

data("Carseats")
names(Carseats)

fit_q10 <- lm(Sales~Price+Urban+US,data = Carseats)
summary(fit_q10)

fit_q10_e <- lm(Sales~Price+US,data = Carseats)
summary(fit_q10_e)
confint(fit_q10_e)

plot(fit_q10_e)

#Question 11

set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)
fit_11 <- lm(y~x+0)
summary(fit_11)

fit_11_b <- lm(x~y+0)
summary(fit_11_b)

fit_11_c <- lm(y~x)
summary(fit_11_c)
fit_11_d <- lm(x~y)
summary(fit_11_d)


# Question 12

x = rnorm(100)
y = x + rnorm(100,sd = 0.0001)
summary(lm(y~x+0))
summary(lm(x~y+0))

# Question 13

par(mfrow=c(1,1))
set.seed(1)
x = rnorm(100)
eps = rnorm(100,sd = 0.75**0.5)
y = -1 + 0.5*x + eps
summary(lm(y~x))
plot(y~x)
abline(lm(y~x),col="red",lwd=3)
summary(lm(y~x+I(x^2)))

#Question 14

set.seed(1)
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)
plot(x1~x2)
cor(x1,x2)

summary(lm(y~x1+x2))

summary(lm(y~x1))
summary(lm(y~x2))

x1 = c(x1,0.1)
x2 = c(x2,0.8)
y = c(y,6)

summary(lm(y~x1+x2))
summary(lm(y~x1))
summary(lm(y~x2))

par(mfrow=c(2,2))
plot(lm(y~x1+x2))


#Question 15

data("Boston")
names(Boston)
fit15_1 <- lm(crim~zn,data = Boston)
fit15_2 <- lm(crim~indus,data = Boston)
fit15_3 <- lm(crim~chas,data = Boston)
fit15_4 <- lm(crim~nox,data = Boston)
fit15_5 <- lm(crim~rm,data = Boston)
fit15_6 <- lm(crim~age,data = Boston)
fit15_7 <- lm(crim~dis,data = Boston)
fit15_8 <- lm(crim~rad,data = Boston)
fit15_9 <- lm(crim~tax,data = Boston)
fit15_10 <- lm(crim~ptratio,data = Boston)
fit15_11 <- lm(crim~black,data = Boston)
fit15_12 <- lm(crim~lstat,data = Boston)
fit15_13 <- lm(crim~medv,data = Boston)
summary(fit15_1)
summary(fit15_2)
summary(fit15_3)
summary(fit15_4)
summary(fit15_5)
summary(fit15_6)
summary(fit15_7)
summary(fit15_8)
summary(fit15_9)
summary(fit15_10)
summary(fit15_11)
summary(fit15_12)
summary(fit15_13)
plot(crim~rad)
plot(crim~lstat)
plot(crim~medv)
plot(crim~dis)

plot(fit15_8)

lm.fit15 <- lm(crim~.,data = Boston)
summary(lm.fit15)

par(mfrow=c(1,1))

c_x = c(coef(fit15_1)[2],coef(fit15_2)[2],coef(fit15_3)[2],coef(fit15_4)[2],
        coef(fit15_5)[2],coef(fit15_6)[2],coef(fit15_7)[2],coef(fit15_8)[2],
        coef(fit15_9)[2],coef(fit15_10)[2],coef(fit15_11)[2],coef(fit15_12)[2],
        coef(fit15_13)[2])
c_y = coef(lm.fit15)[2:14]
plot(c_x,c_y)

summary(lm(crim~poly(zn,3)))
summary(lm(crim~poly(indus,3)))
summary(lm(crim~poly(nox,3)))
summary(lm(crim~poly(rad,3)))
