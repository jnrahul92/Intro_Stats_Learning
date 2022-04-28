library(ISLR2)

# Question 6

cv.error = rep(0,5)
attach(Wage)
train = sample(1:3000,2500,replace = TRUE)
for (i in 1:5){
  fit = lm(wage~poly(age,i),data = Wage,subset = train)
  pred = predict(fit, newdata = data.frame(age=poly(age,i))[-train])
  cv.error[i] = sum((pred - Wage[-train]$wage)**2)
}
lines(cv.error)

agelims = range(age)
age.grid = seq(agelims[1],agelims[2])
lm.best = lm(wage~poly(age,3),data = Wage)
preds = predict(lm.best, newdata = list(age = age.grid),se=T)
se.bands = cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
plot(age,wage,xlim = agelims,cex=0.5,col="darkgrey")
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid, se.bands, lwd = 1, lty = 3, col = "blue")

cv.error.2 = rep(0,5)

for (i in 2:5) {
  fit = lm(wage~cut(age,i),data = Wage, subset = train)
  pred = predict(fit, newdata = Wage[-train])
  cv.error.2[i] = sum((pred-Wage[-train]$wage)**2)
}
plot(cv.error.2,type = "l")


# Question 7

pairs(Wage)
library(gam)

mod_1 = gam(wage~s(age, df = 16)+year+education,data = Wage)
mod_2 = gam(wage~s(age, df = 16)+year+education+maritl,data = Wage)
mod_3 = gam(wage~s(age, df = 16)+year+education+maritl+jobclass,data = Wage)
mod_4 = gam(wage~s(age,df = 16)+year+education+maritl+jobclass+race,data = Wage)

anova(mod_1, mod_2, mod_3, mod_4, test = "F")


# Question 8

data("Auto")
names(Auto)
summary(Auto)
plot(Auto$mpg, Auto$horsepower)
plot(Auto$mpg, Auto$displacement)
plot(Auto$mpg, Auto$cylinders)
plot(Auto$mpg, Auto$weight)
plot(Auto$mpg, Auto$acceleration)
plot(Auto$mpg, Auto$year)

attach(Auto)

mod_1 = gam(mpg~s(horsepower,df = 5)+s(displacement,df = 5)+s(weight,df = 5)+lo(acceleration,span = 0.5),
            data = Wage)
mod_2 = gam(mpg~s(horsepower,df = 5)+s(displacement,df = 5)+cylinders+s(weight,df = 5)+lo(acceleration,span = 0.5),
            data = Wage)
anova(mod_1, mod_2)


# Question 9

attach(Boston)

lm.a = lm(nox~poly(dis,3))
summary(lm.a)
par(mfrow=c(1,1))
plot(dis,nox,col="darkgrey",cex=0.5)
dislim = range(dis)
dis.grid = seq(dislim[1],dislim[2])
preds = predict(lm.a, newdata = list(dis=dis.grid),se=T)
se.preds = cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
lines(dis.grid,preds$fit, col="blue", lwd=2)
matlines(dis.grid, se.preds, col = "blue", lwd = 2, lty = 2)

par(mfrow=c(2,5))
error = rep(0,10)

for (i in 1:10){
  fit = lm(nox~poly(dis,i),data = Boston)
  pred = predict(fit, newdata = list(dis=dis.grid),se=T)
  se.preds = cbind(pred$fit+2*pred$se.fit,pred$fit-2*pred$se.fit)
  plot(dis,nox,col="darkgrey",cex=0.5,main = c("Degree ",i, " fit"))
  lines(dis.grid,pred$fit, col="blue", lwd=2)
  matlines(dis.grid, se.preds, col = "blue", lwd = 2, lty = 2)
  error[i] = sum(fit$residuals)
}

lm.d = lm(nox~bs(dis,df = 4),data = Boston)
summary(lm.d)
par(mfrow=c(1,1))
attr(bs(dis,df = 4),"knots")
pred = predict(lm.d, newdata = list(dis=dis.grid),se=T)
se.preds = cbind(pred$fit+2*pred$se.fit,pred$fit-2*pred$se.fit)
plot(dis,nox,col="darkgrey",cex=0.5)
lines(dis.grid,pred$fit, col="blue", lwd=2)
matlines(dis.grid, se.preds, col = "blue", lwd = 2, lty = 2)

lm.e = lm(nox~ns(dis,df = 4),data = Boston)
pred = predict(lm.e, newdata = list(dis=dis.grid),se=T)
se.preds = cbind(pred$fit+2*pred$se.fit,pred$fit-2*pred$se.fit)
lines(dis.grid,pred$fit, col="red", lwd=2)
matlines(dis.grid, se.preds, col = "red", lwd = 2, lty = 2)


#Question 11

attach(College)
library(leaps)
dim(College)
names(College)

train = sample(1:777,500,replace = TRUE)
mod_1 = regsubsets(Outstate~., data = College, subset = train, method = "forward")
summary(mod_1)
coef(mod_1,4)

gam_1 = gam(Outstate~Private+Room.Board+perc.alumni+Expend, data = College, subset = train)
par(mfrow=c(1,4))
plot(gam_1)
summary(gam_1)

test.pred = predict(gam_1, newdata = College[-train,])
sqrt(sum((test.pred- College[-train,]$Outstate)**2))
var(College$Outstate)
