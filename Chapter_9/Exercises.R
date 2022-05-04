# Import the libraries

library(ISLR2)
library(e1071)

# Question 7

data("Auto")
names(Auto)
med_mil = median(Auto$mpg)
Auto$mpg_2 = as.factor(ifelse(Auto$mpg>med_mil,1,0))
table(Auto$mpg_2)

dat = Auto[,-1]
names(dat)

tune.out = tune(svm, mpg_2~., data = dat, kernel = "linear", 
                ranges = list(cost = c(0.1,1,10,100,1000)))
summary(tune.out)
summary(tune.out$best.model)

table(tune.out$best.model$fitted, dat$mpg_2)

tune.out = tune(svm, mpg_2~., data = dat, kernel = "radial", 
                ranges = list(cost = c(0.1,1,10,100,1000),
                              gamma = c(0.01,0.1,0.2,0.5,1,2)))
summary(tune.out)
summary(tune.out$best.model)
table(tune.out$best.model$fitted, dat$mpg_2)

tune.out  = tune(svm, mpg_2~., data = dat, kernel = "polynomial",
                 ranges = list(degree = c(1,2,3,4,5)))
summary(tune.out)
summary(tune.out$best.model)
table(tune.out$best.model$fitted, dat$mpg_2)


# Question 8

data(OJ)
summary(OJ$Purchase)
OJ$Purchase = as.factor(OJ$Purchase)
train = sample(1:nrow(OJ), 800)

svc = svm(Purchase~., data = OJ, subset = train,kernel = "linear", cost = 0.01, scale = FALSE)
summary(svc)
table(svc$fitted, OJ[train,"Purchase"])
(44+132)/800

pred.te = predict(svc, newdata = OJ[-train,])
table(pred.te, OJ[-train,"Purchase"])
(20+39)/270

tune.out = tune(svm, Purchase~., data = OJ[train,],kernel = "linear", 
                ranges = list(cost = c(0.01,0.05,0.1,0.2,0.5,1,5,10)),
                scale = TRUE)
summary(tune.out)
plot(tune.out$performances$cost, tune.out$performances$error,type = "b",
     xlab = "cost", ylab = "Error", main="Cross Validation Results")

svm.bestmod = tune.out$best.model

table(svm.bestmod$fitted, OJ[train,"Purchase"])
(58+78)/800

pred.te = predict(svm.bestmod, newdata = OJ[-train,])
table(pred.te, OJ[-train,"Purchase"])
(23+15)/270


tune.out = tune(svm, Purchase~., data = OJ[train,],kernel = "radial", 
                ranges = list(cost = c(0.01,0.05,0.1,0.2,0.5,1,5,10)),
                scale = TRUE)
summary(tune.out)
plot(tune.out$performances$cost, tune.out$performances$error,type = "b",
     xlab = "cost", ylab = "Error", main="Cross Validation Results")

svm.bestmod.radial = tune.out$best.model

table(svm.bestmod.radial$fitted, OJ[train,"Purchase"])
(42+81)/800

pred.te.radial = predict(svm.bestmod.radial, newdata = OJ[-train,])
table(pred.te.radial, OJ[-train,"Purchase"])
(24+19)/270


tune.out = tune(svm, Purchase~., data = OJ[train,],kernel = "polynomial", 
                ranges = list(cost = c(0.01,0.05,0.1,0.2,0.5,1,5,10)),
                scale = TRUE, degree=2)
summary(tune.out)
plot(tune.out$performances$cost, tune.out$performances$error,type = "b",
     xlab = "cost", ylab = "Error", main="Cross Validation Results")

svm.bestmod.poly = tune.out$best.model

table(svm.bestmod.poly$fitted, OJ[train,"Purchase"])
(33+94)/800

pred.te.poly = predict(svm.bestmod.poly, newdata = OJ[-train,])
table(pred.te.poly, OJ[-train,"Purchase"])
(15+30)/270
