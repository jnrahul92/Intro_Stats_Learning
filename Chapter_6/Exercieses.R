#Library Import
library(ISLR2)
library(pls)
library(leaps)
library(MASS)
library(glmnet)

# Question 8

X = rnorm(100)
eps = rnorm(100)
y = 0.5 + 2 * X + 4 * (X**2) + 3 * (X**3) + eps
df <- data.frame(X,y)

#Best Subset

reg.best <- regsubsets(y~poly(X,10), data = df, nvmax = 10)
reg.summary = summary(reg.best)

par(mfrow = c(2,2))
plot(reg.summary$rss,xlab = "Number of variables", ylab ="RSS",type = "l")
plot(reg.summary$adjr2,xlab = "Number of variables", ylab ="Adjusted Rsq",type = "l")
adj_max = which.max(reg.summary$adjr2)
points(adj_max,reg.summary$adjr2[adj_max],col="red",pch=20)
plot(reg.summary$cp,xlab = "Number of variables", ylab ="Cp",type = "l")
adj_min_cp = which.min(reg.summary$cp)
points(adj_min_cp,reg.summary$cp[adj_min_cp],col="red",pch=20,cex=2)
plot(reg.summary$bic,xlab = "Number of variables", ylab ="BIC",type = "l")
adj_min_bic = which.min(reg.summary$bic)
points(adj_min_bic,reg.summary$bic[adj_min_bic],col="red",pch=20,cex=2)

# Forward Step-wise

reg.fwd = regsubsets(y~poly(X,10),data = df, method = "forward", nvmax = 10)
reg.bwd = regsubsets(y~poly(X,10),data = df, method = "backward", nvmax = 10)

coef(reg.best,3)
coef(reg.fwd,3)
coef(reg.bwd,3)

# Lasso

train = sample(1:length(X),length(X)/2)
test = (-train)
y.test = y[test]
grid = 10^seq(10,-2,length=100)
set.seed(1)
par(mfrow=c(1,1))
cv.out = cv.glmnet(poly(X,10)[train,],y[train],alpha=1)
plot(cv.out)
best_lam = cv.out$lambda.min
best_lam
lasso.pred = predict(cv.out,s=best_lam, newx = poly(X,10)[test,])
mean((lasso.pred-y.test)**2)
lasso <- glmnet(poly(X,10),y,alpha = 1, lamda=grid)
lasso.coef = predict(lasso, s = best_lam, type = "coefficients")[1:11,]
lasso.coef[lasso.coef!=0]

y = 1 + 0.7 * (X**7) + eps
df <- data.frame(X,y)
reg.best <- regsubsets(y~poly(X,10), data = df, nvmax = 10)
reg.summary = summary(reg.best)

par(mfrow = c(2,2))
plot(reg.summary$rss,xlab = "Number of variables", ylab ="RSS",type = "l")
plot(reg.summary$adjr2,xlab = "Number of variables", ylab ="Adjusted Rsq",type = "l")
adj_max = which.max(reg.summary$adjr2)
points(adj_max,reg.summary$adjr2[adj_max],col="red",pch=20)
plot(reg.summary$cp,xlab = "Number of variables", ylab ="Cp",type = "l")
adj_min_cp = which.min(reg.summary$cp)
points(adj_min_cp,reg.summary$cp[adj_min_cp],col="red",pch=20,cex=2)
plot(reg.summary$bic,xlab = "Number of variables", ylab ="BIC",type = "l")
adj_min_bic = which.min(reg.summary$bic)
points(adj_min_bic,reg.summary$bic[adj_min_bic],col="red",pch=20,cex=2)


train = sample(1:length(X),length(X)/2)
test = (-train)
y.test = y[test]
grid = 10^seq(10,-2,length=100)
set.seed(1)
par(mfrow=c(1,1))
cv.out = cv.glmnet(poly(X,10)[train,],y[train],alpha=1)
plot(cv.out)
best_lam = cv.out$lambda.min
best_lam
lasso.pred = predict(cv.out,s=best_lam, newx = poly(X,10)[test,])
mean((lasso.pred-y.test)**2)
lasso <- glmnet(poly(X,10),y,alpha = 1, lamda=grid)
lasso.coef = predict(lasso, s = best_lam, type = "coefficients")[1:11,]
lasso.coef[lasso.coef!=0]


# Question 9

data("College")
dim(College)
train = sample(nrow(College),nrow(College)/2)
test = (-train)
names(College)
?College

lm.fit = lm(Apps~., data = College, subset = train)
lm.pred <- predict(lm.fit, newdata = College[test,])
mean((lm.pred- College$Apps[test])**2)

X = model.matrix(Apps~.,data = College)[,-1]
y = College[,2]

cv.out = cv.glmnet(X[train,],y[train],alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
ridge.pred = predict(cv.out,s=bestlam, newx = X[test,])
mean((ridge.pred- College$Apps[test])**2)

lasso.cv.out = cv.glmnet(X[train,],y[train],alpha=1)
plot(lasso.cv.out)
best_lam = lasso.cv.out$lambda.min
lasso.pred = predict(lasso.cv.out, s=best_lam, newx = X[test,])
mean((lasso.pred - College$Apps[test])**2)
lasso.coef = predict(lasso.cv.out, s = best_lam, type = "coefficients")
lasso.mod = glmnet(X[train,],y[train],alpha = 1, lambda = grid)
plot(lasso.mod)
lasso.coef[lasso.coef!=0]

pcr.fit = pcr(Apps~., data = College, scale = TRUE, subset = train,validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit)
pcr.pred = predict(pcr.fit, X[test,], ncomp = 16)
mean((pcr.pred - College$Apps[test])**2)

pls.fit = plsr(Apps~., data = College, scale = TRUE, subset = train,validation = "CV")
validationplot(pls.fit, val.type = "MSEP")
pls.pred = predict(pls.fit, X[test,],ncomp = 10)
mean((pls.pred - College$Apps[test])**2)


# Question 11

data("Boston")
names(Boston)
X = model.matrix(crim~., data = Boston)[,-1]
y = Boston$crim

train = sample(nrow(X),nrow(X)/2)
test = (-train)
lm.fit <- lm(data = Boston, crim~., subset = train)
lm.pred = predict(lm.fit, newdata = Boston[test,])

ridge.cv.mod = cv.glmnet(X,y,subset=train,alpha=0,lambda = grid)
plot(ridge.cv.mod)
best_lam = ridge.cv.mod$lambda.min
ridge.pred = predict(ridge.cv.mod, s = best_lam, newx = X[test,])

lasso.cv.mod = cv.glmnet(X,y,subset=train,alpha=1,lambda = grid)
plot(lasso.cv.mod)
best_lam = lasso.cv.mod$lambda.min
lasso.pred = predict(lasso.cv.mod, s=best_lam, newx = X[test,])
lasso.coef = predict(lasso.cv.out, s=best_lam, type="coefficients")
lasso.coef[lasso.coef!=0]

pcr.fit = pcr(crim~., data=Boston, subset=train, scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit)
pcr.pred = predict(pcr.fit, X[test,], ncomp = 8)

pls.fit = plsr(crim~., data=Boston, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit)
pls.pred = predict(pls.fit, X[test,], ncomp = 4)

mean((lm.pred - Boston$crim)**2)
mean((ridge.pred[1] - Boston$crim)**2)
mean((lasso.pred[1] - Boston$crim)**2)
mean((pcr.pred[1] - Boston$crim)**2)
mean((pls.pred[1] - Boston$crim)**2)

summary(lm.fit)
