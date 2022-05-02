# Question 7

library(randomForest)
library(MASS)
library(ISLR2)
train = sample(1:nrow(Boston),nrow(Boston)/2)
names(Boston)

mty_seq = seq(1:13)
ntree = seq(500,5000,500)

test.err = matrix(nrow = length(mty_seq),ncol = length(ntree))
for (i in seq(length(mty_seq))){
  for (j in seq(length(ntree))){
    rf.Boston = randomForest(medv~., data = Boston[train,], mtry = mty_seq[i], ntree = ntree[j])
    pred = predict(rf.Boston, newdata = Boston[-train,])
    test.err[i,j] = mean((pred - Boston[-train,"medv"])**2)
  }
}

test.err
which.min(test.err)
#7,1000

rm(rf.Boston)
rm(test.err)


# Question 8

library(tree)

train = sample(1:nrow(Carseats),0.7*nrow(Carseats))
tree.carseats = tree(Sales~. , data = Carseats, subset = train)
plot(tree.carseats)
text(tree.carseats,pretty = 0)
tree.pred = predict(tree.carseats, newdata = Carseats[-train,])
mean((tree.pred - Carseats[-train,"Sales"])**2)

cv.tree.carseats = cv.tree(tree.carseats, FUN = prune.tree)
cv.tree.carseats
plot(cv.tree.carseats$size, cv.tree.carseats$dev,type = "b")

prune.tree.carseats = prune.tree(tree.carseats,best = 7)
plot(prune.tree.carseats)
text(prune.tree.carseats,pretty = 0)
tree.pred = predict(prune.tree.carseats, newdata = Carseats[-train,])
mean((tree.pred - Carseats[-train,"Sales"])**2)

bag.carseats = randomForest(Sales~., data = Carseats, subset = train)
bag.pred = predict(bag.carseats, newdata = Carseats[-train,])
mean((bag.pred - Carseats[-train,"Sales"])**2)
varImpPlot(bag.carseats)

rf.carseats = randomForest(Sales~., data = Carseats, subset = train, mtry = 4)
rf.pred = predict(rf.carseats, newdata = Carseats[-train,])
mean((rf.pred - Carseats[-train,]$Sales)**2)
importance(rf.carseats)

#Question 9

library(ISLR2)
attach(OJ)
train = sample(1:nrow(OJ),800)

tree.oj = tree(Purchase~., data = OJ, subset = train)
summary(tree.oj)
tree.oj
plot(tree.oj)
text(tree.oj, pretty = 0)

test.pred = predict(tree.oj, newdata = OJ[-train,],type = "class")
table(test.pred, OJ[-train,"Purchase"])
(33+27)/(length(test.pred))

cv.tree.oj = cv.tree(tree.oj, FUN = prune.misclass)
cv.tree.oj
plot(cv.tree.oj$size, cv.tree.oj$dev,type = "b")

prune.tree.oj = prune.tree(tree.oj, best = 2)
plot(prune.tree.oj)
text(prune.tree.oj,pretty = 0)
test.pred.prune = predict(prune.tree.oj, newdata = OJ[-train,],type = "class")
table(test.pred.prune, OJ[-train,"Purchase"])
(31+22)/(length(test.pred))

summary(tree.oj)
summary(prune.tree.oj)


# Question 10

library(gbm)
attach(Hitters)
dim(Hitters)
Hitters = na.omit(Hitters)
dim(Hitters)
Hitters$log_sal = log(Hitters$Salary)

train = seq(1,200)

shr = seq(0.01,0.15,0.02)
shr.error = rep(NA,8)
  
for (i in 1:8){
  boost.hit = gbm(log_sal~.-Salary, data = Hitters[train,], distribution = "gaussian",shrinkage = shr[i],
                  n.trees = 1000)
  test.pred = predict(boost.hit, newdata = Hitters[-train,], n.trees = 1000)
  shr.error[i] = mean((test.pred - Hitters[-train,"log_sal"])**2)
}
plot(shr, shr.error,type = "b")
which.min(shr.error)
summary(boost.hit)

# Question 11

data("Caravan")
names(Caravan)

train = seq(1,1000)

boost.caravan = gbm(Purchase~., data = Caravan[train,], distribution = "gaussian", shrinkage = 0.01,
                    n.trees = 1000)
summary(boost.caravan)

test.boost = predict.gbm(boost.caravan, newdata = Caravan[-train,], n.trees = 1000,type = "response")
test.boost.pred = ifelse(test.boost > 1.2,"Yes","No")
table(test.boost.pred, Caravan[-train,"Purchase"])
(42+278) / length(test.boost.pred)

log.caravan = glm(Purchase~., data = Caravan, subset = train, family = binomial)
summary(log.caravan)

test.log = predict.glm(log.caravan, newdata = Caravan[-train,], type = "response")
test.log.pred = ifelse(test.log > 0.2, "Yes","No")
table(test.log.pred, Caravan[-train,"Purchase"])
(350+231) / length(test.log.pred)

library(class)

standardized.X = scale(Caravan[,-86])
train.X = standardized.X[train,]
test.X = standardized.X[-train,]
train.Y = Caravan$Purchase[train]
test.Y = Caravan$Purchase[-train]

knn.pred = knn(train.X, test.X, train.Y, k = 1)
mean(knn.pred != test.Y)

knn_3.pred = knn(train.X, test.X, train.Y, k = 3)
mean(knn_3.pred != test.Y)

knn_5.pred = knn(train.X, test.X, train.Y, k = 5)
mean(knn_5.pred != test.Y)
