
library(MASS)
library(ISLR2)
library(class)

# Question 10

summary(Weekly)
pairs(Weekly)

glm.fit <- glm(data = Weekly, Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               family=binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit,type = "response")
glm.pred <- rep("Down",1089)
glm.pred[glm.probs>0.5] <- "Up"
table(glm.pred,Weekly$Direction)
mean(glm.pred==Weekly$Direction)

test <- Weekly$Year > 2008
Weekly.2009 <- Weekly[test,]
glm.fit <- glm(data = Weekly, Direction~Lag2, subset = !test, family = binomial)
glm.probs <- predict(glm.fit, newdata = Weekly.2009, type = "response")
glm.pred <- rep("Down",104)
glm.pred[glm.probs>0.5] <- "Up"
table(glm.pred,Weekly.2009$Direction)
mean(glm.pred==Weekly.2009$Direction)

lda.fit <- lda(Direction~Lag2, data = Weekly, subset = !test)
lda.pred <- predict(lda.fit, newdata = Weekly.2009)
table(lda.pred$class, Weekly.2009$Direction)
mean(lda.pred$class==Weekly.2009$Direction)

qda.fit <- qda(Direction~Lag2, data = Weekly, subset = -test)
qda.pred <- predict(qda.fit, newdata = Weekly.2009)
table(qda.pred$class, Weekly.2009$Direction)
mean(qda.pred$class==Weekly.2009$Direction)

train.X <- cbind(Weekly[!test,]$Lag2)
test.X <- cbind(Weekly[test,]$Lag2)
train.Y <- Weekly[!test,]$Direction
knn.pred <- knn(train.X,test.X,train.Y,1)
table(knn.pred,Weekly.2009$Direction)
mean(knn.pred==Weekly.2009$Direction)

knn.pred <- knn(train.X,test.X,train.Y,3)
table(knn.pred,Weekly.2009$Direction)
mean(knn.pred==Weekly.2009$Direction)

knn.pred <- knn(train.X,test.X,train.Y,10)
table(knn.pred,Weekly.2009$Direction)
mean(knn.pred==Weekly.2009$Direction)

knn.pred <- knn(train.X,test.X,train.Y,50)
table(knn.pred,Weekly.2009$Direction)
mean(knn.pred==Weekly.2009$Direction)


#Question 11

med_mpg <- median(Auto$mpg)
Auto$mpg01 <- ifelse(Auto$mpg>med_mpg,1,0)
table(Auto$mpg01)
pairs(Auto)
boxplot(Auto$mpg01,Auto$horsepower)
boxplot(Auto$mpg01,Auto$weight)

test <- ifelse(runif(392)>0.7,1,0)
train <- !test
training <- Auto[!test,]
testing <- Auto[!train,]

cor(Auto[,-9])

lda.fit <- lda(mpg01~cylinders+displacement+horsepower+weight,data = training)
lda.pred <- predict(lda.fit,newdata = testing)
mean(lda.pred$class==testing$mpg01)
table(lda.pred$class,testing$mpg01)

qda.fit <- qda(mpg01~cylinders+displacement+horsepower+weight,data = training)
qda.pred <- predict(qda.fit,newdata = testing)
mean(qda.pred$class==testing$mpg01)
table(qda.pred$class,testing$mpg01)

glm.fit <- glm(mpg01~cylinders+displacement+horsepower+weight,data = training,
               family=binomial)
glm.probs <- predict(glm.fit, newdata = testing, type = "response")
glm.pred <- rep(0,121)
glm.pred[glm.probs>0.5] <- 1
table(glm.pred, testing$mpg01)
mean(glm.pred==testing$mpg01)
summary(glm.fit)

train.X <- cbind(training$displacement,training$horsepower,training$weight)
test.X <- cbind(testing$displacement,testing$horsepower,testing$weight)
train.Y <- training$mpg01

knn.pred <- knn(train.X,test.X,train.Y,1)
table(knn.pred,testing$mpg01)
mean(knn.pred==testing$mpg01)

knn.pred <- knn(train.X,test.X,train.Y,3)
table(knn.pred,testing$mpg01)
mean(knn.pred==testing$mpg01)

knn.pred <- knn(train.X,test.X,train.Y,4)
table(knn.pred,testing$mpg01)
mean(knn.pred==testing$mpg01)

knn.pred <- knn(train.X,test.X,train.Y,7)
table(knn.pred,testing$mpg01)
mean(knn.pred==testing$mpg01)

knn.pred <- knn(train.X,test.X,train.Y,11)
table(knn.pred,testing$mpg01)
mean(knn.pred==testing$mpg01)

# Question 13

med_crim <- median(Boston$crim)
Boston$medcrim <- ifelse(Boston$crim>med_crim,1,0)
names(Boston)
cor(Boston)

test <- ifelse(runif(506)>0.7,1,0)
train <- !test
training <- Boston[!test,]
testing <- Boston[!train,]

glm.fit <- glm(medcrim~tax+rad+dis+age, data = training,
               family=binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit, newdata = testing, type = "response")
glm.pred <- rep(0,175)
glm.pred[glm.probs>0.5] <- 1
table(glm.pred, testing$medcrim)
mean(glm.pred==testing$medcrim)


lda.fit <- lda(medcrim~tax+rad+dis+age, data = training)
lda.pred <- predict(lda.fit,newdata = testing)
table(lda.pred$class,testing$medcrim)
mean(lda.pred$class==testing$medcrim)

qda.fit <- qda(medcrim~tax+rad+dis+age, data = training)
qda.pred <- predict(qda.fit,newdata = testing)
table(qda.pred$class,testing$medcrim)
mean(qda.pred$class==testing$medcrim)

train.X <- cbind(training$tax,training$rad,training$dis,training$age)
test.X <- cbind(testing$tax,testing$rad,testing$dis,testing$age)
train.Y <- training$medcrim

knn.pred <- knn(train.X,test.X,train.Y,1)
t <- table(knn.pred,testing$medcrim)
recall <- t[4] / (t[4]+t[3])
precision <- t[4] / (t[4]+t[2])
print(c("Recall = ",recall))
print(c("Precision = ",precision))
print(c("Accuracy = ", mean(knn.pred==testing$medcrim)))

knn.pred <- knn(train.X,test.X,train.Y,2)
t <- table(knn.pred,testing$medcrim)
recall <- t[4] / (t[4]+t[3])
precision <- t[4] / (t[4]+t[2])
print(c("Recall = ",recall))
print(c("Precision = ",precision))
mean(knn.pred==testing$medcrim)

knn.pred <- knn(train.X,test.X,train.Y,3)
table(knn.pred,testing$medcrim)
mean(knn.pred==testing$medcrim)

knn.pred <- knn(train.X,test.X,train.Y,5)
table(knn.pred,testing$medcrim)
mean(knn.pred==testing$medcrim)
