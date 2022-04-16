# Question 8a
college_data <- read.csv("D:/Intro_Stats_Learning/Data/College.csv")

# Question 8b
rownames(college_data) <- college_data[,1]
fix(college_data)
college_data <- college_data[,-1]
fix(college_data)

#Question 8c
summary(college_data)
college_data$Private <- as.factor(college_data$Private)
pairs(college_data[,1:10])
plot(college_data$Private,college_data$Outstate)
elite <- rep("No",nrow(college_data))
elite[college_data$Top10perc>50]<-"Yes"
elite <- as.factor(elite)
college_data <- data.frame(college_data,elite)
summary(college_data)
plot(college_data$elite,college_data$Outstate)
par(mfrow=c(2,2))
hist(college_data$Outstate)
hist(college_data$Books)
hist(college_data$Grad.Rate)
hist(college_data$S.F.Ratio)

#Question 9a
auto <- read.csv("D:/Intro_Stats_Learning/Data/Auto.csv",na.strings = "?")
auto <- na.omit(auto)
str(auto)

#Question 9b
range(auto$mpg,)
range(auto$cylinders)
range(auto$weight)

#Question 9c
summary(auto)
sd(auto$cylinders)

#Question 9d
auto_subset <- auto[-c(10,85),]
summary(auto_subset)

#Question 9e-f
plot(auto$mpg,auto$cylinders)
plot(auto$mpg,auto$horsepower)
plot(auto$mpg,auto$weight)

#Question 10a
library(MASS)

#Question 10b
pairs(Boston)
pairs(crim~zn+indus+chas+nox,Boston)

#Question 10c
plot(Boston$dis,Boston$crim)
plot(Boston$dis,Boston$tax)
plot(Boston$dis, Boston$ptratio)
plot(Boston$dis, Boston$black)

#Question 10d
nrow(Boston[Boston$chas==1,])

#Question 10e
median(Boston$ptratio)

#Question 10f
Boston[Boston$medv==min(Boston$medv),]

#Question 10g
nrow(Boston[Boston$rm>7,])
nrow(Boston[Boston$rm>8,])
summary(Boston[Boston$rm>8,])
summary(Boston)
