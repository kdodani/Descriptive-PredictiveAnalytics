library(car) 
library(MASS) 
library(MPV) 
library(Hmisc)


# Import the data:
mydata <- read.csv(file="/Users/yjq/Desktop/Project template and data/House sale data Vancouver.csv", header=TRUE)
mydata

# Create a training set and a validation set using the following code
# This will divide the dataset into ~80% training set (1042 observations)
# and ~ 20% validation set (260 observations)

# DO NOT MODIFY THIS CODE!

mydata.train <- mydata[1:1042,]
mydata.valid <- mydata[1043:nrow(mydata),]

#Price vs Days on Market
plot(Price ~ Days.on.market, data = mydata.train, pch=16, main = "Price vs Days on market")
x1 <- mydata.train$Days.on.market[order(mydata.train$Days.on.market)]
y1 <- mydata.train$Price[order(mydata.train$Days.on.market)]
lines(lowess(x1, y1, delta=100), col="red")
points(x=308, y=1400000, col="red", pch=16)
points(x=327, y=1120000, col="red", pch=16)
points(x=719, y=1150000, col="red", pch=16)
points(x=305, y=1275000, col="red", pch=16)
points(x=540, y=1480000, col="red", pch=16)
points(x=492, y=1250000, col="red", pch=16)
points(x=303, y=1900000, col="red", pch=16)
points(x=338, y=1370000, col="red", pch=16)
points(x=435, y=1270000, col="red", pch=16)

plot(Price ~ Total.floor.area, data = mydata.train, pch=16,main = "Price vs Total floor area")
x2 <- mydata.train$Total.floor.area[order(mydata.train$Total.floor.area)]
y2 <- mydata.train$Price[order(mydata.train$Total.floor.area)]
lines(lowess(x2, y2, delta=100), col="red")
points(x=734, y=2898000, col="red", pch=16)
points(x=301, y=2230000, col="red", pch=16)
points(x=5872, y=2840000, col="blue", pch=16)
points(x=5615, y=2300000, col="blue", pch=16)
points(x=5600, y=2325000, col="blue", pch=16)

plot(Price ~ Age, data = mydata.train, pch=16,main = "Price vs Age")
x3 <- mydata.train$Age[order(mydata.train$Age)]
y3 <- mydata.train$Price[order(mydata.train$Age)]
lines(lowess(x3, y3, delta=100), col="red")

plot(Price ~ Lot.Size, data = mydata.train, pch=16,main = "Price vs Lot size")
x4 <- mydata.train$Lot.Size[order(mydata.train$Lot.Size)]
y4 <- mydata.train$Price[order(mydata.train$Lot.Size)]
lines(lowess(x4, y4, delta=100), col="red")
points(x=13078, y=3000000, col="red", pch=16)
points(x=11700, y=2300000, col="red", pch=16)
points(x=10287, y=1300000, col="red", pch=16)
points(x=12883.77, y=2950000, col="red", pch=16)
points(x=10846.28, y=2949900, col="red", pch=16)
points(x=10348, y=1935000, col="red", pch=16)
points(x=15983, y=2685000, col="red", pch=16)
points(x=9180, y=1798000, col="red", pch=16)

z.full <- lm(Price ~ Lot.Size + Total.floor.area+Days.on.market+Age, data=mydata.train)

vif(z.full)

AIC(z.full)

z.step <- stepAIC(z.full, trace=FALSE) #trace=true will show you all the models it is fitting
summary(z.step)

mydata.2 <- mydata.train[c(5, 6,8,9)] # put columns 2, 3, and 4 in a dataset so that all the variables present are numeric
mydata.2

cor.matrix <- cor(mydata.2, method="pearson")
round(cor.matrix, 4)

cor.matrix.2 <- rcorr(as.matrix(mydata.2))
cor.matrix.2

# Add any transformations to the training dataset:
mydata.train$Age1<-(mydata.train$Age)+1
mydata.train$Days.on.market1<-(mydata.train$Days.on.market)+1
mydata.train$Price.log <- log(mydata.train$Price)
mydata.train$Total.floor.area.log <- log(mydata.train$Total.floor.area)
mydata.train$Lot.Size.log <- log(mydata.train$Lot.Size)
mydata.train$Age1.log <- log(mydata.train$Age1)
mydata.train$Days.on.market1.log <- log(mydata.train$Days.on.market1)

# If you conducted any transformations of x or y in your final model:
# Create scatterplots that visualize the association between the variables as they are used in your model:
# Is the form linear for each of these associations after the transformations?
plot(Price.log~Total.floor.area.log,data=mydata.train,main='Price vs Total floor area in Log units')
x1 <- mydata.train$Total.floor.area.log[order(mydata.train$Total.floor.area.log)] 
y1 <- mydata.train$Price.log[order(mydata.train$Total.floor.area.log)] 
lines(lowess(x1, y1, delta=1), col="red")
# it is non-linear

plot(Price.log~Lot.Size,data=mydata.train, main='Price (in log units) vs Lot Size (original)')
x2 <- mydata.train$Lot.Size[order(mydata.train$Lot.Size)] 
y2 <- mydata.train$Price.log[order(mydata.train$Lot.Size)] 
lines(lowess(x2, y2, delta=1), col="red")
#it is not linear

plot(Price.log~Age1,data=mydata.train, main='Price (in log units) vs Age (original)')
x3 <- mydata.train$Age1[order(mydata.train$Age1)] 
y3 <- mydata.train$Price.log[order(mydata.train$Age1)] 
lines(lowess(x3, y3, delta=1), col="red")
#it is not linear

plot(Price.log~Age1.log,data=mydata.train,main='Price vs Age (in log units)')
x4 <- mydata.train$Age1.log[order(mydata.train$Age1.log)] 
y4 <- mydata.train$Price.log[order(mydata.train$Age1.log)] 
lines(lowess(x4, y4, delta=1), col="red")
#it is not linear

plot(Price.log~Days.on.market1.log,data=mydata.train, main='Price vs Days on market (in log units)')
x5 <- mydata.train$Total.floor.area.log[order(mydata.train$Days.on.market1.log)] 
y5 <- mydata.train$Price.log[order(mydata.train$Days.on.market1.log)] 
lines(lowess(x5, y5, delta=1), col="red")
#it is not linear

# Fit your final model here, use summary to get model co-efficients and other useful output:
z.interac8 <- lm(Price.log ~ Total.floor.area.log + Lot.Size +Age1 +Age1.log +Days.on.market1.log+Lot.Size*Age1, data=mydata.train)
resid.interac8 <- resid(z.interac8)
predict.interac8 <- predict(z.interac8)
plot(resid.interac8 ~ predict.interac8, pch=16, main='Residual Plot')
abline(h=0, lty=2)
summary(z.interac8)

# Check assumptions of linearity, equal variance, normality here.
# Comment on any unusual observations (outliers) that are observed.


hist(resid.interac8)
shapiro.test(resid.interac8)

qqnorm(resid.interac8, ylab= "residuals", xlab = "Normal score", pch=16)
qqline(resid.interac8)

mydata.train4 <- cbind(mydata.train, predict.interac8)
mydata.train4

mydata.train4$predict.originals <- exp(mydata.train4$predict.interac8)
mydata.train4[, c(4,17)] 
SSY2 <- sum((mydata.train4$Price - mean(mydata.train4$Price))^2); SSY2  
SSE2 <- sum((mydata.train4$Price - mydata.train4$predict.originals)^2); SSE2
SSreg2 <- sum((mydata.train4$predict.originals - mean(mydata.train4$Price))^2); SSreg2 

pseudo.R4 <- 1 - SSE2/SSY2
pseudo.R4

nrow(mydata.train) 
rootMSE2 <- sqrt(SSE2/1035)  
rootMSE2 

AIC(z.interac8)

z.step8 <- stepAIC(z.interac8, trace=TRUE) #trace=true will show you all the models it is fitting
summary(z.step8)

PRESS(z.interac8)



cor.matrix <- cor(mydata.2, method="pearson")
round(cor.matrix, 4)

cor.matrix.2 <- rcorr(as.matrix(mydata.2))
cor.matrix.2

options(scipen=0)
summary(z.interac8)

drop1(z.interac8, test="F")

mydata.valid$Age1<-(mydata.valid$Age)+1
mydata.valid$Days.on.market1<-(mydata.valid$Days.on.market)+1
mydata.valid$Price.log <- log(mydata.valid$Price)
mydata.valid$Total.floor.area.log <- log(mydata.valid$Total.floor.area)
mydata.valid$Lot.Size.log <- log(mydata.valid$Lot.Size)
mydata.valid$Age1.log <- log(mydata.valid$Age1)
mydata.valid$Days.on.market1.log <- log(mydata.valid$Days.on.market1)
prediction789<-predict(z.interac8, newdata=mydata.valid)
predictionTrans2 <- exp(prediction789)

prediction2<-as.data.frame(cbind(mydata.valid,prediction789, predictionTrans2))
prediction2

prediction2$errors2 <- prediction2$Price - prediction2$predictionTrans2

RMSE2 <- sqrt(sum(prediction2$errors2^2)/nrow(prediction2)); RMSE2
MAE2 <- sum(abs(prediction2$errors2))/nrow(prediction2); MAE2



# Include the code for visualizing appropriate transformations of the variables here

Price.log = log(mydata.train$Price)
fullhouse = as.data.frame(Price.log)
fullhouse$Days.on.market.root = (mydata.train$Days.on.market)^(1/2)
fullhouse$Age.root = (mydata.train$Age)^(1/2)
fullhouse$Price.root = (mydata.train$Price)^(1/2)
fullhouse$Lot.Size.root = (mydata.train$Lot.Size)^(1/2)
fullhouse$Total.floor.area.sq = (mydata.train$Total.floor.area)^2
fullhouse$Days.on.market.sq = (mydata.train$Days.on.market)^2
fullhouse$Age.sq = (mydata.train$Age)^2
fullhouse$Lot.Size.sq = (mydata.train$Lot.Size)^2

plot(Price.log~Total.floor.area.sq,data=fullhouse, main = "Price.log vs total floor area square")
x6 <- fullhouse$Total.floor.area.sq[order(fullhouse$Total.floor.area.sq)] 
y6 <- fullhouse$Price.log[order(fullhouse$Total.floor.area.sq)] 
lines(lowess(x6, y6, delta=1), col="red")

plot(Price.log~Lot.Size.sq,data=fullhouse, main = "Price.log vs Lot size.sq")
x7 <- fullhouse$Lot.Size.sq[order(fullhouse$Lot.Size.sq)] 
y7 <- fullhouse$Price.log[order(fullhouse$Lot.Size.sq)] 
lines(lowess(x7, y7, delta=1), col="red")

plot(Price.log~Age.root,data=fullhouse, main = "Price.log vs Age.root")
x8 <- fullhouse$Age.root[order(fullhouse$Age.root)] 
y8 <- fullhouse$Price.log[order(fullhouse$Age.root)] 
lines(lowess(x8, y8, delta=1), col="red")

plot(Price.log~Days.on.market.root,data=fullhouse, main="Price.log vs Days on market.root")
x9 <- fullhouse$Days.on.market.root[order(fullhouse$Days.on.market.root)] 
y9 <- fullhouse$Price.log[order(fullhouse$Days.on.market.root)] 
lines(lowess(x9, y9, delta=1), col="red")

# Include the code for your other candidate models here.
Days.on.market.root = (mydata.train$Days.on.market)^(1/2)
mydata.trainA = as.data.frame(Days.on.market.root)
mydata.trainA$Age.root = (mydata.train$Age)^(1/2)
mydata.trainA$Price.log = log(mydata.train$Price)
mydata.trainA$Lot.Size.log = log(mydata.train$Lot.Size)
mydata.trainA$Total.floor.area.log = log(mydata.train$Total.floor.area)
mydata.trainA2 = cbind(mydata.trainA,mydata.train)



zA <- lm(Price.log ~ Days.on.market.root+Age.root+Total.floor.area.log +Lot.Size.log+Lot.Size.log*Age.root , data = mydata.trainA2)
summary(zA)

predictA = predict(zA)
residA = resid(zA)
predictA.orig=exp(predictA)
SSYA= sum((mydata.trainA2$Price - mean(mydata.trainA2$Price))^2)
SSYA
SSEA = sum((mydata.trainA2$Price - predictA.orig)^2)
SSEA
SSregA = sum((predictA.orig - mean(mydata.trainA2$Price))^2)
SSregA

hist(residA)
qqnorm(residA, ylab= "residuals", xlab = "Normal scores", pch=16)
qqline(residA)
plot(residA ~ predictA, pch=16, main='Residual Plot')
abline(h=0, lty=2)

pseudo.R2.A = 1 - SSEA/SSYA
pseudo.R2.A

rootMSE.A = sqrt(SSEA/1036)
rootMSE.A

AIC(zA)
PRESS(zA)

drop1(zA, test="F")

Days.on.market.root = (mydata.valid$Days.on.market)^(1/2)
mydata.validA = as.data.frame(Days.on.market.root)
mydata.validA$Age.root = (mydata.valid$Age)^(1/2)
mydata.validA$Price.log = log(mydata.valid$Price)
mydata.validA$Lot.Size.log = log(mydata.valid$Lot.Size)
mydata.validA$Total.floor.area.log = log(mydata.valid$Total.floor.area)
predictA.valid = predict(zA, newdata=mydata.validA)
mydata.validA2 = as.data.frame(cbind(mydata.validA, predictA.valid))
mydata.validA2$predictA.valid.orig = exp(mydata.validA2$predictA.valid)
mydata.validA2$Price = mydata.valid$Price


mydata.validA2$errors <- mydata.validA2$Price - mydata.validA2$predictA.valid.orig
RMSE.A <- sqrt(sum(mydata.validA2$errors^2)/nrow(mydata.validA2)); RMSE.A
MAE.A<- sum(abs(mydata.validA2$errors))/nrow(mydata.validA2); MAE.A


mydata.train$Age1<-(mydata.train$Age)+1
mydata.train$Days.on.market1<-(mydata.train$Days.on.market)+1
mydata.train$Price.log <- log(mydata.train$Price)
mydata.train$Total.floor.area.log <- log(mydata.train$Total.floor.area)
mydata.train$Lot.Size.log <- log(mydata.train$Lot.Size)
mydata.train$Age1.log <- log(mydata.train$Age1)
mydata.train$Days.on.market1.log <- log(mydata.train$Days.on.market1)


z.interac7 <- lm(Price.log ~ Total.floor.area.log + Lot.Size.log+Age1.log+Days.on.market1.log+Lot.Size*Age1, data=mydata.train)
resid.interac7 <- resid(z.interac7)
predict.interac7 <- predict(z.interac7)
plot(resid.interac7 ~ predict.interac7, pch=16, main='Residual Plot')
abline(h=0, lty=2)
summary(z.interac7)

hist(resid.interac7)
shapiro.test(resid.interac7)

qqnorm(resid.interac7, ylab= "residuals", xlab = "Normal scores", pch=16)
qqline(resid.interac7)

mydata.train3 <- cbind(mydata.train, predict.interac7)
mydata.train3

mydata.train3$predict.original <- exp(mydata.train3$predict.interac7)
mydata.train3[, c(4,17)] 
SSYB <- sum((mydata.train3$Price - mean(mydata.train3$Price))^2); SSYB 
SSEB <- sum((mydata.train3$Price - mydata.train3$predict.original)^2); SSEB
SSregB <- sum((mydata.train3$predict.original - mean(mydata.train3$Price))^2); SSregB

pseudo.R.B <- 1 - SSEB/SSYB 
pseudo.R.B

nrow(mydata.train) 
rootMSEB <- sqrt(SSEB/1036)  
rootMSEB

AIC(z.interac7)

PRESS(z.interac7)

mydata.valid$Age1<-(mydata.valid$Age)+1
mydata.valid$Days.on.market1<-(mydata.valid$Days.on.market)+1
mydata.valid$Price.log <- log(mydata.valid$Price)
mydata.valid$Total.floor.area.log <- log(mydata.valid$Total.floor.area)
mydata.valid$Lot.Size.log <- log(mydata.valid$Lot.Size)
mydata.valid$Age1.log <- log(mydata.valid$Age1)
mydata.valid$Days.on.market1.log <- log(mydata.valid$Days.on.market1)
prediction456<-predict(z.interac7, newdata=mydata.valid)
predictionTrans1 <- exp(prediction456)

prediction1<-as.data.frame(cbind(mydata.valid,prediction456, predictionTrans1))
prediction1

prediction1$errors1 <- prediction1$Price - prediction1$predictionTrans1

RMSE1 <- sqrt(sum(prediction1$errors1^2)/nrow(prediction1)); RMSE1
MAE1 <- sum(abs(prediction1$errors1))/nrow(prediction1); MAE1




