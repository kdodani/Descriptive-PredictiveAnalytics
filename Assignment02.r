cdc.data <- read.csv(file="Fruits_veg_and_smoking.csv", header=TRUE)

print(cdc.data) 

plot(fruits.veg ~ smoking, data=cdc.data, pch=16)

plot(fruits.veg ~ smoking, data=cdc.data, xlab="% of people who smoke every day",ylab="% of people who eat at least 5 servings of fruits and vegetables every day",main='Scatter Plot',pch=16)
x1 <- cdc.data$smoking[order(cdc.data$smoking)] #put the x-values in order
y1 <- cdc.data$fruits.veg[order(cdc.data$smoking)] # put the y-values in order based on the order of the x-values
lines(lowess(x1, y1, delta=1), col="red")

#Q1 Fit a linear model for Fruits_veg_and_smoking.csv
z.cdc <- lm(fruits.veg ~ smoking, data = cdc.data)
summary(z.cdc)

#Q1 Plot regression line
plot(fruits.veg ~ smoking, data=cdc.data, pch=16)
abline(z.cdc)

#Q1 Confidence bands
xnew <- seq(min(cdc.data$smoking), max(cdc.data$smoking), length.out = 100)
xnew #creating new x values, giving lower (min x value) and upper limit (max x value) that gives a 100 x values that are equally spaced out
ynew.ci <- data.frame(predict(z.cdc, newdata = data.frame(smoking = xnew), interval = "confidence", level = 0.95))
ynew.ci #using predict funtion for new x values, newdata is a kw has to be there, convert to data frame so that r sees it that way, choose a type of interval ..becasue we want confidence band choose confidence so that r knows what formula to use, and level is our confidence level

#Q1 Create a plot of the data including the regression line and the confidence bands for Fruits_veg_and_smoking.csv
plot(fruits.veg ~ smoking, data=cdc.data, pch=16,xlab="% of people who smoke every day",ylab="% of people who eat at least 5 servings of fruits and vegetables every day",main='Confidence Bands')
abline(z.cdc) #fitted values are already taken care of
lines(ynew.ci$lwr ~ xnew, lty = 2, col="red") #lwr is y values and then plotting x values that go with y
lines(ynew.ci$upr ~ xnew, lty = 2, col="red")



resid1 <- resid(z.cdc)
predict1 <- predict(z.cdc)

mydata2 <- cbind(cdc.data, predict1,resid1)
mydata2

plot(resid1 ~ predict1, pch=16)
abline(0,0, lty=2)

plot(resid1 ~ cdc.data$smoking, pch=16)
abline(0,0, lty=2)

#Q3 What is the standard error for predicted value (District of Columbia)

predictcolumb=24.71
xcolumb=13.4

#SSEcolumb <- ((mean(cdc.data$fruits.veg)) - predictcolumb)^2; SSEcolumb
b0 <- z.cdc$coefficients[1]
b1 <- z.cdc$coefficients[2]
pred.y <- b1*xcolumb + b0
y.fitted <- z.cdc$fitted.values
sse <- sum((cdc.data$fruits.veg - y.fitted)^2)
mse <- sse/(50-2)
#critical value
t.val <- qt(0.975, 50-2)

mean.se <- (1 / 50 + (xcolumb - mean(cdc.data$smoking))^2 / 
                (sum((cdc.data$smoking - mean(cdc.data$smoking))^2)))
pred.se <- (1 + (1 / 50) + (xcolumb - mean(cdc.data$smoking))^2 / 
            (sum((cdc.data$smoking - mean(cdc.data$smoking))^2))) 
pred.se

pred.conf.upper <- pred.y + t.val * sqrt(mse * pred.se)
pred.conf.lower <- pred.y - t.val * sqrt(mse * pred.se)
pred.conf.upper
pred.conf.lower

mydata2<-rbind(mydata2,c('Columbia',' ',xcolumb,predictcolumb,' '))
mydata2

#Q6 plot of the data including the regression line and the prediction intervals
xnew <- seq(min(cdc.data$smoking), max(cdc.data$smoking), length.out = 100)
ynew.pred <- data.frame(predict(z.cdc, newdata = data.frame(smoking = xnew), interval = "prediction", level = 0.95))
ynew.pred
plot(fruits.veg ~ smoking, data=cdc.data, pch=16,xlab="% of people who smoke every day",ylab="% of people who eat at least 5 servings of fruits and vegetables every day",main='Prediction Intervals')
abline(z.cdc)
lines(ynew.pred$lwr ~ xnew, lty = 2, col="blue")
lines(ynew.pred$upr ~ xnew, lty = 2, col="blue")

next.data <- read.csv(file="/Users/khushdodani/Desktop/BABS507_Assignment02/Company_employment_and_sales.csv", header=TRUE)
next.data

plot(sales ~ employees, data=next.data, pch=16)

#Q8 
plot(sales ~ employees, data=next.data, xlab="# of employees",ylab="sales in millions of $",main='Scatter Plot',pch=16)
x1 <- next.data$employees[order(next.data$employees)] #put the x-values in order
y1 <- next.data$sales[order(next.data$employees)] # put the y-values in order based on the order of the x-values
lines(lowess(x1, y1, delta=1), col="red")

z1 <- lm(sales ~ employees, data = next.data)

resid1 <- resid(z1)
predict1 <- predict(z1)

plot(resid1 ~ predict1, pch=16, main='Residual Plot')
abline(0, 0, lty=2)

#Q11
next.data$sales.log <- log(next.data$sales)
next.data$employees.log <- log(next.data$employees)

str(next.data)

#Q12
plot(sales.log ~ employees.log, data = next.data, pch = 16, xlab='# of employees', ylab='sales in millions of $',main='Sales vs # of Employees Natural Log Plot')
x2 <- next.data$employees.log[order(next.data$employees.log)] #put the x-values in order
y2 <- next.data$sales.log[order(next.data$employees.log)] # put the y-values in order based on the order of the x-values
lines(lowess(x2, y2, delta=100), col="red")

z2 <- lm(sales.log ~ employees.log, data = next.data)#working with transformed units

resid2 <- resid(z2)
predict2 <- predict(z2)

#Q14
plot(resid2 ~ predict2, pch=16)
abline(0, 0, lty=2)

#Q14
plot(resid2 ~ employees.log, data = next.data, pch = 16, main='residual plot') 
abline(0,0,lty=2)

#Q16
hist(resid2,breaks=5,main='Histogram of Residuals')

#Q16
hist(resid2,breaks=16,main='Histogram of Residuals')

#Q16
hist(resid2,breaks=24,main='Histogram of Residuals')

#Q16
qqnorm(resid2, ylab= "residuals", xlab = "normal scores", pch=16)
qqline(resid2)

#Q17
shapiro.test(resid2)

#Q17, Q18, Q20
summary(z2)

#Q21
qf(0.99,1,38)

#Q22
anova(z2)

#Q23
mydata3 <- cbind(next.data, predict2)
mydata3

#Q23
mydata3$predict.orig <- exp(mydata3$predict2)
SSY <- sum((mydata3$sales - mean(mydata3$sales))^2); SSY
SSE <- sum((mydata3$sales - mydata3$predict.orig)^2); SSE
SSreg <- sum((mydata3$predict.orig - mean(mydata3$sales))^2); SSreg

#Q23
pseudo.R2 <- 1 - SSE/SSY # Always use this formula when calculating the Pseudo-R2
pseudo.R2

#Q24 
residualse=exp(0.6941) #from summary table
residualse

#Q25
qt(0.995,38)

#Q29
xnew2 <- seq(min(next.data$employees.log), max(next.data$employees.log), length.out = 100)
xnew2 

ynew.pi <- data.frame(predict(z2, newdata = data.frame(employees.log = xnew2), interval = "prediction", level = 0.99))
ynew.pi

#Q29
new.values2 <- cbind(xnew2,ynew.pi)
print(new.values2)

#Q29
plot(sales.log ~ employees.log, data = next.data, pch = 16, main='prediction intervals')
abline(z2)
lines(ynew.pi$lwr ~ xnew2, lty = 2)
lines(ynew.pi$upr ~ xnew2, lty = 2)

#Q30you dont have to backtransform here since original values are not in log form
new.values2
new.values2$fit.back <- exp(new.values2$fit) 
new.values2$lwr.back <- exp(new.values2$lwr)
new.values2$upr.back <- exp(new.values2$upr)
new.values2$xnew2 <- exp(new.values2$xnew2)
new.values2

#Q30
plot(sales ~ employees, data = next.data, pch = 16, main='prediction intervals in original units')
lines(new.values2$fit.back ~ new.values2$xnew2, lty=1)
lines(new.values2$lwr.back ~ new.values2$xnew2, lty = 2)
lines(new.values2$upr.back ~ new.values2$xnew2, lty = 2)
