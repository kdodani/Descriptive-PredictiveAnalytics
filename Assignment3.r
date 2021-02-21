#Q1 Graph the relationship between yield and temperature
mydata<-read.csv(file="Yield_and_temperature.csv", header=TRUE)
str(mydata)
max=max(mydata$temperature)
min=min(mydata$temperature)
plot(yield~temperature, data=mydata,xlab="Temperature",ylab="Yield",main='Scatter Plot',pch=16,
    xlim=c(min,max))
x1 <- mydata$temperature[order(mydata$temperature)] 
y1 <- mydata$yield[order(mydata$temperature)] 
lines(lowess(x1, y1, delta=1), col="red")

#Q2 Fit a quadratic model
mydata$temperature.sq <- (mydata$temperature)^2
mydata

z1 <- lm(yield ~ temperature + temperature.sq, data=mydata) 
resid1 <- resid(z1)
predict1 <- predict(z1)
plot(resid1 ~ predict1, pch=16, main='Residual Plot')
abline(h=0, lty=2)
hist(resid1,breaks=5,main='Histogram of Residuals', xlim=c(-5,5))
qqnorm(resid1, ylab= "residuals", xlab = "Normal scores", pch=16, xlim=c(-2,2), ylim=c(-4,3))
qqline(resid1)

shapiro.test(resid1)
summary(z1)

predicted yield = 12.1 + 2.391 * temperature + 0.000650 *temperature.sq

#Q4 Anova table
anova(z1)

#Q5 What is the F critical value for this test
qf(0.95,2,8)

#Q8 t critical 
qt(0.975,8)

#Q9,10 95% confidece interval for b1,b2
options(scipen=999)
confint(z1,level=0.95)

0.000890606

temperature <- seq(min(mydata$temperature), max(mydata$temperature), length.out = 100) 
xnew <- as.data.frame(temperature)
xnew$temperature.sq <- (xnew$temperature)^2 
xnew

ynew.pred <- data.frame(predict(z1, newdata =  xnew, interval = "prediction", level = 0.95)) 
ynew.pred



#Q14 Create a plot of the original data (yield vs. temperature) including the model fit as a solid line and dashed lines for the prediction intervals.
plot(yield ~ temperature, data = mydata, main='Plot with original data and prediction intervals', xlab='Temperature',ylab='Yield',
    xlim=c(580, 870), ylim=c(120, 160))
lines(ynew.pred$fit ~ xnew$temperature) # since the model fit in the original units is not linear, we need to use lines() to plot it rather than abline().
lines(ynew.pred$lwr ~ xnew$temperature, lty = 2)
lines(ynew.pred$upr ~ xnew$temperature, lty = 2)

#Q13
a <- data.frame(predict(z1, data.frame(temperature=(724.54), temperature.sq=(724.54)^2), interval = "prediction", level = 0.95)) 
a

#Part 2 Data= Tree_Data

#Q15 
mydatatree<-read.csv(file="/Users/khushdodani/Desktop/BABS07_Assignment3/Tree_Data.csv", header=TRUE)
str(mydatatree)
plot(volha~topht, data=mydatatree,xlab="Average height of trees (m)",ylab="Volume of timber per hectare (m3 / ha)",main='Figure A: volha vs. topht',pch=16)
x2 <- mydatatree$topht[order(mydatatree$topht)] 
y2 <- mydatatree$volha[order(mydatatree$topht)] 
lines(lowess(x2, y2, delta=1), col="red")


z2 <- lm(volha ~ topht, data = mydatatree)

resid2 <- resid(z2)
predict2 <- predict(z2)
plot(resid2 ~ predict2, pch=16)
abline(0, 0, lty=2)



plot(volha~dbh, data=mydatatree,xlab="Average diameter at breast height (cm)",ylab="Volume of timber per hectare (m3 / ha)",main='Figure B: volha vs. dbh',pch=16)
x3 <- mydatatree$dbh[order(mydatatree$dbh)] 
y3 <- mydatatree$volha[order(mydatatree$dbh)] 
lines(lowess(x3, y3, delta=1), col="red")


z3 <- lm(volha ~ dbh, data = mydatatree)

resid3 <- resid(z3)
predict3 <- predict(z3)
plot(resid3 ~ predict3, pch=16)
abline(0, 0, lty=2)
hist(resid3, xlim=c(-600, 600))
qqnorm(resid3, ylab= "residuals", xlab = "Normal scores", pch=16)
qqline(resid3)

plot(volha~stemsha, data=mydatatree,xlab="Stems per hectare (number of trees per hectare)",ylab="Volume of timber per hectare (m3 / ha)",main='Figure C: volha vs. stemsha',pch=16)
x4 <- mydatatree$stemsha[order(mydatatree$stemsha)] 
y4 <- mydatatree$volha[order(mydatatree$stemsha)] 
lines(lowess(x4, y4, delta=1), col="red")



z4 <- lm(volha ~ stemsha, data = mydatatree)

resid4 <- resid(z4)
predict4 <- predict(z4)
plot(resid4 ~ predict4, pch=16)
abline(0, 0, lty=2)

plot(volha~baha, data=mydatatree,xlab="Basal area per hectare (m2 / ha)",ylab="Volume of timber per hectare (m3 / ha)",main='Figure D: volha vs. baha',pch=16)
x5 <- mydatatree$baha[order(mydatatree$baha)] 
y5 <- mydatatree$volha[order(mydatatree$baha)] 
lines(lowess(x5, y5, delta=1), col="red")


z5 <- lm(volha ~ baha, data = mydatatree)

resid5 <- resid(z5)
predict5 <- predict(z5)
plot(resid5 ~ predict5, pch=16)
abline(0, 0, lty=2)


plot(volha~age, data=mydatatree,xlab="Average age of trees",ylab="Volume of timber per hectare (m3 / ha)",main='Figure E: volha vs. age',pch=16)
x6 <- mydatatree$age[order(mydatatree$age)] 
y6 <- mydatatree$volha[order(mydatatree$age)] 
lines(lowess(x6, y6, delta=1), col="red")


z6 <- lm(volha ~ age, data = mydatatree)

resid6 <- resid(z6)
predict6 <- predict(z6)
plot(resid6 ~ predict6, pch=16)
abline(0, 0, lty=2)

#Q17-23
z.bt <- lm(volha ~ baha + topht + baha*topht, data=mydatatree)
predict.bt <- predict(z.bt)
resid.bt <- resid(z.bt)
plot(resid.bt ~ predict.bt, pch=16)
abline(0,0, lty=2)

hist(resid.bt, xlim=c(-50, 50))

qqnorm(resid.bt, ylab= "standardized residuals", xlab = "Normal scores")
qqline(resid.bt)

shapiro.test(resid.bt)

summary(z.bt)

#partial f test, dropping the interaction to test its significance
drop1(z.bt, test="F")

#23
#predicted volume = -36.1176 + (-2.1824*baha) + (2.5438*topht) + (0.8278*baha*topht)
#area A = -36.1176 + (-2.1824*30) + (2.5438*24) + (0.8278*30*24) = 
areaa<- -36.1176 + (-2.1824*30) + (2.5438*24) + (0.8278*30*24)
#areaa
#area B = -36.1176 + (-2.1824*37) + (2.5438*20) + (0.8278*37*20)
areab<- -36.1176 + (-2.1824*37) + (2.5438*20) + (0.8278*37*20)
#areab
a <- data.frame(predict(z.bt, data.frame(baha=c(30), topht=c(24)), interval = "prediction", level = 0.95)) 
#a
b <- data.frame(predict(z.bt, data.frame(baha.log=c(37), topht.log=c(20)), 
                                         interval = "prediction", level = 0.95))
#b

#24-38
mydatatree$baha.log <- log(mydatatree$baha)
mydatatree$topht.log <- log(mydatatree$topht)
mydatatree$volha.log <- log(mydatatree$volha)
z.linear <- lm(volha.log ~ baha.log + topht.log, data=mydatatree)
summary(z.linear)


drop1(z.linear,test="F")

#checking above table using different formula
#z.tophtlog<-lm(volha.log~topht.log,data=mydatatree)
#anova(z.linear,z.tophtlog)

#Q36
confint(z.linear,level=0.95)

AIC(z.linear)

AIC(z.bt)

#Q34
z.linear.resid<-resid(z.linear)
z.linear.predict<-predict(z.linear)

mydatatree.new <- cbind(mydatatree, z.linear.predict)
mydatatree.new

mydatatree.new$predict.orig <- exp(mydatatree.new$z.linear.predict)  


SSY <- sum((mydatatree.new$volha - mean(mydatatree.new$volha))^2); SSY  
SSE <- sum((mydatatree.new$volha - mydatatree.new$predict.orig)^2); SSE
SSreg <- sum((mydatatree.new$predict.orig - mean(mydatatree.new$volha))^2); SSreg

pseudo.R2 <- 1 - SSE/SSY # Always use this formula when calculating the Pseudo-R2
pseudo.R2

nrow(mydatatree)


rootMSE <- sqrt(SSE/24)  #root mean square error in original units
rootMSE

c <- data.frame(predict(z.linear, data.frame(baha.log=log(30), topht.log=log(24)), interval = "prediction", level = 0.95)) 
c
d <- data.frame(predict(z.linear, data.frame(baha.log=log(37), topht.log=log(20)), 
                                         interval = "prediction", level = 0.95))
d
exp(d$upr)


