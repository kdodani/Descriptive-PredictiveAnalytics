cdc.data <- read.csv(file="Fruits_veg_and_smoking.csv", header=TRUE)

head(cdc.data)


#Q1 plot a scatterplot of % of people who eat enough fruits and vegetables vs. % of people who smoke every day
plot(fruits.veg ~ smoking, data=cdc.data, pch=16)

#Q1 Fit a smoothing curve to the plot and give detailed axis labels
plot(fruits.veg ~ smoking, data=cdc.data, xlab="% of people who smoke every day",ylab="% of people who eat at least 5 servings of fruits and vegetables every day",main='Scatter Plot',pch=16)
x1 <- cdc.data$smoking[order(cdc.data$smoking)] #put the x-values in order
y1 <- cdc.data$fruits.veg[order(cdc.data$smoking)] # put the y-values in order based on the order of the x-values
lines(lowess(x1, y1, delta=1), col="red")
#abline(lm(fruits.veg ~ smoking, data=cdc.data))

#Q5 Create a linear model to fit this data
z.cdc <- lm(fruits.veg ~ smoking, data = cdc.data)
summary(z.cdc)

#Q5 Extract predicted values
predict.cdc<-predict(z.cdc)
predict.cdc

#Q5 Extract the residuals
resid.cdc <- resid(z.cdc); resid.cdc

cdc.data.model<-cbind(cdc.data,predict.cdc,resid.cdc)
cdc.data.model

#Q5 Residuals vs explanatory variable smoking 
plot(resid.cdc ~ cdc.data$smoking, xlab='% of people smoking every day', ylab='Residuals',main='Residual Plot',pch=16)
abline(0,0, lty=2)

#Q5 Residuals vs predicted values
plot(resid.cdc ~ predict.cdc, xlab='Predicted values', ylab='Residuals', main='Residual Plot', pch=16)
abline(0,0, lty=2)

#Q8 histogram of residuals
hist(resid.cdc,breaks=16,main='Histogram of Residuals')

#Q9 Normal probability plot of residuals 
qqnorm(resid.cdc, ylab= "Residuals", xlab = "Normal scores", pch=16, main='Normal Probability Plot of Residuals')
qqline(resid.cdc)

#Q10 Normality test
shapiro.test(resid.cdc)

#Q13, 15, 16, 17, 18, 19
summary(z.cdc)

#Q20,21 95% confidence interval 
confint(z.cdc)




