library(MASS)
library(ISLR)

#install.packages("ISLR")
data("Boston")

#print head
head(Boston)

#rows for dataset
nrow(Boston)

summary(Boston)

set.seed(2)
library(caTools)

#split using 70 percent
split<-sample.split(Boston$medv ,SplitRatio = 0.7)
split

training_data<-subset(Boston,split=="TRUE")
testing_data<-subset(Boston,split=="FALSE")


###Exploratory Data Analysis###

#creating scatterplot matrix
attach(Boston)
library(lattice)
splom(~Boston[c(1:6,14)], groups=NULL, data=Boston,axis.line.tck = 0,axis.text.aplha = 0)
splom(~Boston[c(7:14)], groups=NULL, data=Boston,axis.line.tck = 0,axis.text.aplha = 0)

#corplot to visualize
#install.packages("corrplot", dependencies = 1)
library(corrplot)
cr<-cor(Boston)
corrplot(cr, type = "lower")
corrplot(cr, method = "number")


#to view corelation of variables
plot(Boston$crim ,Boston$medv, cex = 0.5, xlab = "CrimeRate", ylab = "Price")
cr<-cor(Boston)
pairs(~ medv + ptratio + black + lstat + dis + rm + crim, data = Boston, main = "Boston Data")

## crim is not acceptable to be a linear variable


#studying crim and medv
plot(crim,medv)
fit1<-lm(medv~crim, data=Boston)
abline(fit1, col="red")

# regression fit line

#studying rm and medv
plot(rm,medv)
fit1<-lm(medv~rm, data=Boston)
abline(fit1, col="red")
# regression fit line

#studying lstat and medv
plot(lstat,medv)
fit1<-lm(medv~lstat, data=Boston)
abline(fit1, col="red")

# regression fit line

##Creating Model


#Since line is acceptable through rm and lstat variable we use rm, lstat to model to predict data
#Using rm, lstat as they are good linear variables.

#Rm
model_regx_rm<-lm(medv~rm,data = training_data)
#summary
summary(model_regx_rm)
#prediction
predic_rm<-predict(model_regx_rm, testing_data)
predic_rm
#compare actual values and prediction
plot(testing_data$medv, type = "l", lty = 1.8, col = "green")
lines(predic_rm,type = "l", col = "blue")



#lstat
model_regx_lstat<-lm(medv~lstat,data = training_data)
#summary
summary(model_regx_lstat)
#prediction
predic_lstat<-predict(model_regx_lstat, testing_data)
predic_lstat
#compare actual values and prediction 
plot(testing_data$medv, type = "l", lty = 1.8, col = "green")
lines(predic_lstat,type = "l", col = "blue")



# finding root mean sq. error
rmse<-sqrt(mean(predic_rm-testing_data$medv)^2)
rmse
rmse<-sqrt(mean(predic_lstat-testing_data$medv)^2)
rmse






#### NoW we try multi linear regression ####

#selecting only variables
model_regx_ml<-lm(medv~ rm + lstat,data = Boston)
#summary
summary(model_regx_ml)


#selecting all variables
model_regx_all<-lm(medv~.,data = training_data)
#summary
summary(model_regx_all)


#removing age and indus
model_regx_selected<-lm(medv~ crim + zn + tax + chas + rm + rad + dis + nox + 
                 ptratio + black + lstat,data = training_data)
#summary
summary(model_regx_selected)

#prediction
predic_selected<-predict(model_regx_selected, testing_data)
predic_selected


# finding root mean sq. error
rmse<-sqrt(mean(predic_selected-testing_data$medv)^2)
rmse

#compare actual values and prediction 
plot(testing_data$medv, type = "l", lty = 1.8, col = "green")
lines(predic_selected,type = "l", col = "blue")



#since rmse value is still high we need to optimize the model

f1=lm(medv~lstat +I(lstat^2),Boston)
summary(fit1)
attach(Boston)
f11=lm(medv~poly(lstat,4))
plot(medv~lstat)
points(lstat,fitted(f11),col="blue",pch=20)

f2=lm(medv~rm +I(rm^2),Boston)
summary(f2)
attach(Boston)
fit22=lm(medv~poly(rm,4))
plot(medv~rm)
points(rm,fitted(fit22),col="blue",pch=20)


#building final model
fit_final=lm(medv~lstat+crim+rm+dis+black+chas+nox+rad+tax+ptratio+I(lstat^2)+I(rm^2))
summary(fit_final)

#prediction
predic_fit_final<-predict(fit_final, testing_data)
predic_fit_final

# finding root mean sq. error
rmse<-sqrt(mean(predic_fit_final-testing_data$medv)^2)
rmse

#compare actual values and prediction 
plot(testing_data$medv, type = "l", lty = 1.8, col = "green")
lines(predic_fit_final,type = "l", col = "blue")

