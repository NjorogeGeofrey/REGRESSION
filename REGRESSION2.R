
Rscript

###Install packages####
library(car)
library(carData)
library(ggplot2)
library(lm.beta)
library(lme4)
library(lmtest)
library(olsrr)
library(readxl)
library(pastecs)
library(Hmisc)
install.packages("Rmisc")
library(Rmisc)

###Creating a dataset####

sk<-skinfold_bodyfat_Assignment_DATASET_2_


###Visual Representation of the data####
##For Age###

hist(sk$Age)
boxplot(sk$Age)
qqnorm(sk$Age)
qqline(sk$Age)
##For Triceps##
hist(sk$Triceps)
boxplot(sk$Triceps)
qqnorm(sk$Triceps)
qqline(sk$Triceps)
##For Biceps##
hist(sk$Biceps)
boxplot(sk$Biceps)
qqnorm(sk$Biceps)
qqline(sk$Biceps)
##For Subscapular##
hist(sk$Subscapular)
boxplot(sk$Subscapular)
qqnorm(sk$Subscapular)
qqline(sk$Subscapular)
##For Suprailiac##
hist(sk$Suprailiac)
boxplot(sk$Suprailiac)
qqnorm(sk$Suprailiac)
qqline(sk$Suprailiac)
##For BodyFat###
hist(sk$BodyFat)
boxplot(sk$BodyFat)
qqnorm(sk$BodyFat)
qqline(sk$BodyFat)

###Running Shapiro Wilk Test###
shapiro.test(sk$Age)###not normally distributed p=2.791e-06
shapiro.test(sk$Triceps)### not normally distributed
shapiro.test(sk$Biceps)### not normally distributed
shapiro.test(sk$Subscapular)#### not normally distributed
shapiro.test(sk$Suprailiac)#### not normally distributed
shapiro.test(sk$BodyFat)####not normally distributed
shapiro_test_Male <- shapiro.test(sk$BodyFat[sk$Gender == '0'])
print(shapiro_test_Male)###Normally Distributed

shapiro_test_Female<-shapiro.test(sk$BodyFat[sk$Gender== '1'])
print(shapiro_test_Female)###Normally Distributed


### Descriptive Statitics####
round(stat.desc(sk$Age, basic = F, norm = T),2)
round(stat.desc(sk$Triceps, basic = F, norm = T),2)
round(stat.desc(sk$Biceps, basic = F, norm = T),2)
round(stat.desc(sk$Subscapular, basic = F, norm = T),2)
round(stat.desc(sk$Suprailiac, basic = F, norm = T),2)
round(stat.desc(sk$BodyFat,basic = F, norm = T),2)
round(stat.desc(sk$BodyFat[sk$'Gender' == '0'], basic = F, norm = T),2)
round(stat.desc(sk$BodyFat[sk$'Gender' == '1'], basic = F, norm = T),2)
###Confindence Interval for the predictor variables###
CI(sk$Age, ci=0.95)
CI(sk$Triceps, ci=0.95)
CI(sk$Subscapular, ci=0.95)
CI(sk$Suprailiac, ci=0.95)
CI(sk$BodyFat, ci= 0.95)
CI(sk$BodyFat[sk$Gender == '0'], ci=0.95)

####Correlation between dependent and independent variables###
cor(sk) 
CMat <-cor(sk)
CMat3 <-round(CMat,3)
CMat3

### Calculating correlataion Coefficient ####
cor.test(sk$Age,sk$BodyFat )
cor.test(sk$Triceps,sk$BodyFat )
cor.test(sk$Biceps,sk$BodyFat )###no correlation 
cor.test(sk$Subscapular,sk$BodyFat )
cor.test(sk$Suprailiac,sk$BodyFat )
cor.test(sk$Gender,sk$BodyFat) ### mo correlation

# Regression on these variables for a first model and to begin to check assumptions
# All variables included - called the full  model

skmodel1 <- (lm(sk$BodyFat ~ Age +Gender+Triceps+Biceps+Subscapular+Suprailiac, data = sk))
skmodel1
summary(skmodel1)


###New model with signficant variable###
skmodel2 <- (lm(sk$BodyFat ~ Age +Triceps+Subscapular+Suprailiac, data = sk))
skmodel2
summary(skmodel2)

###Assumption 1 linerarity####
plot(skmodel2)



##Assumption 3 Durbin Watson test####
dwtest(skmodel2)

##Asssumption 4 normality of the error term####
shapiro.test(resid(skmodel2))
hist(resid(skmodel2))

###Assumption 5#### Outlier

plot(skmodel2,5)

###Assumption 6### Leverage
plot(skmodel2,5)
3*(5+1)/100 # specific value on the x axis

###Homoskedicity
bptest(skmodel2)


###Assumption 7: Influnce####
sort(round(cooks.distance(skmodel2),3))
plot(cooks.distance(skmodel2),type="h")


cooks_values<-cooks.distance(skmodel2)
plot(cooks_values,
     type="h",
     main=" Cooks Distance",
     ylab= "Cooks Distance",
     xlab= "Observation Index")

abline(h=4/length(cooks_values), col="red")

high_cooks<- which(cooks_values>(4/length(cooks_values)))
print(high_cooks)

###Check for Multicollinearity###
car::vif(skmodel2)### no multicollinearity
##Assumption 8####
ols_plot_diagnostics(skmodel2)

ols_vif_tol(skmodel2)

# Perform the Durbin-Watson test
durbinWatsonTest <- dwtest(skmodel2)

# Print the result for the durbin-Watson
durbinWatsonTest




