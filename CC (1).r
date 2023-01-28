##Install all the necessary libraries/packages
install.packages("psych")
install.packages("corrplot")
install.packages("dplyr")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("devtools")
install.packages("readxl")
install.packages("ca")

install.packages("FactoMineR")

library(FactoMineR)
library(ca)
library(psych)
library(corrplot)
library("psych")
library(ggplot2)
library(car)
library(dplyr)
library("readxl")
library(devtools)
require(ggbiplot)
library(MASS)
library(magrittr)
library(ggpubr)


library(readxl)
data=read.csv("C:/Users/Lenovo/Downloads/Data file.csv")
lm.fit=lm(sales~TV+radio+newspaper, data=data)
summary(lm.fit)
residual=resid(lm.fit)
plot(fitted(lm.fit),residual, main="Residual Plot")
lm.fit=lm(sales~TV, data=data)
summary(lm.fit)
residual=resid(lm.fit)
lm.fit=lm(sales~newspaper, data=data)
summary(lm.fit)
plot(fitted(lm.fit),residual, main="Residual Plot")
residual=resid(lm.fit)
lm.fit=lm(sales~radio, data=data)
summary(lm.fit)
residual=resid(lm.fit)
plot(fitted(lm.fit),residual, main="Residual Plot")

intercept_only <- lm(sales ~ 1, data=data)
all <- lm(sales ~ .-predictions-error, data=data)
forward <- step(intercept_only, direction='forward', scope=formula(all), trace=0)
forward$anova
forward$coefficients


intercept_only <- lm(sales ~ 1, data=data)
all <- lm(sales ~ .-predictions-error, data=data)
forward <- step(intercept_only, direction='both', scope=formula(all), trace=0)
forward$anova
forward$coefficients

lm.fit=lm(sales~radio+TV, data=data)
summary(lm.fit)
residual=resid(lm.fit)
plot(fitted(lm.fit),residual, main="Residual Plot")

data=data[,1:7]
init_mod <- lm(sales ~ ., data = data)
step(init_mod, scope = . ~ .^2, direction = 'both')
fit=lm(formula = sales ~ TV + radio + newspaper + Area_suburban + 
     Area_urban + TV:radio + TV:newspaper, data = data)
summary(fit)
residual=resid(fit)
plot(fitted(fit),residual, main="Residual Plot")

car::vif(fit)
fit=lm(formula = sales ~ TV + radio + newspaper + Area_suburban + 
         Area_urban+ TV:newspaper, data = data)
summary(fit)
residual=resid(fit)
plot(fitted(fit),residual, main="Residual Plot")

fit=lm(formula = sales ~ TV + radio + newspaper + TV:radio + TV:newspaper, 
   data = data)
summary(fit)
residual=resid(fit)
plot(fitted(fit),residual, main="Residual Plot")
car::vif(fit)

fit=lm(formula = sales ~ TV + radio + TV:radio , 
       data = data)
summary(fit)
residual=resid(fit)
plot(fitted(fit),residual, main="Residual Plot")
car::vif(fit)


fit=lm(formula = sales ~ TV + radio + TV:radio +TV:Area_suburban, 
       data = data)
summary(fit)
residual=resid(fit)
plot(fitted(fit),residual, main="Residual Plot")
car::vif(fit)

qqPlot(fit,labels=row.names(City), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")
outlierTest(fit)

#Observations 131 and 156 are the outliers which were also visible in the box plot

#Remove the outliers and reload the data
