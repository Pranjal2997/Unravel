##Install all the necessary libraries/packages
install.packages("psych")
install.packages("corrplot")
install.packages("dplyr")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("devtools")
install.packages("readxl")


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
df=read.csv("D:/Case Comps/Unravel 3/Data file.csv")
lm.fit=lm(sales~TV, data=df)
summary(lm.fit)
lm.fit=lm(sales~radio, data=df)
summary(lm.fit)
#for suburban region
r=read.csv("C:/Users/Lenovo/OneDrive/Documents/suburban.csv")
lm.fit1=lm(sales~TV+radio+newspaper, data=r)
summary(lm.fit1)
##For optimization of budget
install.packages("QuantPsyc")
library("QuantPsyc")
lm.beta(lm.fit1)
##regression diagnostics
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


###Model Selection
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
###Interaction terms
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
###Final Model
fit1=lm(formula = sales ~ TV + radio + TV:radio ,
       data = data)
summary(fit1)
residual=resid(fit1)
plot(fitted(fit),residual, main="Residual Plot")
car::vif(fit)

##Sales forecasting
new <- data.frame(TV=c(250), radio=c(50), Area_suburban=c(0), Area_urban=c(1))
predict(fit1, newdata=new)


fit=lm(formula = sales ~ TV + radio + TV:radio, 
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

###Elasticity / ROI by log-log regression
data1<-log(df)
tail(data1)

data2 <-  data1%>% dplyr::select(TV, radio, newspaper, sales)
data2
str(data2)

##remove 0-1 values for radio
log_reg <- lm(formula=sales ~ radio, data = data2)
summary(log_reg)

log_reg_TV <- lm(formula=sales ~ TV, data = data1)
summary(log_reg_TV)

log_reg_newspaper <- lm(formula=sales ~ newspaper, data = data1)
summary(log_reg_newspaper)
