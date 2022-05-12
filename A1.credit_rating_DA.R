#Dalton Anderson

rm(list=ls())

library(dplyr)
library(ggplot2)
library(readxl)
library(corrplot)
library(stargazer)

#preprocessing

#Load in dataset
#Import data
credit_master <- read_excel(".xlsx")
colnames(credit_master)=tolower(make.names(colnames(credit_master)))


#turn gender into a factor
credit_master$state=as.factor(credit_master$gender)
#turn student into a factor
credit_master$state=as.factor(credit_master$student)
#turn married into a factor
credit_master$state=as.factor(credit_master$married)
#turn ethnicity into a factor
credit_master$state=as.factor(credit_master$ethnicity)
#turn education into a factor
credit_master$state=as.factor(credit_master$education)
#turn age into a factor
credit_master$state=as.factor(credit_master$age)
#check results
str(credit_master)

#1.) What variables predict credit scores and by how much?

#sample set created
tempdf = credit_master
set.seed(59657076)
sample <- tempdf
credit = sample_n(sample, 150)

#model with education
cred1.out = lm(rating~income+limit+cards+student+married+balance+education, data = credit)
summary(cred1.out)

#model with education + age
cred2.out = lm(rating~income+limit+cards+student+married+balance+education+age, data = credit)
summary(cred2.out)

#final model
cred.out = lm(rating~income+limit+cards+gender+student+married+ethnicity+balance+education, data = credit)
summary(cred.out)

#LINE

# linearity of the data
plot(cred.out, 1)

#I am okay with this

# normality of residuals
hist(cred.out$residuals)
plot(simple_model, 2)


#homoscedasticity assumption
plot(cred.out, 3)


#Cook's distance
plot(cred.out, 4)
#no values over .5

#Residuals vs Leverage
plot(cred.out, 5)

plot(cred.out, 6)

#2.) Is there a racial or gender bias on credit score? If so, by how much?
summary(cred.out)

stargazer(cred.out, cred2.out, probit.model, title="Results", align=TRUE)
#a.)Create a predictor table (see sample assignment solutions) with three columns for predictor, 
#expected sign of effect, and a one-sentence rationale for effect. (2 points)

stargazer(cred.out, cred1.out, cred2.out,
          type = "text",
          digits = 1,
          header = FALSE,
          title= "Regression Results")
#b.) Next run relevant models to answer questions 1 and 2 above. No more than 2-3 models in all. 
#Explain the rationale for your models. (1 point)


#c.)Interpret your models to answer the two questions asked above. (2 points)
  




