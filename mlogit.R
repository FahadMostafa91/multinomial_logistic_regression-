
############### Link we can follow ############
#https://advstats.psychstat.org/book/mregression/catpredictor.php?fbclid=IwAR3cHy6XOblbk4puN9PvUtB1lN-szQjztOfJml7Q9Dn_kC_11GchM07MFtQ
#https://webcache.googleusercontent.com/search?q=cache:gjs51DLjWIwJ:https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/+&cd=1&hl=en&ct=clnk&gl=us
#https://stats.idre.ucla.edu/r/dae/logit-regression/
#https://rpubs.com/beane/n4_2


# load library #################################
library(mlogit)
library(neuralnet)
library(nnet)
library(ggplot2)
library(caret)
set.seed(10)

library(readxl)
Cobb <- read_excel("Cobb_cl.xlsx")
#View(Cobb)
head(Cobb)

# data name defined 
chronic<-factor(Cobb$chronic, c(0,1,2), labels=c('No', 'Yes','Unknown'))
age<- Cobb$age
#race <-Cobb$race
race<-factor(Cobb$race, c(1,2,3), labels=c('WH', 'AM','OTH'))
#sex <-Cobb$sex
sex<-factor(Cobb$sex, c(1,2), labels=c('F', 'M'))
contrasts(sex)
contrasts(race)
contrasts(chronic)
df=data.frame(age,race,sex,chronic)
head(df)


# Training the multinomial model
multinom.fit <- multinom(chronic ~ age+race+sex, data=df)

# Checking the model
summary(multinom.fit)

## CIs using profiled log-likelihood
confint(multinom.fit)
## confussion matrix

predict(multinom.fit, df, type="probs")
predict(multinom.fit, df, type="class")

training_pred <-predict(multinom.fit, df, type="class")

install.packages(e1071)
library(e1071)
confusionMatrix(training_pred, df$chronic)

library(aod)
pred<-predict(multinom.fit)
# prediction of the model
pred


## extracting coefficients from the model and exponentiate
exp(coef(multinom.fit))

head(probability.table <- fitted(multinom.fit))
pred<- predict(multinom.fit, chronic, type = "class")

predict(multinom.fit)

        
