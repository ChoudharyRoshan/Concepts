# Decison Tree For Credit Risk file
# Setting my working Directory

setwd("C:/Users/user/Desktop/ATI - Analaytics-Mohit/Machine-Learning-with-R-datasets-master")

credit <- read.csv("credit.csv")
str(credit)

# Checking the frequency of checking balance
table(credit$checking_balance)

# Checking the frequency of savings balance
table(credit$savings_balance)

# 
Checking the frequency of checking balance

#
summary(credit$amount)

# How many people defaulted in the dataset
table(credit$default)

set.seed(12345)
credit_rand <- credit[order(runif(1000)), ]

summary(credit$amount)
summary(credit_rand$amount)

head(credit$amount)
head(credit_rand$amount)

credit_train <- credit_rand[1:900, ]
credit_test <- credit_rand[901:1000, ]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

#
install.packages("C50", dependencies = TRUE)
library(C50)

credit_model <- C5.0(credit_train[-21], as.factor(credit_train$default))

credit_model

summary(credit_model)

credit_pred <- predict(credit_model, credit_test)

library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
