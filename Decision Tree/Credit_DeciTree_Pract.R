Credit <- read.csv("C:/Users/atulr/Desktop/Advanced Analytics/Machine-Learning/credit.csv", header = TRUE, stringsAsFactors = TRUE)
Credit_Raw <- Credit

summary(Credit)
##checking for missinng values
Credit[!complete.cases(Credit),]
## EDA
library(ggplot2)
p1 <- ggplot(Credit, aes(x = credit_history, y = amount))+
  geom_point()+
  facet_wrap(~personal_status, ncol = 2)+
  theme(text = element_text(size = 20))

class(Credit$amount)
options(scipen = 9999)
## making some changes to see if it improves the Model
##Credit$months_loan_duration <- ifelse(Credit$months_loan_duration <= 12, "Short Term Loan", ifelse(Credit$months_loan_duration > 12 & Credit$months_loan_duration <= 20, "Short-Midterm Loan", ifelse(Credit$months_loan_duration > 20 & Credit$months_loan_duration <= 42, "Mid Term Loan", "Long Term Loans")))
#the above transformation doesn yield a good result
#Credit$months_loan_duration <- as.factor(Credit$months_loan_duration)
#Credit <- Credit[,-19]
#Credit$checking_balance <- ifelse(Credit$checking_balance == "unknown" & Credit$savings_balance == "< 100 DM", "< 0 DM", ifelse(Credit$checking_balance == "unknown" & Credit$savings_balance == "> 1000 DM", "> 200 DM", "unknown"))
# Credit$months_loan_duration <- ifelse(Credit$months_loan_duration <= 12, "1-Year Loan", 
#                                       ifelse(Credit$months_loan_duration > 12 & Credit$months_loan_duration <= 24, "2-Year Loan", 
#                                        ifelse(Credit$months_loan_duration > 24 & Credit$months_loan_duration <= 36, "3-Year Loan",
#                                          ifelse(Credit$months_loan_duration > 36 & Credit$months_loan_duration <= 48, "4-Year Loan",
#                                           ifelse(Credit$months_loan_duration > 48 & Credit$months_loan_duration <= 60, "5-Year Loan",
#                                            ifelse(Credit$months_loan_duration > 60 & Credit$months_loan_duration <= 72, "6-Year Loan", "7-Year Loan"))))))

plot(Credit$savings_balance, Credit$amount)
Credit$default <- as.factor(Credit$default)
Credit$installment_rate <- as.factor(Credit$installment_rate)
Credit$residence_history <- as.factor(Credit$residence_history)
Credit$existing_credits <- as.factor(Credit$existing_credits)
Credit$dependents <- as.factor(Credit$dependents)

p2 <- ggplot(Credit, aes(x = as.factor(credit_history), y = amount, fill = default)) + 
  geom_bar(stat='identity')+
  theme(text = element_text(size = 20))
p2

p3 <- ggplot(Credit, aes(x = as.factor(months_loan_duration), y = amount, fill = default)) + 
  geom_bar(stat='identity')+
  theme(text = element_text(size = 20))
p3

p4 <- ggplot(Credit, aes(x = as.factor(personal_status), y = amount, fill = default)) + 
  geom_bar(stat='identity')+
  theme(text = element_text(size = 20))
p4  

p5 <- ggplot(Credit, aes(x = as.factor(credit_history), y = amount, fill = credit_history)) + 
  geom_bar(stat='identity')+
  theme(text = element_text(size = 20))
p5

p6 <- ggplot(Credit, aes(x = as.factor(purpose), y = amount, fill = default)) + 
      geom_bar(stat='identity')+
      theme(text = element_text(size = 20))

p7 <- ggplot(Credit, aes(x = property, y = amount, fill = default)) + 
  geom_bar(stat='identity')+
  theme(text = element_text(size = 20))

p8 <- ggplot(Credit, aes(x = job, y = amount, fill = default)) + 
  geom_bar(stat='identity')+
  theme(text = element_text(size = 20))

p9 <- ggplot(Credit, aes(x = foreign_worker, y = amount, fill = default)) + 
  geom_bar(stat='identity')+
  theme(text = element_text(size = 20))

p10 <- ggplot(Credit, aes(x = as.factor(dependents), y = amount, fill = default)) + 
  geom_bar(stat='identity')+
  theme(text = element_text(size = 20))

plot(Credit$amount, Credit$savings_balance)


p11 <- ggplot(Credit, aes(x = job, y = age)) + 
  geom_bar(stat='identity')+
  theme(text = element_text(size = 20))

plot(Credit$amount, Credit$savings_balance)

## to check the defaulters against the duration of loans
summary(Credit$months_loan_duration)

## model building C50

credit <- read.csv("credit.csv")
str(credit)

# Checking the frequency of checking balance
table(Credit$checking_balance)

# Checking the frequency of savings balance
table(Credit$savings_balance)

# 
#Checking the frequency of checking balance

#
summary(Credit$amount)

# How many people defaulted in the dataset
table(Credit$default)

set.seed(12345)
credit_rand <- Credit[order(runif(1000)), ]

summary(Credit$amount)
summary(credit_rand$amount)

head(Credit$amount)
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


###########################
Credit <- read.csv("C:/Users/atulr/Desktop/Advanced Analytics/Machine-Learning/credit.csv", header = TRUE, stringsAsFactors = TRUE)

Credit$default <- as.factor(Credit$default)
Credit$installment_rate <- as.factor(Credit$installment_rate)
Credit$residence_history <- as.factor(Credit$residence_history)
Credit$existing_credits <- as.factor(Credit$existing_credits)
Credit$dependents <- as.factor(Credit$dependents)

Credit_Raw <- Credit

CreditMod <- Credit
tab <- table(CreditMod$checking_balance, CreditMod$savings_balance)
options(scipen = 9999)
chisq.test(tab) #there is relationship btn  these two categorical var, so combining them
##not good ideastart
CreditMod$SavBalCredBal_Comb <- factor(CreditMod$checking_balance : CreditMod$savings_balance)
CreditMod$SavBalCredBal_Comb <- ifelse(CreditMod$SavBalCredBal_Comb == "< 0 DM:< 100 DM", "Less0DMandLess100DM",
                                ifelse(CreditMod$SavBalCredBal_Comb == "< 0 DM:> 1000 DM", "Less0DMandgret1000DM",
                                ifelse(CreditMod$SavBalCredBal_Comb == "< 0 DM:101 - 500 DM", "Less0DMand101to500",
                                ifelse(CreditMod$SavBalCredBal_Comb == "< 0 DM:501 - 1000 DM", "Less0DMand501to1000",
                                ifelse(CreditMod$SavBalCredBal_Comb == "< 0 DM:unknown", "Less0DMandunk",
                                ifelse(CreditMod$SavBalCredBal_Comb == "> 200 DM:< 100 DM", "Gret200DMandless100DM",
                                ifelse(CreditMod$SavBalCredBal_Comb == "> 200 DM:> 1000 DM", "Gret200DMandgret1000DM",
                                ifelse(CreditMod$SavBalCredBal_Comb == "> 200 DM:101 - 500 DM", "Gret200DMand101to500",
                                ifelse(CreditMod$SavBalCredBal_Comb == "> 200 DM:501 - 1000 DM", "Gret200DMand501to1000",
                                ifelse(CreditMod$SavBalCredBal_Comb == "> 200 DM:unknown", "Gret200DMandunk",
                                ifelse(CreditMod$SavBalCredBal_Comb == "1 - 200 DM:< 100 DM", "1to200andlessDM",
                                ifelse(CreditMod$SavBalCredBal_Comb == "1 - 200 DM:> 1000 DM", "1to200andgret1000DM",
                                ifelse(CreditMod$SavBalCredBal_Comb == "1 - 200 DM:101 - 500 DM", "1to200and101to500",
                                ifelse(CreditMod$SavBalCredBal_Comb == "1 - 200 DM:501 - 1000 DM", "1to200and501to1000",
                                ifelse(CreditMod$SavBalCredBal_Comb == "1 - 200 DM:unknown", "1to200andunk",
                                ifelse(CreditMod$SavBalCredBal_Comb == "unknown:< 100 DM", "unkandless100DM",
                                ifelse(CreditMod$SavBalCredBal_Comb == "unknown:> 1000 DM", "unkandgret1000DM",
                                ifelse(CreditMod$SavBalCredBal_Comb == "unknown:101 - 500 DM", "unkand101to500",
                                ifelse(CreditMod$SavBalCredBal_Comb == "unknown:501 - 1000 DM", "unkand501to1000",
                                "unkandunk")))))))))))))))))))
CreditMod <- CreditMod[-c(1,6)]
#Not good ideaend

CreditMod$persStatOthDebComb <- factor(CreditMod$personal_status : CreditMod$other_debtors)

CreditMod$persStatOthDebComb <- ifelse(CreditMod$persStatOthDebComb == "divorced male:co-applicant", "divorced male&co-applicant",
                                       ifelse(CreditMod$persStatOthDebComb == "divorced male:guarantor", "divorced male&guarantor",
                                        ifelse(CreditMod$persStatOthDebComb == "divorced male:none", "divorced male&none",
                                        ifelse(CreditMod$persStatOthDebComb == "female:co-applicant", "female&co-applicant",
                                        ifelse(CreditMod$persStatOthDebComb == "female:guarantor", "female&guarantor",
                                        ifelse(CreditMod$persStatOthDebComb == "female:none", "female&none",
                                        ifelse(CreditMod$persStatOthDebComb == "married male:co-applicant", "married male&co-applicant",
                                        ifelse(CreditMod$persStatOthDebComb == "married male:guarantor", "married male&guarantor",
                                        ifelse(CreditMod$persStatOthDebComb == "married male:none", "married male&none",
                                        ifelse(CreditMod$persStatOthDebComb == "single male:co-applicant", "single male&co-applicant",
                                        ifelse(CreditMod$persStatOthDebComb == "single male:guarantor", "single male&guarantor","single male&none")))))))))))

CreditMod <- CreditMod[-c(9,10)]

set.seed(12345)
creditMod_rand <- CreditMod[order(runif(1000)), ]

creditMod_train <- creditMod_rand[1:900, ]
creditMod_test <- creditMod_rand[901:1000, ]

creditMod_model <- C5.0(creditMod_train[-19], as.factor(creditMod_train$default))

creditMod_pred <- predict(creditMod_model, creditMod_test)

CrossTable(creditMod_test$default, creditMod_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#############################
library(randomForest)
Credit_RFmod <- randomForest(default~. , data = credit_train, mtry=5, ntree=20)

credit_RFtest <- predict(Credit_RFmod, credit_test)
CrossTable(credit_test$default, credit_RFtest,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#############################
## decision tree using Rpart
library(rpart)
Credit_rpartmod <- rpart(default~., data = creditMod_train, method = "class")
Credit_RpartPred <- predict(Credit_rpartmod, creditMod_test, type = "class")



CrossTable(creditMod_test$default, Credit_RpartPred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#############################

## Ensembling c50, logistic and mod model

Predicted4models <- cbind(credit_pred, Credit_Logist_Test$values, creditMod_pred, Credit_RpartPred)
## to get mode of above df
modefunc <- function(x){
  tabresult <- tabulate(x)
  themode <- which(tabresult == max(tabresult))
  if(sum(tabresult == max(tabresult))>1) themode <- "2"
  return(themode)
}
EnsembledPredFin <- apply(Predicted4models, 1, modefunc)
CrossTable(credit_test$default, EnsembledPredFin,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))




EnsembledPred <- as.factor(ifelse(credit_pred=="1" & Credit_Logist_Test$values =="1", "1",
                            ifelse(credit_pred=="1" & creditMod_pred == "1", "1",
                            ifelse(Credit_Logist_Test$values =="1" & creditMod_pred == "1", "1", "2"))))
CrossTable(credit_test$default, EnsembledPred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
