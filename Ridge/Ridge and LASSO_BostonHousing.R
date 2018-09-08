#Libraries Needed
library(glmnet)     #For Ridge Lasso and Elastic net
library(caret)     #For Partition & custom control parameters
library(mlbench)   #For Bostonhousing
library(psych)     #For Corelation

#Data
data(BostonHousing)
data<-BostonHousing

str(data)
?BostonHousing

#To see the co-relations
pairs.panels(data[,-c(4,14)], cex=2)

#Data Partition
index<-createDataPartition(data$medv, p=0.7, list = FALSE, times=1)
train_data<-data[index,]
test_data<-data[-index,]

#To create custom control parameters
custom<-trainControl(method="repeatedcv",   #Its a method of repeated cross validation
                     number = 10,            #Its the K value under k-fold cross validation
                     repeats = 5,            #How many times to repeat the K-fold
                     verboseIter = T)        #To see whats happening in which iteration, make it TRUE 

#Linear Model
set.seed(1234)
lm<-train(medv~.,train_data,
          method='lm',
       trcontrol=custom)


#Reults of lm
lm$results     #To check the results
lm             #To check whatit says
summary(lm)    #To check the summary and significance of the variabless
plot(lm$finalModel)   #To get the models plos eg. fitted v/s residuals


#Ridge Regression
set.seed(1234)
ridge<-train(medv~.,train_data, method='glmnet',
             tuneGrid= expand.grid(alpha=0,
                                   lambda=seq(0.0001,1,length=5)),
             trControl=custom)   #aplha is 0 because of ridge regression

#Plot the ridge. It gives output based on cross validation
plot(ridge)
ridge
plot(ridge$finalModel, xvar="lambda", label=T) #Ridge plot, to see hoe increase in lamda increases the co-effecients
plot(ridge$finalModel, xvar="dev", label=T)  #To see the variance explained with increase in lamda value
plot(varImp(ridge, scale=F))  #To check the variable importance in graph. Scale is just to change the x axis numbers


#LASSO Regression
set.seed(1234)
Lasso<-train(medv~.,train_data, method='glmnet',
             tuneGrid= expand.grid(alpha=1,
                                   lambda=seq(0.0001,0.2,length=5)),
             trControl=custom)   #aplha is 1 because of lasso regression

#Plotting  Lasso
plot(Lasso)
Lasso
plot(Lasso$finalModel, xvar="lambda", label=T) #Lasso plot, to see how increase in lamda increases the co-effecients
plot(Lasso$finalModel, xvar="dev", label=T)  #To see the variance explained with increase in lamda value
plot(varImp(Lasso, scale=F))  #To check the variable importance in graph. Scale is just to change the x axis numbers




#Elastic net regression
set.seed(1234)
elastic<-train(medv~.,train_data, method='glmnet',
              tuneGrid= expand.grid(alpha=seq(0,1,length=10),
                                    lambda=seq(0.0001,0.2,length=5)),
              trControl=custom)   #aplha & Lasso both needs to find an optimum value because of elastic net regression

#Plotting  Elastic
plot(elastic)
elastic
plot(elastic$finalModel, xvar="lambda", label=T) #Elastic plot, to see how increase in lamda increases the co-effecients
plot(elastic$finalModel, xvar="dev", label=T)  #To see the variance explained with increase in lamda value
plot(varImp(elastic, scale=F))  #To check the variable importance in graph. Scale is just to change the x axis numbers


#comparing models  (Not working here)
model_list<-list(linearModel=lm, Ridge=ridge, Lasso=Lasso, Elastic=elastic)
res <- resamples(model_list)  #Part of caret package
summary(res)
bwplot(res) #Box and Viscous plot
xyplot(res, metric = 'RMSE')


#Best Model
elastic$bestTune
best<-elastic$finalModel
coef(best, elastic$bestTune$lambda)

#Save the model for later use
saveRDS(elastic, "final_model.rds")


#Read RDS
fm<-readRDS("final_model.rds")
print(fm)



#Prediction
p1 <- predict(fm, train_data)
sqrt(mean((train_data$medv-p1)^2))   #Calculating RMSE

p2 <- predict(fm, test_data)         #Calculating RMSE
sqrt(mean((test_data$medv-p2)^2))
