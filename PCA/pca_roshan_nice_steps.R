wisc <- read.csv(paste0(getwd(),"/wisc_bc_data.csv"), header=TRUE, stringsAsFactors = TRUE)
View(wisc)

str(wisc)
summary(wisc)

nrow(wisc)
names(wisc)
## Checking how many complete.cases we have
colSums(is.na(wisc))
incompleteRows <- nrow(wisc[!complete.cases(wisc) , ])
totalMissing <- incompleteRows/nrow(wisc)*100

nrow(wisc)
print(incompleteRows)
print(totalMissing)

library(ggplot2)
#Radis Mean v/s Diagnosis 
p1 <- ggplot(wisc,aes(diagnosis,radius_mean, fill=diagnosis))+geom_boxplot()+
  theme_minimal()+
  theme(text=element_text(20))

p1


#RadiusMean density
p2 <- ggplot(wisc, aes(x=radius_se))+geom_density(fill="blue")
p2


#Scatter Plot [radius mean v/s radius_se]
p3 <- ggplot(wisc, aes(radius_se,radius_mean))+geom_point(size=3, color="red")
p3

#radis_se is high co-related with radius_mean
cr1 <- cor.test(wisc$radius_se, wisc$radius_mean)
cr1


# Using functions from {psych} package  corelation
library(psych)
pairs.panels(wisc[, 2:8]) 

##PCA##

#Removing dependent variable
my_data <- subset(wisc, select= -c(id))


#To check if all are numeric before performing PCA
str(my_data)

#converting into test and train using caTools
library(caTools)
set.seed(101) 
sample = sample.split(my_data$diagnosis, SplitRatio = .7)
fst_train = subset(my_data, sample == TRUE)
fst_test  = subset(my_data, sample == FALSE)

View(fst_train)
nrow(fst_train)

View(fst_test)
nrow(fst_test)

train<-fst_train[,-1]
#principal component analysis
prin_comp <- prcomp(train, scale. = T)
names(prin_comp)

#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale


# outputs the principal component rotation
prin_comp$rotation
nrow(prin_comp$rotation)

#Let's look at first 4 principal components and first 5 rows.
prin_comp$rotation[1:5,1:4]

prin_comp$x[1:5,1:4]
dim(prin_comp$x)

#cr2<-cor.test(prin_comp$rotation,prin_comp$x)

#Lets plot the resultant principal components.
biplot(prin_comp, scale = 0)  #The parameter scale = 0 ensures that arrows are scaled to represent the loadings.

#prcomp() function also provides the facility to compute standard deviation of each principal component. 
#sdev refers to the standard deviation of principal components.

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#To compute the proportion of variance explained by each component, 
#we simply divide the variance by sum of total variance. This results in:

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]



#So, how do we decide how many components should we select for modeling stage ?

#The answer to this question is provided by a scree plot. 
#A scree plot is used to access components or factors which explains the most of variability in the data. 
#It represents values in descending order.

#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

if(FALSE){
  "The plot above shows that ~ 10 components explains around 94% variance in the data set. 
   In order words, using PCA we have reduced 31 predictors to 10 without compromising on explained variance. 
   This is the power of PCA> Lets do a confirmation check, by plotting a cumulative variance plot. 
  This will give us a clear picture of number of components."
}

#cumulative scree plot
  plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

if(FALSE){"This plot shows that 10 components results in variance close to ~ 98%. 
Therefore, in this case, we'll select number of components as 10 [PC1 to PC10] and proceed to the modeling stage. 
This completes the steps to implement PCA on train data. For modeling, we'll use these 10 components as 
predictor variables and follow the normal procedures"}
  
if(FALSE){"But, few important points to understand:

We should not combine the train and test set to obtain PCA components of whole data at once. Because, 
this would violate the entire assumption of generalization since test data would get leaked into the training set. In other words, 
the test data set would no longer remain unseen. Eventually, this will hammer down the generalization capability of the model.

We should not perform PCA on test and train data sets separately. Because, the resultant vectors from train and test PCAs will have different directions (due to unequal variance). 
Due to this, we'll end up comparing data registered on different axes. Therefore, the resulting vectors from train and test data should have same axes.

So, what should we do?

We should do exactly the same transformation to the test set as we did to training set, including the center and scaling feature. Let's do it in R:"}


#add a training set with principal components
train.data <- data.frame(diagnosis = fst_train$diagnosis, prin_comp$x)
View(train.data)

#We are interested in first 12 PCA only, Hence
train.data<-train.data[,1:10]

#Random Forest
library(randomForest)
set.seed(123)
rf <- randomForest(diagnosis~., data= train.data)
rf
summary(rf)


#transform test into PCA
test.data <- predict(prin_comp, newdata = fst_test[,-1])
test.data <- as.data.frame(test.data)

#select the first 10 components
test.data <- test.data[,1:10]


#make prediction on test data
rf.prediction <- predict(rf, test.data)
View(rf.prediction)

class(rf.prediction)
fst_test1<-as.vector(fst_test[,1])
x<-as.data.frame(rf.prediction,fst_test1)
View(x)


cm<-table(fst_test[,1],rf.prediction)
