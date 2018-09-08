wisc <- read.csv(paste0(getwd(), "/wisc_bc_data.csv"), header= TRUE, stringsAsFactors = TRUE)
str(wisc)
wisc<- wisc[2:31]  #Removing ID variable

#Partioning data
set.seed(123)
index <- sample(2, nrow(wisc),
               replace= TRUE,
               prob = c(0.7,0.3)
               )
train <- wisc[index==1, ]
test <- wisc[index==2, ]

#Scatter plot and corelations
library(psych)
pair1 <- pairs.panels(train[,1:8],
                      gap = 0,
                      bg = c("red","yellow")[train$diagnosis],
                      pch=21)
pair1

#PCA
pc <- prcomp(train[,-1],
             center = TRUE,
             scale. = TRUE
             )

attributes(pc)

#Center
pc$center

#scale
pc$scale

#rotations or loadings
pc$rotation[1:10,1:4]

#To see the variance contribution
summary(pc)


#Orthogonality of pc
pairs.pca <- pairs.panels(pc$x, 
             gap=0,
             gb = c("red", "yellow")[train$diagnosis],
             pch=21)

#Bi-plot [needs devtools as well as ggbiplot in github]
library(devtools)
library(ggplot2)
#install_github("vqv/ggbiplot")   [not working]

#g <- ggbiplot(pc,
#              obs.scale=1,
#              var.scale=1,
#              groups = train$diagnosis,
#              ellipse=TRUE,
#              circle=TRUE,
#              ellipse.prob = 0.68)
# g <- g + scale_color_discrete(name = "")
# g <- g + theme(legend.direction = 'horizontal',
#               legend.position = "top")

#prediction using pc
train_pc <- predict(pc, train)
train_pc <- data.frame(train_pc, train[,1])

test_pc <- predict(pc, test)
ptest_pc <- data.frame(test_pc, test[,1])

#PC is done and our Train and Test data are ready interms of PC

#using random forest for modelling
library(randomForest)
set.seed(123)
my_model <- randomForest(train...1.~., data= train_pc,
                         ntree= 300,
                         mtry=6,
                         importance = TRUE,
                         proximity = TRUE)

#attributes of a random forest moddel
attributes(my_model)

my_model$confusion
my_model$importance


#Prediction and confusion matrix - Train Data
library(caret)
p1 <- predict(my_model, train_pc)

confusionMatrix(p1, train_pc$train...1.)


#graph for trees - Error rate of Random forest
plot(my_model)
 
#Tune mtry
tuneRF(train_pc[,-30], train_pc[,30],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 300,
       trace = TRUE,
       improve = 0.05)   #we now get to know ntree should be 300 and mtry =8 so we make the changes in above modelling code my_model


#no. of trees
hist(treesize(my_model),
     main = "No. of nodes of tree",
     col= "green")

#Variable importance [plot, qualitative & varused]
varImpPlot(my_model,
           sort = T,
           n.var=10,
           main = "Top 10- Variable importance")

#To see quantitative value in importance use below
importance(my_model)

#To see which predicted variables are actually used in Rf
varUsed(my_model)

#To extract a single tree
getTree(my_model, 1, labelVar = T)


#Multi Dimensioning scaling plot for Proximity Matrix
MDSplot(my_model, train_pc$train...1.)
