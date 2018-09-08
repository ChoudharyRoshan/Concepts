#Wisc <- read.csv("C:/Users/atulr/Desktop/Advanced Analytics/Machine-Learning/wisc_bc_data.csv", header = TRUE)

Wisc <- read.csv(paste0(getwd(),"/wisc_bc_data.csv"), header=TRUE, stringsAsFactors = TRUE)
View(Wisc)
str(Wisc)
summary(Wisc)
cor(Wisc$compactness_mean, Wisc$compactness_se)
boxplot(x = Wisc$diagnosis, y= Wisc$radius_mean)
cor(Wisc[-c(1,2)])
p <- ggplot(Wisc, aes(x = diagnosis, y = radius_mean))+
  geom_boxplot(fill = I("Tomato"))+
  theme_minimal()+
  theme(text = element_text(size = 20))

p1 <- ggplot(Wisc, aes(x = diagnosis, y = radius_se))+
  geom_boxplot(fill = I("Tomato"))+
  theme_minimal()+
  theme(text = element_text(size = 20))

p2 <- ggplot(Wisc, aes(x = diagnosis, y = radius_worst))+
  geom_boxplot(fill = I("Tomato"))+
  theme_minimal()+
  theme(text = element_text(size = 20))
library(gridExtra)
grid.arrange(p,p1,p2, nrow = 3)

p3 <- ggplot(Wisc, aes(x = diagnosis, y = area_mean))+
  geom_boxplot(fill = I("Tomato"))+
  theme_minimal()+
  theme(text = element_text(size = 20))

p4 <- ggplot(Wisc, aes(x = diagnosis, y = area_se))+
  geom_boxplot(fill = I("Tomato"))+
  theme_minimal()+
  theme(text = element_text(size = 20))

p5 <- ggplot(Wisc, aes(x = diagnosis, y = area_worst))+
  geom_boxplot(fill = I("Tomato"))+
  theme_minimal()+
  theme(text = element_text(size = 20))
library(gridExtra)
grid.arrange(p3,p4,p5, nrow = 3)

p6 <- ggplot(Wisc, aes(x= perimeter_mean))+
  geom_histogram(file= I("Orange"),
                 color = I("black"),
                 breaks = seq(40,200, 15))+
  theme(text = element_text(size = 20))

p7 <- scatter.smooth(x= Wisc$compactness_mean, y= Wisc$concavity_mean)

class(Wisc$diagnosis)

Wisc$radius_mean <- ifelse(Wisc$radius_mean <= 13.5, "Average", ifelse(Wisc$radius_mean >13.5 & Wisc$radius_mean <= 16, "Above Average","Enlarged"))

library(ggplot2)
ggplot(Wisc, aes(x = as.factor(radius_mean), fill = diagnosis)) + 
  geom_bar(position ='dodge')

ggplot(Wisc, aes(x = diagnosis, y = symmetry_mean))+
  geom_boxplot(fill = I("Tomato"))+
  theme_minimal()+
  theme(text = element_text(size = 20))

Wisc$symmetry_mean <- ifelse(Wisc$symmetry_mean <= 0.1792, "Symmetric", "Non Symmetric")

ggplot(Wisc, aes(x = as.factor(symmetry_mean), fill = diagnosis)) + 
  geom_bar(position ='dodge')+labs(x = "Type of Symmetry", y = "Count of Diagnosis")
  
Wisc$smoothness_mean <- ifelse(Wisc$smoothness_mean <= 0.10530, "Less Smooth","Very Smooth")

ggplot(Wisc, aes(x = as.factor(smoothness_mean), fill = diagnosis)) + 
  geom_bar(position ='dodge')+labs(x = "Type of Smoothness", y = "Count of Diagnosis")

ggplot(Wisc, aes(x = diagnosis, y = perimeter_mean))+
  geom_point()+
  facet_wrap(~radius_mean, ncol = 2)+
  theme(text = element_text(size = 20))


#######
## PCA and Factor analysis
set.seed(1)
pcal <- princomp(Wisc[-c(1,2)], scores = TRUE, cor = TRUE)
summary(pcal)

# For Loadings
loadings(pcal)

# For Scree Plot For Eigen Values
plot(pcal)
screeplot(pcal, type = "line", main = "Scree Plot")

# For Scores Eigen Values
pcal$scores[1:10, 1:4]

#Factor Analysis
fa <- factanal(Wisc[-c(1,2)], factor = 5, lower = 0.01, rotation = "varimax")


### Random Forest in raw data
library(randomForest)
Wisc_RFmod <- randomForest(diagnosis~. , data = Wisc[-1], mtry=6)

z<- table(Wisc$diagnosis, Wisc_RFmod$predicted)
CrossTable(Wisc$diagnosis, Wisc_RFmod$predicted,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual M', 'predicted M'))
# Just to check we are dividing the date into two parts
set.seed(12345)
Wisc_rand <- Wisc[order(runif(569)), ]
Wisc_test <- Wisc_rand[1:171,]
wisc_train <- Wisc_rand[172:569,]

WiscTrain_RFmod <- randomForest(diagnosis~. , data = wisc_train[-1], mtry=6)
Wisc_test_Pred <- predict(WiscTrain_RFmod, Wisc_test)

### Random Forest using above FA
Wisc_FA_RFmod <- randomForest(diagnosis~ area_mean + compactness_worst + perimeter_se + texture_worst, data = Wisc[-1], mtry=2)
CrossTable(Wisc$diagnosis, Wisc_FA_RFmod$predicted,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual M', 'predicted M'))
z<- table(Wisc$diagnosis, Wisc_FA_RFmod4$predicted)
# Considering 5 FA
Wisc_FA_RFmod2 <- randomForest(diagnosis~ area_mean + compactness_worst + perimeter_se + texture_worst + concavity_worst, data = Wisc[-1], mtry=2)
CrossTable(Wisc$diagnosis, Wisc_FA_RFmod$predicted,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual M', 'predicted M'))

### Random Forest using PCA
Wisc_FA_RFmod3 <- randomForest(diagnosis~ fractal_dimension_se  + radius_mean + smoothness_worst + texture_worst, data = Wisc[-1], mtry=2)
CrossTable(Wisc$diagnosis, Wisc_FA_RFmod3$predicted,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual M', 'predicted M'))

### Random Forest Using 5 Factors and Rotation Varimax
Wisc_FA_RFmod4 <- randomForest(diagnosis~ area_mean + fractal_dimension_worst + fractal_dimension_se + texture_worst + concavity_worst, data = Wisc[-1], mtry=2)
CrossTable(Wisc$diagnosis, Wisc_FA_RFmod4$predicted,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual M', 'predicted M'))
