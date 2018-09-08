library(ggplot2)
mtcars <- mtcars
View(mtcars)

ggplot(mtcars, aes(x=mpg))+ geom_histogram(binwidth = 5, fill="green", color="red")+
  ggtitle("Distribution of mpg")+
  xlab("milepergal")+ylab("number")+
  xlim(0,50)

#density
ggplot(mtcars, aes(x=mpg))+geom_density(fill="blue")

#scatter plot
ggplot(mtcars, aes(wt,mpg))+geom_point(size=3, color="red")

mtcars$cyl <- as.factor(mtcars$cyl)
str(mtcars$cyl)

ggplot(mtcars, aes(wt,mpg,color=cyl))+geom_point(size=3)  # adding 3rd variable to scatter plot
                                                          #please not that scatter dot colors has to be removed since it overwrites the code and makes it all green again



#Box plot
ggplot(mtcars, aes(cyl,mpg, fill=cyl))+geom_boxplot()

#Viloin plot  (Another form of box plot)
ggplot(mtcars, aes(cyl,mpg, fill=cyl))+geom_violin()+theme_minimal()

#Interactive plot - using plotly
library(plotly)
d <- diamonds[sample(nrow(diamonds),1000),]
plot_ly(d,x= ~carat,y= ~price,color= ~carat,size= ~carat, text= ~paste("clarity: ", clarity))
  

mtcarsss<- mtcars

write.csv(mtcarsss, "Cars.csv")  
  