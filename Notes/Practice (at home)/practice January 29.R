library(tidyverse)
  iris <- iris
  iris %>% ggplot(aes(x=Species, y=Petal.Length, color=Species)) + 
    geom_boxplot()+
    geom_jitter(width=.1, alpha = .25)
  iris              

iris[1,5]
summary(iris)
iris[iris(Petal.Width>5,)]
typeof(iris
       )
iris <- as.data.frame(iris)
iris[iris$Petal.Width<5,]
iris[iris$Petal.Width<5,] %>% ggplot(aes(x=Species, y= Sepal.Length, color=Species))+
  geom_boxplot(outlier.color = "red") +
  geom_jitter(width=.2, alpha=.5) +
theme_minimal() 
