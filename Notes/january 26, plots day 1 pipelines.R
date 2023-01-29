#load a package if it is already installed 
library()

library(tidyverse)

df <-  read_csv("./Data/rectangles.csv")
glimpse(df)

#pipe operator
mean(df$length)
df$length %>% mean() #cntrl shift M gives you pipe --takes output from left side and shifts it
  #to argument on the right. makes argument to next function
# %>%  puts answer of last function in next argument
df$length %>% mean() %>% length() %>% class()
# the output of left hand side becomes first argument of right hand side function
#steps in pipeline

df %>% names() %>% nchar() 
df %>% ggplot()

#fix width column 
df$width <- df$width %>% as.numeric()
df %>% ggplot(aes(x=length,y=width))+
  geom_point() +
  geom_smooth()
  
#aesthetics are things like x axis y axis color etc
#in aesthetics x and y will be column names of data frames
  #add geometric layer with + == GEOM + adds layers


#HOW TO MAKE PLOTS
#1. start with df
#2 use ggplot () -- starting with data set
#3 

iris
names(iris)
iris %>% names()
iris$Species %>% unique

iris %>% ggplot(aes(x=Species, y=Petal.Length, color=Species)) + 
  geom_boxplot()+
  geom_jitter(width=.1, alpha = .25)
#adding colors color = column mapping Species column to color as well as x axis

#alpha deals with transparency
 iris %>% ggplot(aes(x=Petal.Length, y=Petal.Width, color=Species))+
   geom_point(color="#07F131")+
   geom_smooth(method="lm")+
   theme_minimal()
# lm constrain it to be linear model 
 # color = "black" makes points black
 #go to google find color picker paste color identity into quotes