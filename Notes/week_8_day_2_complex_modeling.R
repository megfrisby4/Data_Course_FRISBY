#linerar regressions 
#mlu-explain.github.io/linear-regression
#predicting an outcome based on coefficients and variables
#points on plot are reality, line that predicts (linear regression) is MODEL of reality

#how it works
#starts out with y intercept as the mean -- straight line
#finds residual of each point to intercept
#adds predicting variable to see if we can shrink residuals
#the goal of regression is reducing the error so we can find the best fit
#line that equals the sum of the residuals (lowest possible value)
#in R you use GLM
#r^2 = how much percentage of model linear regression explains 
#amount of explanatory power given by line 

#works well for large data
#each observation changes the line slightly
#very sensitive to outliers
#the further away a point is the stronger it pulls on the line


#continuous predictors vs binary predictors
#true false 
#asign true = 1 false = 2
#mulitivariate -- several predictors if pool yes or no 
#two linear regressions
#attitive model 

#regression with interaction
#regressions collide (cross)
#dont add the linear regressions 
#interactive model


library(tidyverse)
library(easystats)
mpg %>% names()
mod1 <- glm(data=mpg, formula = cty~class)
mpg %>% filter(class=="compact") %>% pluck("cty") %>% mean()
summary(mod1)
#remember intercept is whatever comes first in the alphabet, what to compare other variables to 

mod2 <- glm(data=mpg, formula=cty~class+displ)
#drawing a line based on displacement and doing that for each class
summary(mod2)
#gives the slope of every line based on the 28, multiply the slope by the value you are plugging in
#if displacement of 1 = -2.17(1) +28,7768 = 
#allowing for situational variance --- seeing if this depends on the class of car
#predicting displacement is different based on another variable
#allowed to have a varying slope

mod3 <- glm(data=mpg, formula=cty~class*displ)
summary(mod3)
#weird colons are interactions terms (:)


#what classes are there
mpg$class %>% unique


mod4 <- glm(data=mpg, 
            formula= cty~class*displ*cyl)
#think class dislp and cyl are going to effect mpg (cty)
#we aslo might think cyl depends on displ,  displ depends on class etc etc 
summary(mod4)
#there is some NAS
#why? not all observations have all these data points

mod5 <- glm(data=mpg, formula = cty~class*displ*cyl*drv)
summary(mod5)

#check how good a model is with root mean squared error
#the smaller the value the better
rmse(mod1)
rmse(mod5) #has lower mean squared error (r^2)
rmse(mod4)
rmse(mod3)
rmse(mod2)
#these values gives you what the mean error is 
#dont want to be too specific, remember you are predicting based on limited observations 
#rmse is not the only way to see how 'good' a model is

compare_performance(mod1, mod2, mod3, mod4, mod5)
#R2 how much percentage of data is explained by data 
#want RMSE small, R2 big

#AIC and BIC how explanatory each thing is in your model
#how good it is, how simple it is 
#how much information each term is 
#higher is better here 

#pipe to plot to compare
compare_performance(mod1, mod2, mod3, mod4, mod5) %>% plot()
#how good a model is is distance from the center

library(MASS)
step <- MASS::stepAIC(mod5)
#call the namespace MASS::stepAIC instead of loading the entire package
#model selection algorithm based on AIC 
#simplifies an interactive model with complex terms 
#can take hours to run

#what you use it for..
#when you save it it returns a list
#then you use step$formula (or whatever the name of your list is)
#this gives you the suggested formula 
step$formula

#then plug that into model 

mod6 <- glm(data=mpg, 
            formula=step$formula)
compare_performance(mod1, mod2, mod3, mod4, mod5, mod6) %>% plot()
#mod6 has the best of everything
#the closer the line is to the outside at each variable the better the mod is at each thing
#.^2 every variable interacting 
#takes forever = suicide!!

library(patchwork)
check_model(mod6)
#gives us a bunch of graphy graphs 

#additive compare variables seperately 
#same slope different intercept 

#multiplicative allow variables to depend on the others/ interact with eachother
#different slopes 

mpg %>% ggplot(aes(x=displ, y=cty, color=factor(cyl)))+geom_smooth(method="glm")

plot <- modelr::gather_predictions(mpg,mod2,mod3) %>% 
  ggplot(aes(x=displ, y=pred, color=class))+
  geom_point(aes(y=cty), alpha=.5)+
  geom_smooth(method="glm")+
  facet_wrap(~model)
plotly::ggplotly(plot)
               