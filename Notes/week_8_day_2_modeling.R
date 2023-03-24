#modeling
#model = simple representation of reality
#usually expressed as equation


#general notes about stats
#the larger the sample size the more acurate the results
#but in science, sample size is limited by time and money

#stat.test
#H0 : average male = avg female
#HA : avg male != avg female

#t-test(x,y)
#t.test(x,y)
#tests null hypothesis

#assumes H0, gives what are the chances you would see this result
#the smaller the p value the stronger the emphasis to accept null, reject the null, failing to accept it. 
#P a measure of how surprise you should be 
#p<0.05 as significant 
#5% get the wrong answer
#reject or fail to reject the null never accept anything

#type 1 error rate of 5% : fail to accept the null hypothesis when the null is actually true
#fail to detect reality 

#type 2 seeing a pattern when there is no pattern
#type 2
#fail to reject the null hypothesis when the alternative hypothesis is true


#.05 is arbitrary - you will be wrong 5% of the time


#t.test()
#aov()
#corr.test() --- pearson or spearman

#generalized linear model
#glm()
#glm(data=df, formula=y~x) 
#y~x: y as a function of x 
#y is response variable that you are interested in 
#x is a predictor variable 
#prediction height as a function of sex 
#glm(data=df, formula=height~sex)

#results for glm()
#estimate(coefficient)-- effect if being male on height or female on height
#p value
#finds averages and uses them as prediction. 
#sexM=52(cm) --- what the males differ from the intercept by 
#intercept = 125 (cm) -- what it wants to guess every human is

#in essence gives us an equation of how to predict heights of males and females 
#this is freaking amazing!!!!!!!




#your (statistical) life begins today!!

library(tidyverse)
mpg=mpg
names(mpg)
#ask what factors could influence the mpg in city = response varible = cty
#manufacturer, model, cyl, trans

mpg %>% ggplot(aes(x=displ, y=cty, color=drv))+geom_point()+geom_smooth(method="glm")+facet_wrap(~cyl)
#method equals glm to graph the model
#with this loess curve we can predict cty mpg for any car given its displacement

#lets actually model this
mod2 <- glm(dat=mpg, formula=cty~displ)
#save the model
summary(mod)
#get a summary of the model that is saved 
#the info i'm getting
#y = mx+b
#y=-2.6305(displ)+25.9915

-2.6301*1+25.9915

#make a new data frame
df <- data.frame(displ=1:10)
predict(mod, newdata = df)
#predict takes in a model, and predicts with some new data
mod
#maybe a straight line isn't the best fit 
mod <- glm(data=mpg, formula=cty~poly(displ,2))
mod <- glm(data=mpg, formula=cty~displ+cyl)
summary(mod)
predict(mod,newdata = df)
#data frame needs another variable
df <- data.frame(displ=1:10, 
                 cyl=5, drv = "f")
  #lets isolate cyl and 5 and look at effect of displacement
predict(mod, newdata = df)
#these answers are as good as youre model was (they are predictions!!!)
mod <- glm(data=mpg, formula=cty~displ+cyl+drv)
mod


#this is a graphical representation of my new model 
mpg %>% ggplot(aes(x=displ, y=cty, color=drv))+
  coord_cartesian(xlim=c(0,7))+
 geom_point()+geom_smooth(method="glm")+
  facet_wrap(~cyl)

#residuals tell you how far off youre points are from your prediction
#coefficients tells us stuff to add to baseline 
#p values how significant the result is or how surprised you should be if you see it. 


#RMSE 
#root mean square error
a <- residuals(mod)^2
b <- mean(a)
c <- sqrt(b)
#general standard error = 
print(c)

library(easystats)
compare_models(mod, mod2)
compare_performance(mod, mod2) %>% plot()
