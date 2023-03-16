library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)

data("mtcars")
glimpse(mtcars)

mod1=lm(mpg~disp, data=mtcars)
summary(mod1)

#call = what model i ran
#residuals = how acurate my data fit model
#coefficients = intercept and slope of line of best fit

ggplot(mtcars, aes(x=disp, y=mpg))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_minimal()
#this is kinda a sucky model!
#there might be something better!

mod2 <- lm(mpg~qsec, data=mtcars)
ggplot(mtcars, aes(x=disp, y=qsec))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_minimal()
#just judging by the residuals (distance from the line)
#it appears the second model is worse

#compare mean squared error
mean(mod1$residuals^2)
mean(mod2$residuals^2)

#add predictions and see if they are close to acutality
df <- mtcars %>% 
  add_predictions(mod1)
df %>% dplyr::select("mpg", "pred")


#ask R to predict dependent values based on hypothetical independents
newdf <- data.frame(disp=c(500,600,700,800,900))

#making predictions
pred=predict(mod1,newdata=newdf)

#combining hypothetical imput with hypothetical predictions into one new data frame
hyp_preds <- data.frame(disp=newdf$disp, pred=pred)

#add new column showing whether a data point is real or hypothetical
df$PredictionType <- "Real"
hyp_preds$PredictionType="Hypothetical"

#join real and hypothetical data with predicitons
fullpreds <- full_join(df,hyp_preds)

#plot on original graph
ggplot(fullpreds, aes(x=disp, y=pred, color=PredictionType))+
  geom_point()+
  geom_point(aes(y=mpg), color="black")+
  theme_minimal()

#compare predictions from several models
mod3 <- glm(data=mtcars,formula=mpg~hp+disp+factor(am)+qsec)

#put all models into a list
mods <- list(mod1=mod1,mod2=mod2,mod3=mod3)
map(mods, performance) %>% reduce(full_join)

#gather residuals from all 3 models
mtcars %>% gather_residuals(mod1,mod2,mod3) %>% 
  ggplot(aes(x=model,y=resid,fill=model))+
  geom_boxplot(alpha=.5)+
  geom_point()+
  theme_minimal()

#gather predictions from all 3 models
mtcars %>% gather_predictions(mod1,mod2,mod3) %>% 
  ggplot(aes(x=disp,y=mpg))+
  geom_point(size=3)+
  geom_point(aes(y=pred,color=model))+
  geom_smooth(aes(y=pred,color=model))+
  theme_minimal()+
  annotate("text", x=250,y=32,label=mod1$call)+
  annotate("text",x=250, y=30, label=mod2$call)+
  annotate("text", x=250, y=28, label=mod3$call)

#put the model into interpretable english
report(mod3)
