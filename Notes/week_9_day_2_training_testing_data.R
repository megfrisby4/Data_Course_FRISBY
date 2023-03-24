library(ranger)
library(modelr)
library(vip)
library(tidyverse)


vip::vip(ranger_model)

library(tidyverse)
library(modelr)
library(palmerpenguins)
library(easystats)

mod <- glm(data=penguins, formula=body_mass_g~sex*species*island+ bill_length_mm+ bill_depth_mm+
           flipper_length_mm)
mod
df <- add_predictions(penguins,mod)
df <- add_residuals(df,mod)
summary(mod)
performance(mod)
penguins$species %>% unique()

#design a new penguin
#model can only predict using exact predictors
#have to give a data fram with all columns

newpenguin <- data.frame(sex="male",
                  species="Gentoo", 
                  island="Biscoe",
                  bill_length_mm=1000, 
                  bill_depth_mm=1000, 
                  flipper_length_mm=1)
names(penguins)
add_predictions(newpenguin,mod)
df$body_mass_g %>% max(na.rm=TRUE)


#we are extrapolating we are predicting data outside the scope of our model
#our model doesn't know how to deal with the values we have given it
#make sure predictions are within range of models

#whats the R^2 value of our model on data it hasn't seen

#throw out data and save for later
#CROSS VALIDATION
#testing dataset and training dataset



# testing data set gets set aside until the end
#training data set is for model building



#how do we pick whats each dataset
#randomly!
penguins
set.seed(69)
testing <- sample(1:nrow(penguins),size=round(nrow(penguins)*.2))
#choose 20% 
testing
#subset to the random values
test <- penguins[testing,]
trained <- penguins[-testing,]

mod2 <- glm(data=trained, formula=mod$formula)
summary(mod2)
performance(mod2)
rmse(mod)
rmse(mod2)
performance(mod)

add_residuals(data=test,mod2) %>% pluck("resid") %>% .^2 %>% mean(na.rm=TRUE) %>% sqrt()
#the dot means the thing you passed square them all 
add_predictions(data=test, mod2)
performance(mod2,newdata=test)
performance(mod)


compare_performance(mod,mod2) %>% plot()

rsquare(mod2,test)
rsquare(mod,penguins)
#tries to get away from overfitting, model is too good
#reserving so we know its good
#testing the model under new conditions


#lets do this 1000 times
testing <- sample(1:nrow(penguins),size=round(nrow(penguins)*.2))
#choose 20% 
testing
#subset to the random values
rsq_mod2 <- c()

for (i in 1:100) {testing <- sample(1:nrow(penguins),size=round(nrow(penguins)*.2))
test <- penguins[testing,]
trained <- penguins[-testing,]
mod2 <- glm(data=trained, formula=mod$formula)
rsq_mod2[i] <-  rsquare(mod2,test)
}

data.frame(value=rsq_mod2) %>% 
  ggplot(aes(x=value))+
  geom_density()

#library(tidymodels)
