library(tidyverse)
library(easystats)
library(modelr)
library(palmerpenguins)

#model ... way to predict

names(penguins)
#what you are predicting.... dependent ..... penguin body mass
#use glm initially
mod1 <- glm(data=penguins, formula = body_mass_g ~ sex + species + flipper_length_mm)
summary(mod1)
check_model(mod1)
#continuous --- predict numeric value
#linear regressions are predicting a continuous value




#classification models
#logistic regression 
#logical(T/F)
#response variable must be a true false variable
#take continuous variables and predict a discrete

penguins %>% mutate(male = case_when(sex=="male" ~ TRUE,  TRUE~FALSE))
#  TRUE~FALSE... in all other cases false
#or
penguins <- penguins %>% mutate(male = case_when(sex=="male" ~ TRUE, 
                                                 sex=="female"~FALSE))

mod2 <- glm(data = penguins, formula = male~body_mass_g+
      flipper_length_mm+bill_length_mm+species, 
      family=binomial)
###clasification!  ..... also include family = binomial (two possibilities)

summary(mod2)
add_predictions(penguins, mod2)
#pred is log odds
#have to add type!!!
add_predictions(penguins, mod2, type = "response")
#changes it to percentage --- probability our model assigned that observation to mal
library(ggplot2)

add_predictions(penguins, mod2, type = "response") %>% 
ggplot(aes(x=body_mass_g,y=pred, color=species))+ geom_smooth()

pen <- add_predictions(penguins, mod2, type = "response")

pen %>% ggplot(aes(x=body_mass_g, y=pred, color=species))+geom_smooth()

mod2 <- glm(data=penguins, formula = male~(body_mass_g + 
                                             flipper_length_mm + 
                                             bill_length_mm+
                                           island+year)*species)

add_predictions(penguins, mod2, type = "response") %>% 
  ggplot(aes(x=body_mass_g,y=pred, color=sex, shape=species))+ geom_point(size=3, alpha = .5)


#cran ordinal regression
#lets you have any number of things besides just 2 true false


#genomics 
#lists 



