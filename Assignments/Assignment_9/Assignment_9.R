library(tidyverse)
library(readr)
library(easystats)
library(patchwork)
library(GGally)
library(modelr)
#read in the grad school data frame
df <- read_csv("./Assignment_9_Data/GradSchool_Admissions.csv")

#look at the data frame 
glimpse(df)
summary(df)

#I want to explore what variables influence admittance
#admittance is now written as 0 or 1 but it is actually a logical vector
#1=true and 2=false 
#i will transform the data to make it that way 
#im also going to make rank a factor, because it is, you cant have a rank of 1.4 etc

df <- df %>% mutate(admit=as.logical(admit), rank=as.factor(rank))
#there we go -- test it out
df
#look at it a bit
summary(df)
glimpse(df)


#do a bit of exploring to define some relationships
#I want to look at how admittance is influenced by the other variables

df %>% select(admit, gpa, gre, rank) %>% ggpairs()
ggsave("variable_relationships.png")
#from this chart it looks like rank gpa and gre all influence adnmittance in some way

#initial plots to look at relationships between admittance and variables
#i also think that gpa and gre are associated, and maybe some of the other 
#variables so I will explore that later when I model

#gpa and admittance
df %>% ggplot(aes(x=gpa, fill=admit)) + geom_density(alpha=.5)

ggsave("gpa_vs_admit_plot.png")

#gre and admittance
df %>% ggplot(aes(x=gre, fill=admit)) + geom_density(alpha=.5)

ggsave("gre_vs_admit_plot.png")

#rank and admittance
df %>% mutate(rank=as.factor(rank)) %>% ggplot(aes(x=rank, fill=admit))+
  geom_density(alpha=.5)
ggsave("rank_vs_admit_plot.png")
#this is interesting... the curves are very similar for rank, 
#the only one that a greater density of admitted occurred was for rank 1
#the others a greater density of not admitted occurred. 

#all in all I think all of these variables influenced 
#admittance so I will construct my model(s) appropriately 

#I know I will need a classification model because I am predicting a logistic regression
mod <- glm(data=df, formula = admit~gre + gpa + rank, family = binomial)
mod
summary(mod)
performance(mod)
check_model(mod)
#looks like my model is ok according to these, residuals kinda suck but everything else is good
rmse(mod)

#try another model
mod2 <- glm(data=df, formula = admit~rank*gre*gpa, family=binomial)
mod2
summary(mod2)
performance(mod2)
check_model(mod2)
# it looks like it might be better in some ways and woorse in others... 
#the residuals might be a little better but the colinearity is worse!
step <- MASS::stepAIC(mod2)
step$formula

#we'll try this generated formula and see if it gives us anything better
mod3 <- glm(data=df, formula= step$formula, family=binomial)
mod3
summary(mod3)
performance(mod3)
check_model(mod3)
#hmm this might look a little better... but not awesome!

#lets see how these compare 
compare_performance(mod, mod2,mod3) %>% plot()

#theres not really a super clear answer here which is dissapointing
#ill try some more models

mod4 <- glm(data=df, formula = admit~gre*gpa + rank, family=binomial)
mod4
summary(mod4)
check_model(mod4)
#not amazing either

compare_performance(mod, mod2,mod3, mod4) %>% plot()
#some more simple models just to see

mod5 <- glm(data=df, formula=admit~rank, family=binomial)
check_model(mod5)


compare_performance(mod, mod2,mod3, mod4,mod5) %>% plot()

mod6 <- glm(data=df, formula=admit~gpa*gre*rank+rank+gpa+gre, family=binomial)
check_model(mod6)

mod7 <-glm(data=df, formula=admit~gpa*gre+rank, family=binomial)
check_model(mod7)

mod8 <- glm(data=df, formula=admit~gpa+rank, family=binomial)
check_model(mod8)

mod9 <- glm(data=df, formula = admit~gre+gpa*rank, family=binomial)

#see how these all compare
compare <- compare_performance(mod,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,
                                     rank=TRUE)
compare
#mod 3 7 and 4 are equally as good 
#I actually made model 4 exactly the same as model 7 oops! 
#mod 4 is probably the simplest so I will choose that one

#I want to add predictions to df using model 4
df <- add_predictions(df, mod4, type='response')

fig1 <- df %>% ggplot(aes(x=gpa, y=pred, color=rank))+geom_smooth()
fig1

fig2 <- df %>% ggplot(aes(x=gre, y=pred, color=rank))+geom_smooth()
fig2

fig1+fig2
ggsave("grad_school_predictions.png")

#conclusions 
#basically if you are a rank 4 you have the least chance overall (according to this model)
#followed, by rank 3
#rank 1 and 2 have much better chances overall
#rank 1 with high gpas and high gres have the best chance this drops off steeply below
#a 3.5 gpa
#rank 2 have a much less chance overall than rank 1 and theres a pretty 
#consistent wane down as GPA goes down 
#theres a lot of conclusions that can be drawn just from looking at this 

#all 3 variables, gpa, gre and rank influence acceptance to grad school 

#all there is left to do is make a markdown report 











###### just a quick note, my computer decided to lose its mind halfway 
#through generating an html report and a lot of the coding in here died with it....
#that being said it was my bad not to save, and this will be a lesson to me :( but all the 
#extra coding I did on here is definetly in the html report!!