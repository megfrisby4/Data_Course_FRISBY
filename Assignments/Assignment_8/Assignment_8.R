#Make a new Rproj and Rscript in your personal Assignment_8 directory and work from there.
#Write a script that:
 # loads the “/Data/mushroom_growth.csv” data set
#creates several plots exploring relationships between the response and predictors
#defines at least 4 models that explain the dependent variable “GrowthRate”
#calculates the mean sq. error of each model
#selects the best model you tried
#adds predictions based on new hypothetical values for the independent variables used in your model
#plots these predictions alongside the real data

library(readr)
library(tidyverse)
library(easystats)
library(modelr)
library(ggplot2)

df <- read_csv("Assignment_8_data/mushroom_growth.csv")
df
glimpse(df)
#plots exploring relationships between response and predictors

#plot 1  relationship between species and growth rate
plot1 <- ggplot(df, aes(x=Species, y=GrowthRate, color=Species))+
  geom_boxplot()+
  theme_minimal()+
  theme(legend.position = "none")

ggsave(filename = "./Assignment_8_figures/species_vs_growthrate.png", width=6, height = 4)

#plot 2  relationship between nitrogen, humidity and growth rate
#it should be pointed out here that the SE for the lines is very large
#the lines dont fit the data perfectly but we get the picture
#it appears there is a slight increase at growth rate around Nitrogen = 20 for both humidities
#it should also be noted that the high humidities have much higher growth rates than low
plot2 <- ggplot(df, aes(x=Nitrogen, y=GrowthRate, color=Humidity))+geom_smooth(se=TRUE)

ggsave(filename="./Assignment_8_figures/nitrogen_and_humidity_vs_growthrate.png", width=6, height = 4)

#plot 3 - relationship between species light and growthrate
#it appears that the P.cornucopiae species has a higher growth rate in general
#for both species as light goes up so does growth rate 
#light is more strongly correlated with growthrate for P.cornucopiae (steeper line)
plot3 <- df %>% group_by(Species) %>% 
  ggplot(aes(x=Light, y=GrowthRate, group=Species, color=Species))+
  geom_smooth(method="lm", se=FALSE)
ggsave("./Assignment_8_figures/light_and_species_vs_growthrate.png", width=6, height=4)

#Im going to get a little simpler now
#I think that species has probably the most profound effect so I will color by species
#and include one other influencing variable

#plot 4 
df %>% names()
#look at temperature influence
plot4 <- df %>% ggplot(aes(x=Temperature, y=GrowthRate, color=Species))+
  geom_smooth(method='lm', se=FALSE)

ggsave("./Assignment_8_figures/species_and_temp_vs_growthrate.png", width=6, height = 4.5)
#this is really intersting it looks like for P. cornucopiae as temperature increases
#growth rate decreases, but for P. ostreotus as temperature increases growth rate increases
#I wonder how this compares to humidity

#plot 5

plot5 <- df %>% ggplot(aes(x=Humidity, y=GrowthRate, color=Species))+geom_boxplot()

ggsave("./Assignment_8_figures/humidity_and_species_vs_growthrate.png", width = 6, height=4.5)

#this plot is very interesting it looks like for high humidity, the growth rate of P.cornucopiae
#is a bit higher than the other but for low the growth rate distribution is about the same

plot6 <- df %>% mutate(Temperature=as.factor(Temperature)) %>% 
  ggplot(aes(x=Humidity, y=GrowthRate, color=Species))+geom_boxplot()+
  facet_wrap(~Temperature)

ggsave("./Assignment_8_figures/humidity_species_temperature_vs_growthrate.png", height=4.5, width=6)
#heres something.....
#looks like when the temperature is 20, and humidity is high the growth rate of P.cornucopiae is much
#higher for it or the other species at any temperature and humidity, but in general 
#higher humidity means higher growth rate 


#I think Ive done enough exploring at this point! ... hopefully 

#models that explain growth rate
mod1 <- glm(data=df, formula=GrowthRate~Species)
summary(mod1)
df %>% names()              
mod2 <- glm(data=df, formula=GrowthRate~Species+Nitrogen+Temperature+Light+Humidity)
summary(mod2)
mod3 <- glm(data=df,formula=GrowthRate~(Nitrogen+Temperature+Light+Humidity)*Species)
summary(mod3)
mod4 <- glm(data=df, formula=GrowthRate~Species*Light*Temperature*Humidity*Nitrogen)
summary(mod4)
mod5 <- glm(data=df, formula=GrowthRate~(Temperature+Humidity)*Species)
summary(mod5)
mod6 <- glm(data=df, formula=GrowthRate~Temperature*Light*Species)
summary(mod6)
mod7 <- glm(data=df, formula=GrowthRate~(Temperature*Light*Humidity*Species)+Nitrogen)
summary(mod7)
mod8 <- glm(data=df, formula=GrowthRate~Temperature*Light*Humidity*Species)
mod9 <- glm(data=df, formula=GrowthRate~Species*Humidity*poly(Temperature, Light,Nitrogen)*
              Temperature*Light*Nitrogen)
#Mean squared error of each model
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)
mean(mod4$residuals^2)
mean(mod5$residuals^2)
mean(mod6$residuals^2)
mean(mod7$residuals^2)
mean(mod8$residuals^2)
mean(mod9$residuals^2)



#I tried, I'm not sure how to make these much better... the best model so far is mod 9 
compare_performance(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9) %>% plot()

#according to this plot its also mod9 as the best
#the makes sense because mod9 has the least value residual 

#adds predictions based on new hypothetical values for the independent variables used in your model
#plots these predictions alongside the real data

#add predictions and see if they are close to acutality
shroom <- df %>% 
  add_predictions(mod9)
shroom %>% dplyr::select("GrowthRate", "pred")

pred

#hypothetical values that R will predict from 
newdf <- data.frame(Nitrogen=c(7,18,23,33,41), Species=c("P.ostreotus", "P.ostreotus", 
                                                         "P.cornucopiae", "P.cornucopiae",
                                                         "P.ostreotus"),
                    Humidity=c("Low", "High", "Low", "Low", "High"), 
                    Temperature=c(20,25,25,20,21), 
                    Light=c(0,11,13,20,5))
#making predictions
pred=predict(mod9,newdata=newdf)

#combine predictions and hypothetical values into a dataframe
hyp_pred <- newdf %>% mutate(pred=pred)

#add new column showing whether a data point is real or hypothetical
shroom$PredictionType <- "Real"
hyp_pred$PredictionType="Hypothetical"

#join real and hypothetical data with predicitons
fullpreds <- full_join(shroom,hyp_pred)

#plot on original graph
ggplot(fullpreds, aes(x=Nitrogen, y=pred, color=PredictionType))+
  geom_smooth(se=FALSE)+ geom_point(alpha=.5)+
  geom_smooth(aes(y=GrowthRate), color="black", se=FALSE)
  theme_minimal()+ labs(y="GrowthRate")
  
#I know this isn't GREAT but I could not get the residuals to work amazingly, so its ok
#I tried, Im not sure what to do to make this model better, maybe I am just not plotting it correctly?
  #it represents it just not GREAT but thats a model I guess, it represents the smooth trendline GREAT!!!
#thre predictions are ok but I would not trust them!!
  #but hey, I did the task I guess. I just sucked it up a little!
