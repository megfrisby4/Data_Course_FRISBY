library(tidyverse)
library(janitor)
library(readr)
 
#task 1 read in unicef data 
#slightly clean it with clean names
#save it as object
read_csv("./unicef-u5mr.csv")
df <- read_csv("./unicef-u5mr.csv") %>% janitor::clean_names()

#look at it a bit
summary(df)
str(df)



#task 2 clean the data
#looks like the columns represent the year and the observations represent U5MR
#going to need a pivot longer
#new columns should be year and u5mr
#also need to remove u5mr_ prefix for year -can be done with mutate 
#make sure to save as an object again or that work was for literally nothing!
#also want to make year numeric --- mutate also!
#last im gong to change the name of the country_name to just country
df %>% names()
clean <- df %>% pivot_longer(cols= -c(country_name, continent, region), 
                    names_to = "year", values_to ="u5mr") %>% 
  mutate(year=str_remove(year, "^u5mr_"), year = as.numeric(year)) %>% 
  rename(country=country_name)
#take a look at the new df
clean
#want to make sure there aren't any weird capitalizations or misspellings
#that might affect my groups if i want to group later
clean$country %>% unique()
#looks good
clean$continent %>% unique()
clean$region %>% unique()
#its a little weird to have the region and the country
#i dont feel like we really need it but we might want to graph something
#by region later so ill just leave it for now!
clean #i think this is an adequate clean df



#task 3 plot each country's U5MR over time
library(ggplot2)
#plot U5MR (y) by year (x)
#line plot
#facet wrap by continent
#need to group by country first so a line is made for each country
plot1 <- clean %>% group_by(country) %>% 
  ggplot(aes(x=year, y=u5mr, group=country))+
  geom_line()+
  facet_wrap(~continent)+
  labs(y="U5MR", x="Year")
plot1



#task 4 save the plot
ggsave(filename = "./FRISBY_Plot_1.png", width = 6, height = 5)    


#task 5 create another plot showing mean U5MR for each continent over time 
#Create another plot that shows the mean U5MR for all 
#the countries within a given continent at each year 
#first we need to group the data and find the mean U5MR for each continent for each year
plot2 <- clean %>% group_by(continent, year) %>% 
  summarize(mean_u5mr=mean(u5mr, na.rm=TRUE)) %>% 
  ggplot(aes(x=year, y=mean_u5mr, color=continent))+
  geom_line()+
  labs(y="Mean U5MR", x="Year")
plot2           



#task 6 save the plot 
ggsave("./FRISBY_Plot_2.png", width=6, height=5)


library(easystats)
#task 7 create 3 models U5MR
clean %>% names()
mod1 <- glm(data = clean, formula=u5mr~year)
summary(mod1)
#accounts for year

mod2 <- glm(data = clean, formula=u5mr~year+continent)
summary(mod2)
#accounts for year and continent but NOT their interaction term

mod3 <- glm(data=clean, formula=u5mr~year*continent)
summary(mod3)
#accounts for year continent and their interaction term (denoted by ":")



#task 8 compare the three models by performance
compare_performance(mod1, mod2,mod3) 
#it appears mod3 has the largest r^2 and smallest RMSE 
#but lets graph it to see whats best :)
compare_performance(mod1, mod2,mod3) %>% plot()
#mod3 is DEFINETLY the best according to this graph!!!

#mod3 is the best because it has the best performance in all areas, R^2 value 
#meaning that the model has the most explanatory power or explains a larger 
#percent of the data compared to the other models, 
#it also has the smallest RMSE or root mean squared error
#which means the actual data has the least amount of differentiation from 
#the trend line that the model has created to represent it



#task 9 -- plot 3 models predictions
mods <- modelr::gather_predictions(clean,mod1,mod2,mod3) %>% 
  ggplot(aes(x=year, y=pred, color=continent))+
  geom_smooth(method="glm")+
  facet_wrap(~model)+labs(y="Predicted U5MR", x="Year", title = "Model Predictions")
mods
#saving it just for the heck of it
ggsave("./FRISBY_model_predictions.png", width=6, height = 5)


#task 10 -- extra credit!!!
clean %>% filter(country=="Ecuador")
#looking at what i need to have to make a new data frame 
ecu <- data.frame(continent="Americas", year=2020, country="Ecuador", 
                  region = "South America", u5mr=NA)
#creating a new data frame i want to make a prediction from
predict(mod3, newdata = ecu)
#doing a little bit of tranformation to get just ecuador
ecuador <- clean %>% filter(country=="Ecuador")
#making the prediction
ecuador_predict <- full_join(ecu, ecuador)
ecuador_predict <- ecuador_predict %>% modelr::add_predictions(mod3)
ecu_2020_prediction <- ecuador_predict %>% 
  filter(year==2020) %>%
  mutate(real=13, difference = real-pred)
#and here is the output
ecu_2020_prediction
#the prediction is -10 deaths -- doesn't make total sense but thats a model!! :)

#use a new model to make a better prediction
names(clean) 
#looking on what I can use as predictor variables
mod4 <- glm(data=clean, formula=u5mr~year+country)
#predicting as a function of year and country because that will be 
#more specific for ecuador
#plotting the performance of my new model against the previous
compare_performance(mod1, mod2,mod3,mod4) %>% plot()
#the fourth (new) model is SIGNIFICANTLY better than the others!!
#lets see how it stands up to predicting -- same process as before
Ecu <- data.frame(continent= "Americas", year = 2020,
                  country="Ecuador", region = "South America", u5mr=NA)
predict(mod4, newdata=Ecu)
better_ecuador_2020_prediction <- full_join(Ecu, ecuador)
#well just add predictions now and focus it down to the year 2020
#also the real value and the difference
better_ecuador_2020_prediction %>% modelr::add_predictions(model = mod4) %>% 
  filter(year==2020) %>% mutate(real=13,difference=real-pred)
