library(tidyverse)
library(palmerpenguins) 
library(ggthemes) #colorblind friedly color schemes! 

#the penguins data frame 
palmerpenguins::penguins

#save as an object 
penguins = palmerpenguins::penguins

#take a look 
glimpse(penguins)
#notice the different variables and begin to ask yourself questions abou the data
#what is the relationship between flipper length and body mass?
#what about by species?

#you can analyze the numbers but graphs are much, much cooler -- obviously.
#reminder because you are an idiot -- variables = columns and objects = rows. 

#open a cool interactive data table
view(penguins)

#we can view the individual column vectors like so 

penguins$species
penguins$island
#etc etc 

#to create a ggplot start with the basic function and then add layers. 
#the basic function is ggplot(data = ... aes(...))+ geom_[](...)
#aesthetics maps certain variables to elements of the chart, color, line, x and
#y axis, etc. Geom_ functions are the geometric objects cast as layers to 
#visualize the data. 

plot_1=ggplot(data=penguins, aes(x=flipper_length_mm, y=body_mass_g))+ geom_point()
plot_1
#this plot shows body mass vs flipper length 
#but can we make it more descriptive? 
#add more aesthetic layers! = more variables represented (pictorially of course!)

plot_2=ggplot(data=penguins, aes(x=flipper_length_mm, y=body_mass_g, color=species))+
geom_point()
plot_2
#color/shape mapping works better for categorical variables 

#what if we want to see overall trends? Well, that would be another geometric representation!
plot_3=ggplot(data=penguins, aes(x=flipper_length_mm, y=body_mass_g, color=species))+
geom_point()+geom_smooth(method='lm', se = FALSE)
plot_3
#geom smooth gives us a linear model. Because the data has not been redefined,
#it is still organized by the same aesthetics 
#se = FALSE removes the standard error around lines (kinda ugly)

#what if we want to see the linear trend for the entire dataset (not by species)
#map the species to the point plot only 
plot_4=ggplot(data=penguins, aes(x=flipper_length_mm, y=body_mass_g))+
  geom_point(aes(color=species))+geom_smooth(method='lm', se = FALSE)
plot_4
#each geometric object can have its own set of aesthetics. 

#just mapping species to color may make it hard to differentiate for color 
#blind people, we can also differentiate species by shape 
plot_5=ggplot(data=penguins, aes(x=flipper_length_mm, y=body_mass_g))+
  geom_point(aes(color=species, shape=species))+geom_smooth(method='lm', se = FALSE)
plot_5

#we can add labels like titles and subtitles 
#you know, being professional is probably a good plan!
plot_6=ggplot(data=penguins, aes(x=flipper_length_mm, y=body_mass_g))+
  geom_point(aes(color=species))+geom_smooth(method='lm', se = FALSE)+
  labs(x="Flipper Length (mm)", y="Body Mass (g)", title = "Body Mass and Flipper Length",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo", 
       color="Species", shape="Species")
plot_6

#note that you can also change the labels on the key with labs(), giving you a 
#chance to differentiate between color and shape 

#finally lets add our colorblind friendly scale color 
plot_final=ggplot(data=penguins, aes(x=flipper_length_mm, y=body_mass_g))+
  geom_point(aes(color=species))+geom_smooth(method='lm', se = FALSE)+
  labs(x="Flipper Length (mm)", y="Body Mass (g)", title = "Body Mass and Flipper Length",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo", 
       color="Species", shape="Species") +
  scale_color_colorblind()
plot_final

#more practice 
glimpse(penguins)

#make scatterplot of bill depth vs bill length
ggplot(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm))+geom_point()
#pay attention to errors, they are very descriptive!
#fix accordingly


