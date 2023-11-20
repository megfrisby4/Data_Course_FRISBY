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

#no apparent relationship

#scatterplot of species vs bill depth 
ggplot(data=penguins, aes(x=species, y=bill_depth_mm))+geom_point()
#really not that exciting, bill depth varies greatly by species
#a better choice for geom might be a boxplot
ggplot(data=penguins)+geom_boxplot(aes(x=species, y=bill_depth_mm, color=sex), na.rm = T)

#plot of bill flipper length vs bill depth 
plot_7=ggplot(data=penguins, aes(x=flipper_length_mm, y=body_mass_g,color = bill_depth_mm))+
  geom_point()+geom_smooth(se = F)
plot_7



##Working with categorical variables (bar charts)


#visualizing data with categorical variables
#only takes on one of a small set of values
#usually quantitative
#use bar chart

bar_1 <- penguins %>% ggplot(aes(x=species))+geom_bar()
bar_1

#its often preferred to reorder the bars based on frequencies
#to do so, reorder as factor

penguins %>% ggplot(aes(x=fct_infreq(species)))+geom_bar()
#fct_infreq argument factors by appearance... which is wicked!




##numerical variables (histograms, density charts, etc.)


#numerical variables (quantitative) can take on a wide range of values
#sensible to add subtract or take averages of values
#can be continuous or discrete

###histograms

#histograms often use to visualize continuous variables
hist_og <- penguins %>% ggplot(aes(x=body_mass_g))+
  geom_histogram(binwidth=200)
hist_og

#a histogram divides the x axis equally into bins (you choose the size)
#and then reports the frequency of value in each bin 
#very informative!

#make sure to set good binwidths to get a reasonable sense of the distribution
#for example 
hist_1 <- penguins %>% ggplot(aes(x=body_mass_g))+
  geom_histogram(binwidth=20)
hist_1
#this histogram sucks ass, literally tells no good info

hist_2 <- penguins %>% ggplot(aes(x=body_mass_g))+
  geom_histogram(binwidth=2000)
hist_2
#this histogram also sucks ass for the same reasons

hist_og
#this on was nice! 




###density plots

#basically a smoothed out histogram
#visualize distribution
density_1 <- penguins %>% ggplot(aes(x=body_mass_g))+geom_density()
density_1


#more practice 
bar_2<- penguins %>%ggplot(aes(y=species))+geom_bar()
bar_2
#fancy horizontal bar chart 

#exploring colors
penguins %>% ggplot(aes(x=species))+
  geom_bar(fill="red")
penguins %>% ggplot(aes(x=species))+
  geom_bar(color='blue')
#fill is way more useful!
#color and fill are not aesthetics by the way!
#learned that one the hard way!

diamonds <- diamonds
view(diamonds)
glimpse(diamonds)

hist_3 <- diamonds %>% ggplot(aes(x=carat))+geom_histogram(fill='green', binwidth = .2)
hist_3
#binwidth of around .2 is pretty good 



#visualizing relationships between 2+ variables

#a numerical and categorical variable
#use back to back boxplots
#good at detecting outliers/summarize the data

box_1 <- penguins %>% ggplot(aes(x=species, y=body_mass_g))+geom_boxplot()
box_1
#use for categorical vs. numerical variable

#you can also make overlapping density plots
density_2 <- penguins %>% ggplot(aes(x=body_mass_g, color=species, fill=species))+
  geom_density(alpha=0.5)
density_2
#alpha = transparency 

#map variables to aesthetic if we want visual attribute represented to vary
#base on values of that variable
#otherwise set the variable (don't map) -- dont place inside aes()

