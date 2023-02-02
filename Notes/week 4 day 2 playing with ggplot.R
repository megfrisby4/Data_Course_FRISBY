#playing with ggplot 
library(tidyverse)
#install.packages ("Palmerpenguidns")
install.packages("palmerpenguins")
library(palmerpenguins)
install.packages("GGally")
library(GGally)
penguins %>% names()

#find your outcome variables (dependent variables)
#           flipper_length_mm

#what predictor variables?? (independent variables)
#island, species, sex
penguins$year %>% unique()
#data collected 2007 2008 2009

ggpairs(penguins)
#:: from GGally package use ggpairs
#gives you how all variables are related to eachother

?ggplot
penguins %>% # data to use
  ggplot(aes(y=flipper_length_mm, x=island)) + # set 'global' mapping # which variables to which aspects of the plot
  # y = flipper length depends on x = 
  #map variables to different parts of plot
  geom_boxplot()

penguins %>% 
  ggplot(aes(x=flipper_length_mm, fill = species))+ 
  geom_density(alpha=.5) + facet_wrap(~sex*island)
#distribution of data (density)
#alpha = opacity

#to get rid of NA
penguins$sex %>% unique()
#yep got NA
!is.na(penguins$sex)
#gets us not NAs are true
penguins[!is.na(penguins$sex),]
#subset it back into data set

p <- penguins[!is.na(penguins$sex),] %>% 
  ggplot(aes(x=flipper_length_mm, fill = species))+ 
  geom_density(alpha=.5) + facet_wrap(~sex*island)
#facet_wrap(~island)


penguins %>% ggplot(aes(y=flipper_length_mm, x=island, fill = species))+
  geom_violin()

#linetype can show the line type but its horrible

#data 
#how to map variables (column names) to parts of a plot)
#what to draw?
#facets?
#scales?
#customize theme

p+theme_bw()
names(penguins)

#make a plot of body_mass_g with some x axis
penguins[!is.na(penguins$sex),] %>% ggplot(aes(y=body_mass_g, x=island, fill=species)) +geom_violin() +
  facet_wrap(~sex*island)

penguins[!is.na(penguins$sex),]  %>% 
  ggplot(aes(x=flipper_length_mm, y=body_mass_g, color = sex))+
  geom_point(size=3,alpha=.1)+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~species)+
  theme_void()

#you can search about anything!
?geom_point
p <- penguins[!is.na(penguins$sex),] %>% ggplot(aes(x=bill_length_mm, y=body_mass_g, color=species, linetype=sex))+
  geom_point(alpha=.5, size=5)+ 
  geom_smooth(method="lm",se=FALSE) + facet_wrap(~sex*island)

#method = lm = linear model
#se = error 

## you can set aesthetics inside of each genome


p+
  theme_minimal()+ 
  theme(axis.title = element_text(face = "bold" ))+
  labs(x="Bill Length (mm)", y= "Body Mass (g)",
       color = "Species", 
       title = "Penguin Body Mass .... Obvioulsy",
       subtitle = "Whatever. Just stop now",
       caption = "Data From Palmer Penguins R Package")+
  scale_color_manual(values = c("pink", "magenta", "purple"))

#now that youve got plot add to it
#use color finder on google for more customized color options!

library(tidyverse
)
library(palmerpenguins)
#make nifty penguin plot
p <- penguins[!is.na(penguins$sex),] %>% ggplot(aes(x=species, y=body_mass_g, color = island, fill = species))+
  geom_boxplot() + facet_wrap(~sex)
ggsave("./penguin.png", width = 5, height = 7, dpi = 300)
getwd()
#saves last plot 
p2 <- p + 
  labs(x = "Species", y = "Body Mass (g)", Color = "Island")+ 
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = .5, face = "italic"),
        axis.text.y= element_text(angle = 
                                    180, size = 20, face = "bold", color = "pink"),plot.background =  element_rect(color="orange", fill = "blue", linewidth = 15, linetype = "dotted"), 
        panel.grid.major.x = element_line(color= "orange", linetype = 4,
                                          lineend = "butt", linewidth = 3), strip.background = element_rect(fill= "pink"))
ggsave(p2, filename= "./ugly.png", height = 6, width = 6)

#h just 0 = left aligned h just = 1 = right aligned

p2+scale_fill_manual(values=c("purple", "green", "black")) + #scale_y_reverse()+
  coord_polar() + theme(strip.text = element_text(size= 12, face = "bold"), legend.text = element_text(face='italic'), 
                        legend.background = element_rect(fill="pink"), legend.position = "none", axis.title = element_text(
                          angle = 60, size = 40
                        ))
# + coord_flip() + coord_cartesian(ylim = c(100,3000)) 
# gg image
penguins %>% ggplot(aes(x=body_mass_g, y=flipper_length_mm)) + 
  geom_image("")
