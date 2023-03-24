#add picture to gglplot
library(tidyverse)
library(palmerpenguins)
library(ggimage)
library(ggpubr)
library(gapminder)
library(gganimate)
library(plotly)

#install gganimate
#install ggpubr 
#install plotly - graphs interactive
#install vegan
#install ggmap
#install leaflet - streetmaps

packageurl <- "https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz" 
install.packages(packageurl, repos=NULL, type="source")

penguins %>% ggplot(aes(x=flipper_length_mm, y=body_mass_g))+
  #geom_point()
geom_image(aes(image="./penguin.png"))

#adding image to column chart
df2 <- data.frame(species=c("Adelie", "Chinstrap", "Gentoo"), body_mass_g=rep(2e05, 3))

penguins %>% ggplot(aes(x=species, y=body_mass_g))+ 
  geom_col(color="red")+
  geom_image(data=df2, aes(image="./penguin.png"), size=.15)

p <- penguins %>% ggplot(aes(x=flipper_length_mm, y=body_mass_g, color=species))+
  geom_point()

p <- p+ transition_time(year)
\

penguins$chunky <- penguins$body_mass_g>5000
penguins %>% ggplot(aes(x=flipper_length_mm, y=body_mass_g, color=chunky))+
  geom_point()+
  geom_hline(yintercept = 5000, color="red")+
  annotate("text", x=190, y=5200, label="Chunky")+
  labs(color="CHUNKY")+
  scale_color_manual(values=c("black", "red"))
  
?geom_hline
  