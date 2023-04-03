install.packages("ggimage")
library(tidyverse)
library(ggimage)
names(iris)
iris %>% ggplot(aes(x=Sepal.Length, y=Sepal.Width))+geom_image(aes(image = "./download.png"), size = .1)
p <- iris %>% ggplot(aes(x=Sepal.Length, fill=Sepal.Length>5), fill = Sepal.Length>5)+geom_bar(aes())+facet_wrap(~Species, scales = "free")
p+ theme(legend.position = "none", strip.background = element_rect(fill="pink"),plot.background = element_rect(fill="blue"),
         panel.background = element_rect(fill="yellow"), panel.grid = element_line(color="purple", linewidth = 1))
