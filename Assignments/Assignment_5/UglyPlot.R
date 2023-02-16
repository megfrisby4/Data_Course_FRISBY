library(tidyverse)
library(ggimage)
library(gapminder)
library(ggpubr)
library(palmerpenguins)
penguins %>% names()
penguins %>% ggplot(aes(x=species, y=flipper_length_mm/bill_depth_mm))+ geom_linerange(ymin=10, ymax = 1000, color = "green")+
  geom_bgimage(image = "./media/dwight.jpeg")+
  geom_boxplot(aes(x=bill_depth_mm, y = flipper_length_mm), linetype=5, color = "yellow", fill="yellow")+
  geom_point(aes(x=bill_depth_mm, y = bill_length_mm, color=sex))+ 
  geom_jitter(aes(color=species, size =45, ))+ 
  geom_image(aes(image="./media/mike.png"),size = .1)+
theme(axis.text.x = element_text(vjust=1, hjust =-5))+
  geom_violin(aes(size = 10))+ 
  annotate("text", x=11, y=125, label =
  "Whenever I'm about to do something,
  I think,
  'Would an idiot do that?' 
  And if they would,
  I do not do that thing.", size= 5, color = "red")+
  theme(axis.text.y = element_text(face='bold.italic', color="blue", angle=165), 
        axis.title.y = element_text(color="purple", angle =67), 
        axis.title.x = element_text(vjust=2, color="green", size = 12, angle = 71), 
        legend.background = element_rect(fill="pink", linetype = 7, size = 12),
        legend.title = element_text(face="bold", angle = 85, hjust=1, vjust = -1), 
        legend.text = element_text(color="brown", size = 14, vjust=-3), 
        axis.text.x = element_text(color="yellow"))+
          labs(y="Bears, Beets, Battlestar Galactica")+
  geom_image(aes(x=6, y=200, image="./media/mike.png"),size = .2)
ggsave(filename = "./media/uglyplot.png", width=6, height = 5)
getwd()
