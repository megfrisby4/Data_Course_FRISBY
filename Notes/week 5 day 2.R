library(tidyverse)
library(palmerpenguins)
penguins
class(penguins$body_mass_g)
mean(penguins$body_mass_g) #there is NAs inroduced so the mean will not work
#need to remove the na's with na.rm

mean(penguins$body_mass_g, na.rm=TRUE)
#TADA

adelie <- penguins[penguins$species=="Adelie",]
chinstrap <- penguins[penguins$species == "Chinstrap",]
gentoo <- penguins[penguins$species=="Gentoo",]
#subset to make a data frame matching each of the values

adelie$body_mass_g %>% mean(na.rm=TRUE)
chinstrap$body_mass_g %>% mean(na.rm=TRUE)
gentoo$body_mass_g %>% mean(na.rm=TRUE)
#can a forloop do this?
means <- numeric()
for (i in unique(penguins$species) %>% as.character()) 
  {ss <- penguins[penguins$species==i,]
ss$body_mass_g %>% mean(na.rm = TRUE) %>% print()
means[i] <- ss$body_mass_g %>% mean(na.rm=TRUE)}
unique(penguins$species) %>% as.character()

#dplyr 
penguins %>% group_by(species,sex) %>% 
  summarize(mean_body_mass= mean(body_mass_g, na.rm=TRUE))

test <- penguins %>% filter(!is.na(sex))%>% group_by(species,sex) %>% 
  summarize(mean_body_mass= mean(body_mass_g, na.rm=TRUE),
            fattest_penguin = max(body_mass_g,
            na.rm=TRUE)) %>% 
test$species <- factor(test$species, levels= c("Gentoo", "Chinstrap", "Adelie"))




test$species %>% unique()

levels(test$species)= c("Gentoo", "Chinstrap", "Adelie")
#overwrite levels of a factor with new order
  test %>% ggplot(aes(x=species, y=fattest_penguin, fill=sex))+ geom_col(position = "dodge")

penguins$sex %>% unique()

## nifty packages = skimr  skim(penguins)





#desc descending order
#arrange use a column
#summarize give it a new name for a columne and what that column equals should 
#equal an equation/command 
#filter give you rows you want to keep 

#dply verbs 
#group_by, summarize
#arange
#filter (filters ROWS!!!)





#unique penguins$species = a factor that sucks!!!


#filter penguins where sex is NOT NA& where penguin body mass >4000
exercise <- penguins %>% filter(!is.na(sex)&penguins$body_mass_g>4000&year==2007)
exercise$sex %>% unique()# just checking to make sure I did that right
exercise$body_mass_g<4000 #samesies
getwd()
