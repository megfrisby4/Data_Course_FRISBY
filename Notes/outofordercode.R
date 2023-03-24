library(tidyverse)
data("CO2")

means <- CO2 %>%
  group_by(Type,Treatment,conc) %>%
  summarise(MeanUptake = mean(uptake))
 


ggplot(means, aes(x=conc,y=MeanUptake,color=Treatment)) + 
  geom_point() +
  facet_grid(~Type)
  
