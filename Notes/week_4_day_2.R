library(tidyverse
        )
library(palmerpenguins)
#make nifty penguin plot
penguins[!is.na(penguins$sex),] %>% ggplot(aes(x=species, y=body_mass_g, color = island))+
  geom_boxplot() + facet_wrap(~sex)
ggsave("penguins.png", width = 5, height = 7, dpi = 300)
getwd()
#saves last plot 