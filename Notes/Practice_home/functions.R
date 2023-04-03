#factors practice January 29, 2023
library(tidyverse)
library(gapminder)
data("gapminder")
df = gapminder
head(gapminder)
#exercise 1
levels(df$continent) <- c(levels(df$continent), 'Antarctica')
levels(df$continent)
 
#exercise 2
levels(df$continent) <- c(levels(df$continent), 'North America', 'South America', 'Central America')
south <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Paraguay", "Peru", "Uraguay", "Venezuala")
north <- c("United States", "Canada", "Mexico")
central <- c("Costa Rica", "El Salvador", "Guetemala", "Honduras", "Nicaragua", "Panama")
levels("South America" == "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Paraguay", "Peru", "Uraguay", "Venezuala"))
df[df$country == c("Argentina","Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Paraguay", "Peru", "Uraguay", "Venezuala")]  

  