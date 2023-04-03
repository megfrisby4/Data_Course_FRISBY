install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)
install.packages("gapminder", dependencies=TRUE)
library(gapminder)
data("gapminder")
df=gapminder
head(gapminder)

#exercise 1
levels(df$continent) <- c(levels(df$continent), 'Antarctica')

#exercise 2
levels(df$continent) <- c(levels(df$continent), 'North America', 'South America', 'Central America')

#exercise 3
as.character(df$continent)
str(df$continent)
df$continent[49:60] = "South America"
df$continent[133:144] = "South America"
df$continent[169:180] = "South America"
df$continent[241:252] = "South America"
df$continent [277:288] = "South America"
df$continent [301:312] = "South America"
df$continent[349:360] = "Central America"
df$continent[445:456] = "South America"
df$continent[469:480] = "Central America"
df$continent[601:612] = "Central America"
df$continent[649:660] = "Central America"
df$continent[985:996] = "North America"
df$continent[1105:1116] = "Central America"
df$continent[1177:1188] = "Central America"
df$continent[1189:1200] = "South America"
df$continent[1201:1212] = "South America"
df$continent[1609:1620] = "North America"
df$continent[1621:1632] = "South America"
df$continent[1633:1644] = "South America"
df$continent
df$continent <- as.character(df$continent)
df$continent
as.factor(df$continent)

#exercise 4
df$continent <- droplevels.factor(x=df$continent,'Americas', )
df$continent

#exercise 5
df$continent <- c(as.character(df$continent))
df$continent <- factor(df$continent)
ordered_continent <- factor(df$continent, levels=c("North America", "Central America", "South America", "Africa",
                                               "Asia", "Europe", "Oceania"), ordered = TRUE)
#exercise 6
table(ordered_continent)
table(df$continent)

#exercise 7
status <- c("Lo", "Hi", "Med", "Med", "Hi", 'Lo', "Hi", "Med", "Med", "Hi")
status <- factor(status)
ordered.status <- factor(status, levels = c ("Lo", "Med", "Hi"), ordered = TRUE)
table (status)
table(ordered.status)
as.numeric(status) +1

#exercise 8 
sub_status <- status[status %in% c("Lo", "Hi")] # extract only the Lo and Hi values 
sub_status
sub_status <- c("Lo", "Hi", "Hi", "Lo", "Hi", "Hi")
sub_status <- factor(sub_status)

#exercise 9 
status2 <- factor(c("Med", "Hi", "Lo", "Hi", "Medium High"))
status2
table(status2)
status3 <- c(as.character(status), as.character(status2))
status3 <- factor(status3)
status3

#exercise 10 
data("gapminder")
df = gapminder
ggplot(df, aes(x= reorder(continent, -lifeExp), y=lifeExp)) + geom_boxplot()

#exercise 11
ggplot(df, aes(x=year, y=pop))+geom_point()
class(df$gdpPercap)
gdpPercap= df$gdpPercap
factor(gdpPercap)
df$pop
str(df)
df$PopFactor <- cut(df$pop,5,labels= c("really low", "low", "med", "high", "really high"))
PopFactor <- df$PopFactor
PopFactor
df
ggplot(df, aes(x=year, y=PopFactor))+geom_point()

df$gdpPercapFactor <- cut(df$gdpPercap,6, labels = c("Hella Low", "Low", "Moderate", "Relatively High", "High",
                                                        "DAAAAMN"))
df$gdpPercapFactor -> gdpPercapFactor
df
ggplot(df, aes(x=year, y=gdpPercapFactor, color = gdpPercapFactor))+geom_point()
