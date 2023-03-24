#load packages
library(tidyverse)

#look at example data
table1
table2
table3
table4a
table4b
table5

table2 %>% pivot_wider(names_from = type, values_from = count)
table3 %>% separate(col=rate, into = c("cases", "population"), convert = TRUE)
table4a %>% pivot_longer(cols=-country, names_to = "year", values_to = "cases")
table4b %>% pivot_longer(cols=-country, names_to="year", values_to = "population")
table5 %>% mutate(newyear=paste0(century, year)) %>% separate(col=rate, into =c("cases", "population"))
#tidy rule 1: every variable is a SINGLE column
#no column has more than one variable

#tidy rule 2: every observation gets its OWN row
#columns are variables and rows are observations
#ALWAYS rectangular 


table2
#need to seperate 'type' to columns cases, population
table2 %>% pivot_wider(names_from = type, 
                       values_from = count)
#names from = where do i find names for new column
#where to get values to put in those new columns

#pivot wider is for when multiple variables in a single column

#pivot longer is when is for when single variable in multiple columns

table4a %>% 
  pivot_longer(cols=-country,names_to="year",
               values_to="cases")

table4b %>% 
  pivot_longer(cols=-country, names_to="year",
               values_to="population")

table3 %>% 
  separate(col=rate, 
           into=c("cases", "population"),
           convert=TRUE)
#if separated by specific character type sep="   "


table5 %>%
  mutate(newyear=paste0(century,year) 
         %>% as.numeric()) %>% 
  select(-c(century, year)) %>% 
  separate(col=rate, 
           into = c("cases", "population"), 
           convert=TRUE) %>% 
  select(country, newyear, cases, population)

#mutate new columns based on existing columns
#paste0 = sticks stuff together with no spaces

df <- read_csv("./Data/wide_income_rent.csv")%>% 
  pivot_longer(col=-variable,
    names_to="state") %>% 
  pivot_wider(names_from=variable,
              values_from=value)



df %>% pivot_longer(col=-variable,
                    names_to="state") %>% 
  pivot_wider(names_from=variable,
              values_from=value)
#pivot longer first, then pivot wider
#go back copy paste and save to variable above



  
