library(tidyverse)
library(janitor)
library(skimr)
library(readxl)
library(ggplot2)


df <- read_xlsx("./Data/messy_data_1.xlsx") %>% clean_names()
names(df) <- c("date", "age_gp", "industry", "job_title", 
               "job_title_2", "salary", "monetary_comp", "currency", "other_currency", "income_cont",
               "country", "state", "city", "total_exp", "field_exp", "education", "gender", "race")


df <- df %>% mutate(industry=str_to_lower(industry),
              job_title=str_to_lower(job_title), 
                job_title = paste(job_title, job_title_2))
df$job_title <- df$job_title %>% str_remove(" NA")
df$salary <- as.numeric(df$salary)
df$monetary_comp <- as.numeric(df$monetary_comp)
df1 <- df %>% mutate(salary= monetary_comp+salary) %>% select(-c(date, job_title_2, monetary_comp,
             other_currency, income_cont, field_exp)) %>% mutate(country=case_when(currency=="USD"~"USA"))
replace(df$country, list = "US", "USA")
str_replace_all(df$country, pattern = "US", "USA")
str_replace(df$c)
df1$country <- str_to_lower(df1$country)
?case_when()
names(df)cou
?pivot_wider
df1$country
df$country
df$country=="us""USA"
