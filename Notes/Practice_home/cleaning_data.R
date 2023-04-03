library(tidyverse)
library(janitor)
library(skimr)
df <- read_csv("./Data/Bird_Measurements.csv") %>% clean_names()
  
male <- df %>% select(starts_with("m_"), egg_mass, 
              species_name, english_name, clutch_size, 
              -ends_with("_n"), species_number, mating_system) %>% 
  mutate(sex="male")

unsexed<- df %>% select(contains("unsexed_"), egg_mass, 
                      species_name, english_name, clutch_size, 
                      -ends_with("_n"), species_number, mating_system) %>% 
  mutate(sex="unsexed")

female <- df %>% select(contains("f_"), egg_mass, 
                        species_name, english_name, clutch_size, 
                        -ends_with("_n"), species_number, mating_system) %>% 
  mutate(sex="female")

names(male) <- names(male) %>% str_remove("m_")
names(female) <- names(female) %>% str_remove("f_")
names(unsexed) <- names(unsexed) %>% str_remove("unsexed_")

clean <- full_join(male, female) %>% full_join(unsexed) %>% arrange(english_name)

clean <- clean %>% 
  mutate(mass=as.factor(mass),
         wing = as.factor(wing), 
         bill = as.factor(bill))
clean
 #male female unsexed, wing, mass, tarsus, bill tail 
#make multiple dfs
#clean the column names first




df <- read_csv("./Data/FacultySalaries_1995.csv") %>% 
  clean_names() 
df1 <- df %>% select(-ends_with("_all"), ends_with("_salary"), 
                    univ_name, state, tier, -ends_with("_comp"), -starts_with("num_"), -fed_id)
  names(df1)=names(df1) %>% str_remove("_salary") %>% str_remove("avg_")
df2 <- df1 %>% pivot_longer(ends_with("_prof"), names_to = "rank", values_to = "salary")

ant <- read_csv("./Data/thatch_ant.csv")
 
