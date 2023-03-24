library(tidyverse)
library(janitor)
janitor::clean_names()
df <- read_csv("./Data/Bird_Measurements.csv") %>% 
  clean_names() %>%   select(-ends_with("_n"))

#first thing to do is pipe csv to janitor:: clean_names


#clean names make into lower case with underscores
df %>% 
clean_names() %>% 
  select(-ends_with("_n"))


c("YouR Mom", "$ per day", "# of people", "gROUP") %>% 
  make_clean_names()
"SnakeCase"
"camel_case"

# # changes to number 
"#" %>% make_clean_names()

#brainstorming ideas to clean data 
#need sex, tarsus, bill, tail, etc.

#select is which columns to keep
#get rid of all columns with _n
df %>% 
  select(-ends_with("_n"))%>% #take and pipe to read in
  names() #check new nammes first for sanity!


#mass is spread out over three columns and should be one, pivot longer
#really complicated so make several separate data frames

maledf <- df %>% select(starts_with("m_"), 
                        species_number,  species_name, 
                        english_name, clutch_size, egg_mass,
              mating_system) #gives us male data frame
  #copy paste
unsexed <- df %>% select(starts_with("unsexed_"), 
              species_number,  species_name, 
              english_name, clutch_size, egg_mass,
              mating_system, family) %>% 
  mutate(sex="unsexed")

female <- df %>% select(starts_with("f_"), 
              species_number,  species_name, 
              english_name, clutch_size, egg_mass,
              mating_system, family) %>% 
  mutate(sex="female")

#start cleaning each data frame with pipes 
male <- df %>% select(starts_with("m_"), 
                        species_number,  species_name, 
                        english_name, clutch_size, egg_mass,
                        mating_system, family) %>% 
  mutate(sex="male") #mutate is how you add new column in data frame

#do it to each pipe above


names(male) <- # assign new names to names of male
  names(male) %>% str_remove("^m_") #str_remove = character string remove


names(female) <- 
  names(female) %>% str_remove("^f_") 

names(unsexed) <- 
  names(unsexed) %>% str_remove("^unsexed_") 


#now that all the major group data frames are cleaned all that is left to do is combine!!!

#join them back together
#full join only takes 2 data frames at a time


clean <- full_join(male, female) %>% 
  full_join(unsexed) %>% 
  mutate(family=as.factor(family), 
         mating_system=as.factor(mating_system), 
         species_number=as.factor(species_number))
  
clean %>% filter(!is.na(mating_system)) %>% #keep rows where mating system is not NA
  ggplot(aes(x=mating_system, y=bill))+
  geom_boxplot()


nrow(df)*3 #yes I am right!!

clean %>% filter(english_name=="Ostrich") # check to see if ostrich eggs are all the same mass


library(skimr)
skim(clean) #feed it dataset
#gives entire summary of dataset
 df %>% 
  filter(complete_rate > .25)
 
 
 
 
df <- read_csv("./Data/FacultySalaries_1995.csv") %>% 
  clean_names() %>% 
  select(-ends_with("_comp"), -ends_with("_comp_all"), -ends_with("_all"))

full_prof <- df %>% select(contains("full_prof_salary"), fed_id , univ_name,
                           state, tier)
assoc_prof <-  df %>% select(contains("assoc_prof_salary"),  fed_id , univ_name,
                             state, tier)
assist_prof <- df %>% select(contains("assist_prof_salary"), fed_id , univ_name,
                             state, tier)

clean <- full_join(full_prof,assoc_prof) %>% full_join(assist_prof)
#plot salary (not comp) by rank and tier 




df %>% 
  select(ends_with("_salary"), fed_id, univ_name, state, tier) %>% 
  pivot_longer(starts_with("avg_"), names_to = "rank", values_to = "salary", 
               names_prefix = "avg_") %>% 
  mutate(rank=rank %>% str_remove("_salary"), 
         tier = tier %>% factor(levels=c("I", "IIA", "IIB", "VIIB")))



"./Data/thatch_ant.csv" #PRACTICE!!!!!