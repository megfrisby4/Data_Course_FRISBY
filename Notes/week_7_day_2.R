library(tidyverse)
library(janitor)
library(skimr)
library(readxl)
library(ggplot2)

file <- "./Data/messy_bp.xlsx"
df <- read_xlsx("./Data/messy_bp.xlsx", range = "A4:M24") %>% clean_names()
 df <-df %>% mutate(birth_date = 
   paste(day_birth, month_of_birth, year_birth, sep = "-"))
 df
   
  df <-  df %>% mutate(pat_id=1:nrow(df))
  df

 df$birth_date=as.Date(df$birth_date)
 df
 clean1 <- df %>% select(birth_date, pat_id, race, sex, hispanic, bp_8, hr_9, bp_10, hr_11, bp_12, hr_13) %>% 
  separate(col=bp_8, into = c("systolic_1", "diastolic_1"), sep = "/") %>% 
  separate(col=bp_10, into = c("systolic_2", "diastolic_2"), sep="/") %>% 
  separate(col=bp_12, into= c("systolic_3", "diastolic_3"), sep="/") %>% 
 rename(hr_1=hr_9, hr_2=hr_11, hr_3=hr_13) %>% mutate(hispanic= df$hispanic=="Hispanic",
                                                      race=str_to_lower(race), sex=str_to_lower(sex))
   clean1                                                 
  trial1 <- clean1 %>% select(-ends_with("_2"), -ends_with("_3")) %>% 
    rename(systolic=systolic_1, diastolic=diastolic_1, heartrate=hr_1) %>% 
    mutate(trial=1)
  
  
  trial2 <- clean1 %>% select(-ends_with("_1"), -ends_with("_3")) %>% 
    rename(systolic=systolic_2, diastolic=diastolic_2, heartrate=hr_2) %>% 
    mutate(trial=2)
  
  trial3 <- clean1 %>% select(-ends_with("_1"), -ends_with(("_2"))) %>% 
    rename(systolic=systolic_3, diastolic=diastolic_3, heartrate=hr_3) %>% 
    mutate(trial=3)
  
  clean <- full_join(trial1,trial2) %>% full_join(trial3)
    
  clean <- clean %>% mutate(systolic=as.numeric(systolic), diastolic=as.numeric(diastolic),
                   heartrate=as.numeric(heartrate),
                   birth_date=as.Date(birth_date),
                   trial=as.numeric(clean$trial), pat_id = as.numeric(pat_id))
  
clean %>% ggplot(aes(x=trial, y=systolic, fill=sex))+ geom_smooth()
clean %>% group_by(pat_id) %>% summarize(systolic, trial) 
  clean %>% group_by(pat_id) %>% summarize(systolic, trial, sex, pat_id) %>% 
    ggplot(aes(x=trial, y=systolic, fill=sex))+geom_smooth()

  
  clean %>% group_by(pat_id, trial, sex) %>% summarize(systolic) %>% 
    ggplot(aes(x=trial, y=systolic, color=sex))
  
  mutate(race=str_to_lower(race), sex=str_to_lower(sex), 
         hispanic=case_when(hispanic=="Hispanic"~ TRUE, TRUE~FALSE))
  
  clean %>% group_by(pat_id) %>% 
    ggplot()+geom_smooth(method="lm",aes(trial, systolic, group=pat_id, color=sex), se=FALSE)

  
  #blood pressure data frame
  #dont do by visits do by hr and bp 
  #but you can its ok you just arent future proof yet!!
  
  
  
  
  #google messy data set for R
  
  
  
  glm(data=clean, formula = systolic,)
  
  
