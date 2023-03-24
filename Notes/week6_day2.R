library(readxl)
library(tidyverse)
library(ggplot2)
readxl::read

#read in data 
file <- "./Data/exp_3_excel.xlsx"

#create seperate data frames from excel
run1 <- read_xlsx(file, range = "A8:B573", col_names = FALSE)
run2 <- read_xlsx(file, range = "D8:E846", col_names = FALSE)

#add column names that is what you want
names(run1) <- c("time_s", "temp_c")
names(run2) <- c("time_s", "temp_c")

#create new row that is row one or row 2
run1$run <- "run_1"
run2$run <- "run_2"

#fully join the two data frames by the variables
df <- full_join(run1,run2)
#make a new cleaned csv
write.csv(df, "./Data/exp_3_cleaned.csv")


df %>% ggplot(aes(x=time_s, y=temp_c, color=chemical))+
  geom_point()

#process text data instead
txt_path <- "./Data/exp_3_text"
txt <- readLines(txt_path)
grep(x = txt, pattern = "Vernier")
#find lines that have "vernier"
grep(x = txt, pattern = "Vernier")


#make another column for when run_1 it means lauric acid and when run_2 = mixture
df <- df %>% 
  mutate(chemical = case_when(run=="run_1"~"Lauric Acid", run=="run_2"~"Mixture"))

glm(data =df,
    formula=temp_c~chemical+time_s) %>% 
  report::report()

