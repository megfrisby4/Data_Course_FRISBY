# For exam 4 I am going to redo Exam 1!
#My reasons for doing are first to see how much I've improved, and second, my own sanity!




#Task 1 - Read the cleaned_covid_data.csv file into an R data frame. 

#pretty easy... here we go 
library(readr)
df <- read_csv("./cleaned_covid_data.csv")

#shaboom there we are 
df

#Task 2 - Subset the data set to just show states that begin with “A”
#and save this as an object called A_states. 

library(tidyverse)
A_states <- df[grepl("^A", x = df$Province_State, ignore.case = T),]

#a look at the data frame 
A_states

#Task 3- Create a plot of that subset showing Deaths over time,
#with a separate facet for each state.

#first change Last_Update column to date format so that the points can be graphed correctly
A_states$Last_Update <- as.Date(A_states$Last_Update)

#Then create a plot showing deaths overtime. X = time, Y = deaths
A_states %>% ggplot(aes(x=Last_Update, y = Deaths)) +
  geom_point()+ 
  geom_smooth(method="loess", se=FALSE)+
  facet_wrap(~Province_State,scales = "free") + labs(x="Time", y = "Deaths")

#save that as a png
ggsave("./Generated_Figures/deaths_over_time.png", height = 8, width = 10)




#task 4 - Find the “peak” of Case_Fatality_Ratio for each state and save this
#as a new data frame object called state_max_fatality_rate.

#first group by Province_State
#then summarize to condense to one row for each group (1 max for each state)
#create a new column that is Maximum_Fatality_Ratio (summarize does this like mutate)
#make Maximum_Fatality_Ratio equal to the maximum value of the 
#Case_Fatality_Ratio column for each group (state).
#arrange in descending order of Maximum Fatality Ratio
#save as an object (data frame) called state_max_fatality_rate
#this can all be done with one pipeline which is pretty coolio!
state_max_fatality_rate <- df %>% group_by(Province_State) %>% 
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm=TRUE)) %>% 
  arrange(desc(Maximum_Fatality_Ratio))

#my new data frame
state_max_fatality_rate 



#task 5 - Use that new data frame from task IV to create another plot.

#X = Province_State; Y = Maximum_Fatality_Ratio

#first I need to make the Maximum Fatality Ratio column a factor so it stays in order
#this can be accomplished with mutate (aka my best friend)
state_max_fatality_rate <- state_max_fatality_rate %>% 
  mutate(Province_State = factor(Province_State, 
                                levels=Province_State))

#as you can see the Maximum_Fatality_Ratio is now a factor
state_max_fatality_rate

#Now create a bar graph in descending order of max fatality rate
#Axis labels rotated to 90 degrees for readability
#fun colors for the hell of it 
state_max_fatality_rate %>% 
  ggplot(aes(x=Province_State, y = Maximum_Fatality_Ratio, fill=Province_State))+ 
  geom_bar(stat = 'identity')+ 
  theme(axis.text.x = element_text(angle=90), legend.position = "none")+
  labs(x = "Province Or State", y = "Maximum Fatality Ratio")

#save as ggplot
ggsave("./Generated_Figures/maximum_fatality_rate.png", width=10, height = 8)



#Task 6 - Bonus!
# Plot cumulative deaths for the entire US over time
#I changed the last update column to date format in the A_states dataframe but not 
#in the entire data frame.
#I will do that now
df <- df %>% mutate(Last_Update = as.Date(Last_Update))

#as you can see it is now a date
df

#make a new data frame to represent cumulative sum of deaths in us over time
#group by date (sometimes there is two observations for each)
#summarize to condense to a single row for each group (date)
#summarize by the cumulative sum in the Deaths column 
#arrange by the date (ascending)
us_cumulative_deaths <- df %>% group_by(Last_Update) %>% 
  summarize(cumulative_deaths = cumsum(Deaths)) %>% arrange(Last_Update)
#it throws a warning, but I know that it is right and in order 
#it will also be apparent in the graph if it isnt't
#that is why I arranged by date after the summarize
us_cumulative_deaths

#graphy graph
#x = date 
#y = cumulative deaths 
#geom smooth - just for fun (and so the y scales are better)
#electric green because why not!
us_cumulative_deaths %>% 
  ggplot(aes(x=Last_Update, y=cumulative_deaths))+
  geom_smooth(stat = 'identity', color = "green", se = F)+
  labs(x="Date", y= "Total Covid Deaths in The USA")+ theme_minimal()

