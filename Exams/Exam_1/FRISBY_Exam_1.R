library(tidyverse)
#task 1
#reading the cleaned covid data csv into a data frame
read.csv("./cleaned_covid_data.csv")
#making it a data frame (saving it as a data frame named df)
df <- read.csv("./cleaned_covid_data.csv")

#task 2
#subset the data to show only the state that begin with A, and save as A_states
A_states <- df[grepl(pattern = "^A", x = df$Province_State),]
#g rep picks patterns from a character vector. 
#df$Province_State is a character vector.
#^A means begins with A
A_states
#gotta make sure and yup its all there (it appears)

#task 3
A_states$Last_Update <- as.Date(A_states$Last_Update)
#change from a character vector to a date vector to plot 
#for some reason if I didn't do this the dates would not show up on the x axis 
#Create a plot of that A_states showing Deaths over time, with a separate facet for each state.
# deaths = y time = x
   A_states %>% ggplot(aes(x=Last_Update, y = Deaths)) +
     geom_point()+ 
     geom_smooth(method="loess", se=FALSE)+
     facet_wrap(~A_states$Province_State,scales = "free") + labs(x="Time")
   #I changed the x axis label to time because that just seemed nicer than "Last_update"
   
   
#task 4
 state_max_fatality_rate <- df %>% group_by(Province_State) %>% 
   summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio,na.rm=TRUE))
 # finding the max case fatality ratio for each state for each state
 #and saving as stat_max_fatality_rate
 state_max_fatality_rate <- state_max_fatality_rate %>% 
   arrange(desc(Maximum_Fatality_Ratio))

 #arranging the max fatality ratios for states in descending order
state_max_fatality_rate
#checking to see my new data frame

#task 5
state_max_fatality_rate$Province_State <- factor(state_max_fatality_rate$Province_State, 
levels=state_max_fatality_rate$Province_State)
#making the province_state column into a factor with levels so that it will stay in order 
#making a new plot with the new data frame
state_max_fatality_rate %>%  ggplot(aes(x=Province_State, y = Maximum_Fatality_Ratio))+ 
  geom_bar(stat = 'identity')+ theme(axis.text.x = element_text(angle=90))
#using a pipeline to make a bar chart. I had to override he default stat for geom_bar (which is count)
#to identity which is me telling the graph that I will put the y values in manually - we can thank google 
#for that one!

#exercise 6
library(tidyverse)
df$Last_Update <- as.Date(df$Last_Update)
#again making this by date, it apparently works better than a character vector and 
#I like that I can list it in sequential order which is very handy!!
cum_deaths_us <- df %>% group_by(Last_Update) %>%
  summarize(cumulative_deaths = cumsum(Deaths)) %>% arrange(Last_Update)
#make a new data fram that is cum_sums_us which is the cumulative sum of deaths in the 
#us over time. 
cum_deaths_us %>% ggplot(aes(x=Last_Update, y=cumulative_deaths))+
  geom_smooth(stat = 'identity')+labs(x="Date (Year-Month)", y= "Cumulative Deaths in USA")
#cleaned up the axis titles just to make it really nice!
#added stat as identity because the data didn't seem to be fitting right
#seemed to work!

