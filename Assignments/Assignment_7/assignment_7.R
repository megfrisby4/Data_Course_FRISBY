#loading my packages
library(tidyverse)
library(janitor)
library(ggplot2)
library("scales")
library(skimr)
library(ggpubr)

#Import the untidy data set and clean names
df <- read_csv("./Utah_Religions_by_County.csv") %>% clean_names()
#examine the data set
df
str(df)
summary(df)
#have a little looksie at the names
df %>% names()


#it looks like the religions are spread out over multiple columns so I should use a pivot longer here
#first to make things easier on myself I am going to make a 
#vector that lists all the columns I will select in my pivot longer
#the religions are the names of all the columns besides the first 4
religions=names(df)[-c(1:4)]


#now I am going to pivot longer
#i will use the religions vector to express what I want for the columns (instead of having to type them all out)
#i will create a new column called religion (a variable to a single column)
# i will also create another new column for the proportion of each religion for each observation
clean <- df %>% pivot_longer(cols=religions, names_to = "religion", values_to = "proportion")
#just viewing my new clean(ish) data frame
clean

#im not loving the underscores in the religion column I will remove them with mutate
clean <- clean %>% mutate(religion=str_replace_all(religion, pattern = "_", " "))
clean

#exploring the data set

#to first explore I wanna see which religions are most prevalent in each county
#to do this I will graph religion by proportion facet wrapped by county 

clean %>% mutate(Religion=case_when(religion=="lds"~'LDS', 
                                  religion!="lds"~"Non-LDS")) %>% 
  ggplot(aes(x=religion, y=proportion))+
  geom_point(aes(color=Religion))+
  facet_wrap(~county)+ 
  theme(axis.text.x = element_blank(), legend.text = element_text())
#dont forget to save (created a new folder to save all my figures for this assignment to!)
ggsave(filename = "./Assignment_7_generated_figures/lds_vs_non-lds_by_county.png")
#it looked like one religion in each county was much more prevalent than the other religions overall
#i looked at it for a bit and based on experience pretty sure the overwhelming religion is lds
#i wanted to show lds vs non lds in color
# but I didnt want the key to say LDS true or false so I went back in and did a little mutating
#i added a new column (just to graph not to the overall cleaned data frame) that designates lds or non lds
#i mapped that column to the color 
# tadaaaaa confirmed that an overwhelming proportion of each county is lds


#ok now I want to know which county has the highest population ... pretty easy but useful information that doesn't 
#involve actually trying to read through and using my cheap ramen-noodle lump brain
clean %>% arrange(-pop_2010) %>% mutate(county=as_factor(county)) %>% group_by(county) %>% 
  ggplot(aes(x=county, y=pop_2010,fill="coral"))+geom_col()+scale_y_continuous(labels = comma)+ 
  theme(axis.text.x = element_text(angle=90), legend.position = 'none')
ggsave("./Assignment_7_generated_figures/county_populations_dec.png")
#kinda lame and not colorful but it gets the job done
#just from this simple var graph I can clearly see that salt lake county has the most people
#this is folowed by utah county, then davis couty, then weber county, them washington, then cache, and all others a pretty 
#small in terms of population, I went back in and made the populations a factor and arranged it by descending to we
#have a clearer picture
?geom_ribbon
clean
#im looking at the data and wondering what the proportion of religious vs non religious people is for each county
clean %>% 
  pivot_longer(cols=c(religious, non_religious),
               names_to="type",
                        values_to="prop") %>% 
  group_by(county) %>% 
   ggplot(aes(x=type, y=prop, group = county)) +
   geom_bar(stat = "identity", aes(fill = type), position = "dodge") + 
   scale_y_continuous(limits = c(0,1)) + 
  facet_wrap(~county, scales="free_y")+
  theme(strip.text=element_text(size=5),
        axis.text.x = element_blank(),
        axis.text.y = element_text(angle=45, size=4), 
        axis.title.y = element_text(size=7), 
        axis.title.x = element_text(size=7), 
        legend.key.height = unit(0.2,'cm'),
        legend.key.width = unit(0.2,'cm'), legend.title = element_text(size=6), legend.text = element_text(size=6))
   
ggsave("./Assignment_7_generated_figures/religious_vs_non_by_county.png", width=7, height=5)

#it took a while to make this plot because I wanted to make it nice so you could actually see the labels, etc.
#if you look at this plot you can see there are far more religious people in most counties besides a few, which are
#dun dun dunnnnn San Juan county (pretty much has 50/50 split) and also grand county and summit county are pretty close
#the most drastic differences between religious and non religious communities are in Utah, box elder, morgan
#sevier, rich, cache, tooele, which you would expect. 
#I could see for sure which ones had the greatest difference if I graphed a function like religious-non-religous 
#but for time sake and the sake of my tiny brain I am not going to do that right now :) I could also order them by 
#descending religious population and that would work too but again, my sanity is actually suffering RN



#how can I plot and see if population of a county correlates with the proportion of any religious group?
df %>% ggplot(x=pop_2010, y=proportion)

clean %>% 
  ggplot(aes(x=pop_2010,y=proportion)) + geom_point() + geom_smooth(method="lm") + lims(y=c(0,1)) +
  facet_wrap(~county,scales = "free") + theme_bw() +
  theme(panel.grid = element_blank(), strip.background = element_rect(fill="gray"))
skim(clean)
  
cor.test(pop_2010, proportion, method=c("pearson", "kendall", "spearman"))
cor.test(clean$pop_2010, clean$proportion)
#Address the questions:
  
 # “Does population of a county correlate with the proportion of any specific religious group in that county?”

#heres the deal... Ive spent many many many many many hours and even over a few days thinking about this and I 
#think this is the best answer I got so here we go heres the bean dip travis...

#correlation is usually represented by a linear graph (the closer the slope to 1, the stronger the correlation)
#i need a line for each religion
#population should be on the x axis because it is being compared with proportion
#ill use different colored lines to represent religion
#method should be linerar model because I want straight lines, a geom_smooth will do

clean%>% ggplot()+geom_smooth(aes(x=pop_2010, y=proportion, color=religion), method="lm", se=FALSE)
ggsave("./Assignment_7_generated_figures/population_vs_religion_proportion_correlation.png", width= 6, height=4.75)
#this figure here (although way underated and I way overthought it at first) shows the population(x)
# and the proportion (y) so population by proportion
#when we look at the lines we can see as the population of a county goes upp most of the proportions of religions dont
#however there is an exception, the catholic line has a slight correlation with population (positive slope) compared
#to the flat line of all the others
#from this we can see that population of a county is correlated with the proportion of catholics in a county
#a bigger county means a greater proportion of hte county is catholic, a lesser population means a lesser proportion
#this is kind of weird becausethe other religions stay at the same proportion no matter the population size


#“Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?”

#i need to graph proportion of religion on the x axis correlated with proportion of non religions on y
#same thought process but I might need to facet wrap to best see the correlation for each religion effectively
#again the line needs to be linear a geom smooth will do
#the graph needs to show the relationship between proportion of a religion in a county and proportion of non_religious
#the main question is what happens while proportion of a specific religion increases (again different colored lines are 
#different relgions)
#the view without facet wrap is pretty sucky and hard to differentiate so I will facet wrap so I can separate
#by religion
#it gives me 13 cute little graphs that I can clearly see the slopes of 
#the only graph (line/slope) that is near a slope of 1 or -1 (correlation is a slope close to 1 or -1)
#is lds which appears to have a slope of darn near -1, this means that an incrase in the proportion of lds generally
#means a pretty strong decrease in non_religious people
#in other words the religion lds is correlated with a decrease in non_religious people in a given county


clean  %>% ggplot()+geom_smooth(aes(x=proportion, y=non_religious, color=religion),
                                                      method="lm", se=FALSE)+facet_wrap(~religion)+
  theme(axis.text.x = element_text(angle=90))
ggsave("./Assignment_7_generated_figures/specific_religion_vs_non_religious_correlation.png", width = 6, height=4.75)
#from this graph we can clearly see the proportion of lds in a given county correlates with the portion of non_religious
#people although negatively


#Just stick to figures and maybe correlation indices…no need for statistical tests yet

#Add comment lines that show your thought processes _____________