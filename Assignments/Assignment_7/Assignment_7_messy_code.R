# Assignment 7 messy code
# Change this to "tidy" format using dplyr verbs

# There's an intuitive dplyr version for almost everything you see here.

# Note: Do not erase the original code, just comment it out and put your own equivalent code below each section
# i.e., change each line of indicated code to a tidy version that does the same thing.


library(tidyverse)
library(janitor)
##########################
#        Part 1          #
##########################

# load data (wide format)
#utah = read.csv("./Utah_Religions_by_County.csv")
utah=read_csv("./Utah_Religions_by_County.csv") %>% clean_names()
names(utah)
# subset to only counties with buddhists observed
#buddhist = utah[utah$Buddhism.Mahayana > 0,]
buddhist=utah %>%  filter(buddhism_mahayana>0)
# order rows by population (descending)
#buddhist = buddhist[order(buddhist$Pop_2010, decreasing = TRUE),]
buddhist= buddhist %>% arrange(-pop_2010)

# write this new dataframe to a file
#write.csv(buddhist, file = "./buddhist_counties.csv", row.names = FALSE, quote = FALSE)
buddhist %>% write_csv(file = "./buddhist_counties.csv")
## get group summaries of religiousity based on population ##

# divide each county into one of six groups based on populations
# note: keep these two lines the same in your updated code!
groups = kmeans(utah$pop_2010,6) # clusters data into 6 groups based on proximity to mean of potential groups
utah$pop_group = groups$cluster # assigns a new variable to utah giving group for each county

# subset to each group and find summary stats on Religiosity for each
#group1 = mean(utah[utah$pop_group == 1,]$religious)
#group2 = mean(utah[utah$pop.group == 2,]$religious)
#group3 = mean(utah[utah$pop.group == 3,]$religious)
#group4 = mean(utah[utah$pop.group == 4,]$religious)
#group5 = mean(utah[utah$pop.group == 5,]$religious)
#group6 = mean(utah[utah$pop.group == 6,]$religious)
means <- utah %>% group_by(pop_group) %>% summarize(mean=mean(religious))
group1=means$mean[1]
group2=means$mean[2]
group3=means$mean[3]
group4=means$mean[4]
group5=means$mean[5]

# same, but mean population
#group1.pop = mean(utah[utah$Pop.Group == 1,]$Pop_2010)
#group2.pop = mean(utah[utah$Pop.Group == 2,]$Pop_2010)
#group3.pop = mean(utah[utah$Pop.Group == 3,]$Pop_2010)
#group4.pop = mean(utah[utah$Pop.Group == 4,]$Pop_2010)
#group5.pop = mean(utah[utah$Pop.Group == 5,]$Pop_2010)
#group6.pop = mean(utah[utah$pop_group == 6,]$pop_2010)


means2 <-  utah %>% group_by(pop_group) %>% summarize(mean=mean(pop_2010))
group1_pop=means2$mean[1]
group2_pop=means2$mean[2]
group3_pop=means2$mean[3]
group4_pop=means2$mean[4]
group5_pop=means2$mean[5]
group6_pop=means2$mean[6]
# make data frame of each group and mean religiosity
#religiosity = data.frame(pop_group = c("group1","group2","group3","group4","group5","group6"),
           #mean_religiosity = c(group1,group2,group3,group4,group5,group6),
           #mean_pop = c(group1_pop,group2_pop,group3_pop,group4_pop,group5_pop,group6_pop))

religiosity <- utah %>% mutate(pop_group= case_when(pop_group ==1 ~ 'group 1'
                                     ,pop_group == 2 ~ 'group 2'
                                     ,pop_group ==3 ~ 'group 3'
                                     ,pop_group == 4 ~ 'group 4'
                                     ,pop_group ==5 ~ 'group 5'
                                    ,pop_group == 6 ~ 'group 6')) %>% 
                           group_by(pop_group) %>% 
                           summarize(mean_religiosity=mean(religious), mean_pop = mean(pop_2010))
                           
religiosity # take quick look at resulting table

# order by decreasing population
#religiosity = religiosity[order(religiosity$mean_pop, decreasing = TRUE),]
religiosity <- religiosity %>% arrange(-mean_pop)


religiosity # take quick look at resulting table


# plot that table (redo this using ggplot)
#plot(x=religiosity$Mean.Pop,y=religiosity$Mean.Religiosity)

religiosity %>% ggplot(aes(x=mean_pop, y=mean_religiosity))+ geom_point()


#####################################
#              Part 2               #
# Beginning to look at correlations #
# run this code without changing it #
# it's already in very tidy form    #
#####################################

# Look for correlations between certain religious groups and non-religious people
religions = names(utah)[-c(1:4)]
utah %>%
  pivot_longer(names_to = "religion", values_to = "proportion",religions) %>%
  ggplot(aes(x=proportion,y=religious)) + geom_point() + geom_smooth(method="lm") + lims(y=c(0,1)) +
  facet_wrap(~religion,scales = "free") + theme_bw() +
  theme(panel.grid = element_blank(), strip.background = element_rect(fill="gray"))


# Look through those plots and answer the following questions:
# 1.  Which religious group correlates most strongly in a given area with the proportion of non-religious people?
# 2.  What is the direction of that correlation?
# 3.  What can you say about the relationships shown here?
# 4.  Examine the axis scales. How could you modify the code above to more accurately portray values on an "equal footing?"

# UPLOAD YOUR ANSWERS TO CANVAS
# DON'T FORGET TO PUSH YOUR TIDY CODE TO GITHUB AS WELL!