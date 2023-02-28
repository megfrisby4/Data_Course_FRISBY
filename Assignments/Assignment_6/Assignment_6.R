library(tidyverse)
library(ggplot2)
library(gganimate)


#loading my data in relative to my new directory!!!
dat <- read_csv("./Data6/BioLog_Plate_Data.csv")
dat

#making the data longer by putting the hours variable in a single column
dat <- dat %>% pivot_longer(cols = c(Hr_24, Hr_48, Hr_144), 
                     names_to = "Hour",
                     values_to = "Absorbance")
dat

#creating a new column that states whether sample is from soil or water 
dat <- dat %>% mutate(Type=case_when(dat$`Sample ID`== "Clear_Creek"~ "water", 
                                dat$`Sample ID`== "Waste_Water"~"water",
                                dat$`Sample ID`=="Soil_1"~"soil", 
                                dat$`Sample ID`=="Soil_2"~"soil"))
dat

#mutating so there is another new column "Time" where the hours quantities are numeric not character
dat <- dat %>% mutate(Time=case_when(Hour=="Hr_24"~24, Hour=="Hr_48"~48, Hour=="Hr_144"~144))
dat

#subsetting so I only have dilution = .1, plotting absorbance over time facet wrapped by ssubstrate
dat[dat$Dilution==.1,] %>% ggplot(aes(y=Absorbance,color=Type, x=Time))+
  geom_smooth(se = FALSE)+ 
  facet_wrap(~Substrate)+
  labs(subtitle="Just dilution 0.1")
ggsave("./Graphs6/justdilution.png", width = 6, height=5)

#subsetting to only have Itaconic acid substrate grouping by Id, time and dilution so i only get those 
#columns (those are all i need to graph)
#summarizing to make a new column mean_absorbance where it is the mean absorbance of each group
#plotting the mean_absorbance over time for each group with the ID as color and facet wrap by Dilution
#transition_reveal by Time to animate

dat[dat$Substrate=="Itaconic Acid",] %>% 
  group_by(`Sample ID`,Time, Dilution) %>% 
  summarize(mean_absorbance=mean(Absorbance, na.rm=TRUE)) %>% 
ggplot(aes(x=Time, y=mean_absorbance, color = `Sample ID`))+
  geom_line()+
  facet_wrap(~Dilution)+
  transition_reveal(Time)
#saving as a gif object in the directory so we can actually view the animation!
anim_save("./Graphs6/meanabsorbance.gif")

