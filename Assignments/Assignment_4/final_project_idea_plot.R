install.packages("tidyverse")
library(tidyverse)
df <- read.csv("./Assignments/Assignment_4/final_project_idea_data .csv")
class(df)
 plot <-  df %>% ggplot(aes(x=country)) + geom_bar(aes(fill= Year )) +
    theme(axis.text.x = element_text(angle = 70, hjust=1),
          axis.title.x = element_text(vjust = 10)) + labs(y="Number of Outbreaks") +
    scale_fill_manual(values =c("red", "orange", "yellow", "green", "blue", "navy", "violet","purple", "pink", "magenta", "#320354",
                                     "#f0056b", "#f03c05"))
  
ggsave(plot, filename="./final_project_idea_plot.png", height=4, width = 7 )
