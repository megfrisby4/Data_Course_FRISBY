list.files(path = "Data", recursive = TRUE, pattern = ".csv", all.files = TRUE, full.names = TRUE
           , ignore.case = TRUE)
messy <- list.files(recursive = TRUE, ignore.case = TRUE, all.files = TRUE, pattern = "a_df.csv")
open.csv("messy")
show("messy")
show (messy)
testing <- read.csv(messy)
$testing
$"DaysAlive_Male"
daysmale <- testing$DaysAlive_Male
IQman <- testing$IQ_Male
plot(x = "IQman", y = "daysmale" )
plot(x = daysmale, y = IQman)
