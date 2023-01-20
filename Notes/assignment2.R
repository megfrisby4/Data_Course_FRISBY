#Doing assignment 2

#find all csvs in the data directory

#4. 
csv_files <- list.files(path="Data", pattern =".csv")

#5.
length(csv_files)

#6.
df <- read.csv("Data/wingspan_vs_mass.csv")

#7.
head(x=df, n=5) #inspects the first 5 lines of the data set
head(df,5)

#8.
list.files(path = "Data", recursive = TRUE, pattern = "^b")
# ^ $ (beggining = ^) (ending = $) ---- in regular expressions
#^/# = begins with/ending

#9.
b <- list.files(path = "Data", recursive = TRUE, pattern = "^b", full.names = TRUE)
for (first in b) {print(readLines(first,n=1))
  
}
  
#10. 
csv_files <- list.files(path = "Data", pattern = ".csv", full.names = TRUE)
for (line1 in csv_files) {print(readLines(line1, n=1))}

