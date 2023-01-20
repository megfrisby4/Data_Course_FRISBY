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
b
readLines(b[1], n=1)
readLines(b[2], n=1)
readLines(b[3], n=1)

# for loops --- HW
#redo this task using for-loop
for(i in b){print (readLines(i,1))} # how do i get to here?
for (variable in vector) {
  
}

