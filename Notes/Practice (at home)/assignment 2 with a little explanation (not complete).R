#4 
csv_files <- list.files(path = "Data", pattern = ".csv")

#5
length(csv_files)

#6
df <- read.csv("Data/wingspan_vs_mass.csv")

#7
head(x=df,n = 5)

#8. 
list.files(path = "Data", pattern = "^b", recursive = TRUE)

#9
# i have to make it so I can actually access the files here, so I need to make sure I can see their FULL file paths, hence why I added full.names to my list files command
b <- list.files(path = "Data", pattern = "^b", recursive = TRUE, full.names = TRUE)
# and after MUCH blood sweat and tears here is the for loop that displays the first lines of those files
for (first in b) {print(x = readLines(first,n=1))}
#the magic here is 'first' means for each value or variable (b-starting files) in the 'b' list (vector that i named b), I am going to print reading the lines from each value that is the first line. 

#10
csv_files <- list.files(path = "Data", pattern = ".csv", full.names = TRUE)
for (line1 in csv_files) {print(readLines(line1, n=1))}


for (first in b) {print(x = readLines(first,n=1))}



for(i in b){print (readLines(i,1))}
