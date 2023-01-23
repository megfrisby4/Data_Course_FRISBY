getwd()

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

#8.
list.files(path = "Data", recursive = TRUE, pattern = "^b")

# ^ $ (beggining = ^) (ending = $) ---- in regular expressions
#^/$= begins with/ending



#9.
b <- list.files(path = "Data", recursive = TRUE, pattern = "^b", full.names = TRUE)

for (first in b) {print(readLines(first,n=1))}

#we need to make the object b include the full paths to all the files beginning with b or I have no access
#to them to be able to read out the first lines of each file

#to make a for loop you put for as the command, then whatever 'variable' you would like to take on the
#identity of each item in the list, then you put 'in' and then whatever vector you want the 'for' command to read from
#inside the weird brackets you put 'print' because you want the command to print the first lines of the files, then
#you put readlines because you want specific lines from each file printed, and then you put your variable 
#which again is taking on the identify of each item in the vector, and then the number of lines from each file 
#you want read. 

#10. 
csv_files <- list.files(path = "Data", pattern = ".csv", full.names = TRUE)
for (line1 in csv_files) {print(readLines(line1, n=1))}

#same dealio here, I needed to allow full names to get the full file path, create a variable, for each item in my vector
#I want the command to print the first line of every file, so  I put readlines, my variable, and then the number of lines.
        