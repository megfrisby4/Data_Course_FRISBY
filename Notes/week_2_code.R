# Week 2 - File paths etc. 
getwd()
list.files()
list.files(path = "Notes")
list.files(path = "Notes")
list.files(path = "Data",pattern = "wingspan_vs", full.names = TRUE)
list.files(recursive = TRUE)
# give me full path to wingspan file 
path <- list.files(recursive = TRUE, 
           pattern = "Wingspan_vs",
           ignore.case = TRUE,
           full.names = TRUE)

path 
wingspan <- read.csv(path)
wingspan
m <- wingspan$mass
v <- wingspan$velocity
s <- wingspan$variety
max(m)
min (m)
mean (m)
summary (m)
cumsum (m)
plot (cumsum(m))
plot (mean(m))
plot (m)
sort (m)
plot (sort (m))
plot (w,m)
plot (x=wingspan$wingspan, y=wingspan$mass)
cor (x=wingspan$wingspan, y=wingspan$mass)
plot (cor (x=wingspan$wingspan, y=wingspan$mass))

csvs <- list.files(recursive = TRUE, pattern = ".csv")
getwd()

airlines <- csvs [1]
readLines(airlines, n=3)
airlines <- read.csv(airlines)
csvs

#New syntax 
#1st item in vector [square brackets, 1-whatever number it goes to]
#csvs [1]
#save it as a variable , airlines <- csvs [1], read n lines in a file = readLines(airlines, n=3)


#list.files() --- list the files in the directory you are working in
# getwd() = --- get working directory --- the absolute file path to the place you currently are 
# read.csv(airlines) -- read to data frame, make sure to save it as data frame =  airlines <- read.csv(airlines) --- just overrode file path variable
# recursively listing files ---- list files in all subdirectories ever 


#square bracket notation lets you pick elements from an object (upper right in environment
airlines <- csvs[1] #character vector... 1st element
readLines(csvs[1])# readlines ets you read in 1 line at a time
#it makes a character vector where each line is an element

airlines <- read.csv(airlines) # makes a data frame of the comma separated values
#pays attention to columns to split lines into columns

#numeric vectors
x <- 1:10  #x = numeric vector of length 10
length(x)
#length counts lenght of vector
#everything in R is vectorized
#all its mathmetical behavior is vectorized
y <- 2:11
x/y
x+y # matches up vectors and gives you the answers of the vectors together (as shown in top right corner)
letters
LETTERS
paste0(letters, LETTERS)# past together letter (character) vectors 

#expressions (logical)
x>3 #can use R to ask yes/no questions about vectors 
(x>3) # tells you what type of vector you have 
TRUE
FALSE
TRUE+FALSE
TRUE+TRUE
#TRUE = 1 FALSE = 0
(x>3) # will get true or false
sum(x>3) #total number of characters in vector greater than 3 
x>=3 # x is greater than or equal to three
x<= 5 # less than or equal to
x==5 # is equal to? is x equal to?


wingspan <- list.files(recursive = TRUE, pattern = "mass.csv", full.names = TRUE)
wingspan
wingspan <- read.csv(wingspan)
names(wingspan) #show names of columns
names(x) #no names in this vector

sum(wingspan$mass >=70)# how many in mass column have mass greater than or equal to 70
wingspan$giant <- wingspan$mass >=70 # create brand new column that gives true false vector or whether the mass is >=70
plot(x=wingspan$wingspan, y=wingspan$mass, col=wingspan$giant)
names(wingspan)

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
#^/# = begins with/ending

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



