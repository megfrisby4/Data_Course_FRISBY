# class notes January 17, 2023
# hashtg means everything to the right on that line 
#is for humans, wont be read as R code 
# run a line of code hit cntrl enter
# hit tab in parenthesis for more information
# ~ means home
#when in doubt hit tab in parenthesis
# list.files(path = "Data/",pattern = ".csv") - looks for files in folder only with .csv in name
#list.files(recursive = TRUE) - re curses/lists all the files, digs into the file
# list.files(path = "Data/",pattern = ".csv", full.names = TRUE) - gives path from where you started
# TRUE = yes please, a special term, FALSE is special as well. 
#TAB helps auto complete
# comma seperates arguments to a function
# highlight push cntrl-shift-c comment vs uncomment (make a hashtag)
# list.files(path = "Data",pattern = ".csv", full.names = TRUE) - brings up paths to all my files 
# alt minus makes little arrow, assigner, save path to file under codename, save as variable for a value
# .csv -- data seperated by commas (comma seperated values)
# read.csv -- read a csv file into R
# wingspan <- read.csv(path) -- saves Data as an R thingy -- R knows about a data set has memorized in as <- (alt (-)) wingspan

# cntrl-alt-b runs everthing in your scirpt before your cursor

# wingspan$ access to one column of data

# $(whatever your dataframes name is)(whatever column you want) --- gives you access to that column

#"num" numeric type of data
#"chr" character type of data
# wingspan = DATAFRAME -- rows and columns
#data frame: 2-dimensional, has rows and columns
#vector: 1-dimensional, kind of like a list, a bunch of numbers in order
# max(m) maximum mass; mean(m); min (m) summary(m)
#cumsum (m)
# plot (cumsum(m)) -- makes a plot of cumulative sum of masses
#sort (m)
# plot (sort (m)) sorting data on a plot
# plot with x and y axis -- plot (w,m)

# to get correlation of data set : cor (x=wingspan$wingspan, y=wingspan$mass)

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





#doing assignment 2 
