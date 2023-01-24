#notes january 24, 2023

#character class  -- things in quotes
letters
class(letters)  #character vector, what type of class is it?
length(letters) #the length of the vector --- number of elements in a vector
rep(letters,2) # repeat the vector n (n=2) times 
rep(letters,each=2)
length(rep(letters,2)) #=52 what is the length of letters repeated twice
class(length(rep(letters,2))) # integer vector, in parenthesis is 52 so its giving class of 52
length(class(length(rep(letters,2)))) # output is integer, only 1 character 
rep() #repeat
# rep(vector, each=n) --- repeat each charactere n times 
rep(letters, each=2) #repeat each letter twice 
rep(letters, each=2)[20] # brackets choose which specific elememt
rep(letters, each=2)[20:25] # : = through, 20 through 25
# [] let you access specific element of any object
# you cant add vectors of different classes

x <- "1" # x = character 1
x+1 #not gonna work
as.numeric(x) #as.numeric changes characters to numeric
as.numeric(x) +1 $#so this will work now
  
as.character(123) #changes number into a character
123+1 #wont work because 123 is now a character

y <- c(1:10,"One") #creating a vector
#vectors can only be one class
#if there is one character in a vector, the whole vector will be characters!
as.numeric(y) #as. function changes vecor classes, it wont know what 
# to do with"one
#NA = missing data
NA
TRUE
FALSE

#numeric class
class(10L) #integer
10 
length(10)
class(10) #numeric
rep(10, 30) #repeat 10 30 times
rep(10:13,30)
rep(10:13, each = 30)
seq() # makes sequences with numbers
seq(0,10, length.out=30) # equidistant
factor() #make letters into a factor function
x <- factor(letters)
class(x)
length(x)
levels(x) #what levels are in the factor
z <- rep(x, each=50) 
length(z)
levels(z)
z # if levels: comes up at the end in your output you are dealing with a factor -- thats annoying!
z[1300]
c(z[1300], "A")
#factors are for discreet variables -- like eye color have specific levels

as.character(z) #makes factor into character vector
as.numeric(z) # makes factor into numeric vector
as.numeric("z") #NA
# ! = not
1 !=5 #one is not equal to five, that is true

mtcars
mtcars$mpg
length(mtcars$mpg)
dim(mtcars) # dimestions of data set, [rows, col]
mtcars[1,1] # first row, first column = 21
#have to put 2 numbers because its a data frame and not a vector
mtcars[1,] # row one all columns
mtcars[1:3,1:3] # rows 1-3 columns 1-3
#[rows, columns]
mtcars[c(1,3),c(1,3)] # rows one and 3 columns one and 3 
c() # combine, have to combine to get just one and 3. 
head(mtcars) #first 6 rows first six colums
mtcars$mpg>21
mtcars[mtcars$mpg>21,] #show what rows has mpg >21 
#rows = where the column mpg has an output greater than 21, all the columns
mtcars$name <- row.names(mtcars) # adds new column where it is row names
dim(mtcars)
mtcars[33,]


1:10 +5
for (yourmom in 1:10) {print(yourmom+5)
  
}
for (yourdad in letters) {print(paste("your dad = ", yourdad))
  
}


# myfiles <- list.files(path="Data", pattern=".csv", full.names=TRUE)
# for (i in myfiles) {print(readLines(i,n=1))
myfiles <- list.files(path="Data", pattern=".csv", full.names = TRUE)
for (i in myfiles) {print(readLines(i,n=3))
  
}
for (i in myfiles) {print(read.csv(i))
  
}# reads all csvs in as data frames
