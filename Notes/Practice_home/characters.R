library(tidyverse)
#exercise 1
vector = "Good morning! "
length(vector)
nchar(vector)

#exercise 2
x <- c("Open", "Sesame ")
y <- c("You", "Suck.")
nchar(x)
nchar(y)
nchar(c(x,y))

#exercise 3
m <-"The capital of the United States is Washington D.C."
names <- unlist(str_split(m, " "))
str_trunc(m,11, ellipsis = "")
str_sub(m, start=13, end=25)
nchar(names)
sum(nchar(names)[1:7])
str_sub(m, start = 29, end=39)
nchar("washington")
nchar(m)
nchar(" D.C.")
51-5
46-10
str_sub(m, start= 36, end = 46)

#exercise 4
paste(m, ", you idiot!", sep = "")
paste(str_sub(m, start = 20, end = 32), ", you idiot!", sep = "")

#exercise 5
q = "What is the capital of the United States?"
c(q, paste0(m,", you idiot!"))
d = str_split(c(q, paste0(m, ", you idiot!")),pattern = " ")
d

#exercise 6 
c(unlist(map(d,1)), "Heck!?")
 map(d,1)
class((unlist(map(d,2))))

#exercise 7
t=c("a", "ab", "c", "d", "e", "fa")
grep("a", t)
grepl("a", t)
f=c("b", "ca", "at","c", "e","aa")
v=list(f,t)
v
grepl("a", v)
d
grep("What", d)
grepl("What", d)

#exercise 8
q
str_replace(q, "a", "A")
str_replace_all(q, " ", "_")
