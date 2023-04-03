mtcars
mtcars[c(1,5,7,9,12),]
head(mtcars)
mpg <- mtcars$mpg
disp <- mtcars$disp
mtcars$gear
mtcars[c((mpg>18&mtcars$gear==3&mtcars$drat==3.08)),]


bs <- list.files(path="Data", pattern = "^b", recursive=TRUE, full.names = TRUE)
bs
b1 <- 
b1 <- bs[1][1]
readLines(b1[1])
readLines(b1, 1)
for (i in bs) {print(readLines(i,n=1))}
