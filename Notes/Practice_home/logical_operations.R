data("mtcars")
head(mtcars)
#exercise 1
mtcars[mtcars$mpg >15& mtcars$mpg<20,]

#exercise 2
mtcars[mtcars$cyl==6 &mtcars$am!=0,]

#exercise 3
mtcars[xor(mtcars$gear==4,mtcars$carb==4),]

#exercise 4
mtcars[seq(2,32,2),]

#exercise 5
mtcars$mpg[seq(4,32,4)] = 0
mtcars$mpg

#exercise 6 
mtcars[1 %in% mtcars$vs& mtcars$vs!=0&1 %in% mtcars$am &mtcars$am!=0,]

#exercise 7
(TRUE+TRUE)*FALSE

#exercise 8
mtcars[1%in% mtcars$vs&&1%in% mtcars$am,]

#exercise10
mtcars
mtcars$am <- replace(mtcars$am,mtcars$am==0,2)
mtcars

#exercise 11
mtcars$vs <- replace(mtcars$vs,values = mtcars$vs+2)
mtcars$vs
mtcars

#exercise 12
mtcars[xor(mtcars$vs>mtcars$am,mtcars$vs<mtcars$am),]
mtcars[xor(mtcars$gear>mtcars$carb,mtcars$gear<mtcars$carb),]
