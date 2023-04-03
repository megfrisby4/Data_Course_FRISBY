
#exercise 1
1:7
for (i in 1:7) {print(i^2,)
  
}

#exercise 2

for(i in colnames(iris)) {print(paste0(i,"(",nchar(i),")"))
  
}

#exercise 3
i <- 1
while (i<6) {print(i)
  i=i+1
  
}
i <- rnorm(100)
while (i<1) {print(i)
  i=i
  
}
seq(1,10, length.out =3)
seq(100,55, by = -5)
seq(1,100, by=20)

semester_start <- as.Date("2019-08-19")
semester_end <- as.Date("2019-12-5")
seq(semester_start, semester_end, by = "week")
seq(semester_start, semester_end, length.out=3)[2]
