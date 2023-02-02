#exercise 1
x=c("ss", "aa", "ff", "kk", "bb")
x[1]
x[c(1,3)]

#exercise 2
d = data.frame (Name = c("Betty", "Bob", "Susan"),
Age = seq(20,30, length.out = 3), Height_cm = c (490,22,0))
d
d[c("Name", "Age")]
d[c("Age", "Name", "Height_cm")][1,]

#exercise 3
Factor_Name <- factor(d$Name)
Factor_Name
Factor_Age <- factor(d$Age)
d$Age[2]
d$Age >20
d[d$Age>20,]
d[d$Height_cm<100,
  ]
d[1,c("Name", "Age")]

