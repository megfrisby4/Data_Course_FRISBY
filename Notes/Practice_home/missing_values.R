#exercise 1
X=c(NA, 3,14, NA, 33, 17, NA, 41)
is.na(X)
!is.na(X)
X[!is.na(X)]

#exercise 2
Y=21:28
Y
Z=data.frame(X,Y)
Z
Z[is.na(Z)]<-0
Z
P=c(X, 33, NA, 400, 12, 0, 15)
P
P[is.na(P)]<-10
P

#exercise 3
W <- c(11,3,5,NA,6)
W
is.na(W)

#exercise 4
A <- c(33,21,12,NA,7,8)
class(A)
mean(A,na.rm=TRUE)

#exercise 5
data(Orange)
head(Orange)
rows <- row.names(Orange[Orange$age==118,])
rows <- as.numeric(rows)
Orange$age[rows] = NA
Orange$age
Orange

#exercise 6
c1 <- c(1,2,3,NA)
c2 <- c(2,4,6,89)
c3 <- c(45,NA,66,101)
 X <- data.frame(c1,c2,c3)
X 
complete.cases(X)
X[!complete.cases(X),]


#exercise 7
df <- data.frame(Name=c("NA","Joseph", "Martin",NA, "Andrea"),
                 Sales=c(15,18,21,56,60),
                 Price = c(34,52,21,44,20), 
                 stringsAsFactors = FALSE)
df
is.na(df$Name)
df$Name
df$Name[1]=NA
df[]
df <- df[complete.cases(df),]
df
