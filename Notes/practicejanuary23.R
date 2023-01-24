wingspan <- list.files(path='Data', pattern= "wingspan", ignore.case = TRUE, full.names= TRUE)
wingspan <- read.csv(wingspan)
print(wingspan)
colnames(wingspan)
X <- wingspan$X
observation <- wingspan$observation
span <- wingspan$wingspan
mass <- wingspan$mass
vel <- wingspan$velocity
var <- wingspan$variety
graph1 <- plot(span, mass, col=12)
cor(span,mass)
mass[1000]/span[1000]
summary(wingspan)
mean(vel)
wingspan$fast <- (vel>30)
mean(vel)
names(wingspan)

      