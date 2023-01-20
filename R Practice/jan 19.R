income <- list.files(path = "Data", pattern = "rent.csv", all.files = TRUE, full.names = TRUE, recursive = TRUE)
income <- read.csv(income)
income$variable
income$Alabama
plot(x = income$Alabama, y = income$Alaska)
income$"income"
income$
getwd
#list.files() list the files in the directory you are working in
# getwd() = get working directory
