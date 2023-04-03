
rectangles <- read.csv("Data/rectangles.csv")
rectangles
class(rectangles)
rectangles$length
length(rectangles$length)
class(rectangles$length)
as.numeric(rectangles$length
)
length <- as.numeric(rectangles$length
)
width <- as.numeric(rectangles$width)
width
rectangles$area <- length*width
rectangles
write.csv(rectangles, "./Data/rectangles_clean.csv")
list.files(path= "./Data/", pattern="rectangles")

