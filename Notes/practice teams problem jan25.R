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
na.exclude(width,)
area <- rectangles$area <- length*width
length(rectangles$area)
area[62]

rectangles

#or instead of excluding the data just write it in
rectangles$width[63] = 231
width
width[63]
rectangles$area <- length*width
area <- rectangles$area


rectangles
