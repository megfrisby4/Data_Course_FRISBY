library(ggplot2)
ggplot(data=faithful, 
       mapping=aes(x=eruptions,
       y=waiting))+
  geom_point()+ 
  geom_density2d()
ggplot(data=faithful)+ 
  geom_point(aes(x=eruptions, y=waiting), 
             shape="square", alpha=0.5)
 ggplot(data=faithful)+
  geom_histogram(aes(x=eruptions, 
                     fill=waiting<60))


data("mtcars")
data("mpg")
ggplot(mpg)+ geom_jitter(aes(x=class,
                             y=hwy), width=0.2)+
  stat_summary(aes(x=class, y=hwy), fun = mean, geom="point", color="red")
ggplot(mpg)+
  geom_point(aes(x=displ,
                 y=hwy, color=class))+
  scale_color_brewer(type='qual')

ggplot(mpg)+
  geom_point(aes(x=displ, y=hwy, color=class, size=cyl))+
  scale_color_brewer(type='qual')+
  scale_size_area(breaks=c(4,5,6,8))

ggplot(mpg)+
  geom_point(aes(x=displ, y=hwy, color=cty, size=cty))+
  guides(color='legend')
#when you have 2 aesthetics (color and size), mappedd to the same variable
# (size) you can combine them into one key using guides

