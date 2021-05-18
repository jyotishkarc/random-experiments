require(ggplot2)
n <- 10
datanorm <- c(1,4,2,6,7,3,-1,4,6,8)
d <- data.frame(x = 1:n, y = datanorm)

myplot <- ggplot(d,aes(x,y)) + geom_point() + 
  geom_line(data=data.frame(spline(d, n=n*10)) , size=2) +
  geom_point(colour = "red",size=4)

final <- myplot + theme_light()
        #  theme(plot.background=element_rect(fill = 'green',colour = 'red'))
