
library(tsne)
library(plotly)
data("iris")

features <- subset(iris, select = -c(Species)) 

# set.seed(1)
tsne <- tsne(features, initial_dims = 2)
tsne <- data.frame(tsne)
pdb <- cbind(tsne,iris$Species)
options(warn = -1)
fig <-  plot_ly(data = pdb ,x =  ~X1, y = ~X2, type = 'scatter', mode = 'markers', split = ~iris$Species)

fig <- fig %>%
   layout(
      plot_bgcolor = "#e5ecf6"
   )

fig
