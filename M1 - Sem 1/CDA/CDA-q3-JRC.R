#### Name: JYOTISHKA RAY CHOUDHURY
#### Roll: MB2203

#### Problem 3

library(dplyr)
library(caret)
library(MASS)

datacl <- read.csv("~/R/R Codes/Experiments with R/M1 - Sem 1/CDA/Datasets/datacl.csv")

set.seed(123)

training.individuals.datacl <- datacl$X %>% createDataPartition(p = 0.8, list = FALSE)
train.datacl <- datacl[training.individuals.datacl, ]
test.datacl <- datacl[-training.individuals.datacl, ]

# ggplot(datacl, aes(x = x1, y = x2)) + geom_point(aes(colour = y))

## LDA
res.lda <- lda(y ~ x1 + x2, data = train.datacl)

predicted.train.lda <- predict(res.lda, train.datacl)
predicted.test.lda <- predict(res.lda, test.datacl)

pred.lda <- predicted.datacl.lda$x %>% as.numeric()

## QDA
res.qda <- qda(y ~ x1 + x2, data = train.datacl)

predicted.train.qda <- predict(res.qda, train.datacl)
predicted.test.qda <- predict(res.qda, test.datacl)

predictions_qda <- data.frame(predict(res.qda, test.datacl))
predictions_qda <- cbind(test.datacl, predictions_qda)

# predictions_QDA %>% count(class, y)
# predictions_QDA %>% summarize(score = mean(class == y))




#### Comparison

lda.cm <- table(test.datacl$y, predicted.test.lda$class)
qda.cm <- table(test.datacl$y, predicted.test.qda$class)

test.datacl %>%
   mutate(lda.pred = (predicted.test.lda$class),
          qda.pred = (predicted.test.qda$class)) %>%
   summarise(lda.error = mean(y != lda.pred),
             qda.error = mean(y != qda.pred))



















