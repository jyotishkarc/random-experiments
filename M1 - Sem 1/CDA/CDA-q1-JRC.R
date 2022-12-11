#### Name: JYOTISHKA RAY CHOUDHURY
#### Roll: MB2203

#### Problem 1

library(dplyr)
library(caret)

data1 <- read.csv("~/R/R Codes/Experiments with R/M1 - Sem 1/CDA/Datasets/data.csv")
data1$diagnosis <- case_when(data1$diagnosis == "B" ~ 0, data1$diagnosis == "M" ~ 1)
data1$id <- 1:length(data1$id)

set.seed(123)

training.individuals.data1 <- data1$id %>% createDataPartition(p = 0.7, list = FALSE)
train.data1 <- data1[training.individuals.data1, ]
test.data1 <- data1[-training.individuals.data1, ]

res <- glm(diagnosis ~ radius_mean + smoothness_mean, data = train.data1,
           family = binomial(link = "logit"))

summary(res)

predicted.data1 <- predict(res, test.data1, type = "response") %>% as.numeric()

predicted.class.data1 <- function(delta){
   
   actual <- test.data1$diagnosis %>% as.factor()
   
   Z <- ifelse(predicted.data1 > delta, 1, 0) %>% as.factor()
   conf.mat <- confusionMatrix(Z,actual)
   # sens <- sensitivity(Z,actual)
   # spec <- specificity(Z,actual)
   
   return(conf.mat)
}

predicted.class.data1(0.25)
predicted.class.data1(0.75)


