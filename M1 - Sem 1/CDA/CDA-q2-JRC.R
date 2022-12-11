#### Name: JYOTISHKA RAY CHOUDHURY
#### Roll: MB2203

#### Problem 2

library(dplyr)
library(caret)

datacl <- read.csv("~/R/R Codes/Experiments with R/M1 - Sem 1/CDA/Datasets/datacl.csv")

set.seed(123)

training.individuals.datacl <- datacl$X %>% createDataPartition(p = 2/3, list = FALSE)
train.datacl <- datacl[training.individuals.datacl, ]
test.datacl <- datacl[-training.individuals.datacl, ]

ggplot(datacl, aes(x = x1, y = x2)) + geom_point(aes(colour = y))

res <- glm(y ~ x1 + x2, data = train.datacl,
           family = binomial(link = "logit"))

summary(res)

predicted.datacl <- predict(res, test.datacl, type = "response") %>% as.numeric()

actual <- test.datacl$y %>% as.factor()

Z <- ifelse(predicted.datacl > 0.5, 1, 0) %>% as.factor()
conf.mat <- confusionMatrix(Z,actual)
# sens <- sensitivity(Z,actual)
# spec <- specificity(Z,actual)



########## LOOCV

total.loss.1 <- total.loss.2 <- total.loss.12 <- rep(0,nrow(datacl))

for(i in 1:nrow(datacl)){
   
   print(i)
   
   res1 <- glm(y ~ x1, data = datacl[-i,], family = binomial(link = "logit"))
   res2 <- glm(y ~ x2, data = datacl[-i,], family = binomial(link = "logit"))
   res12 <- glm(y ~ x1 + x2, data = datacl[-i,], family = binomial(link = "logit"))
   
   
   pred1 <- predict(res1, datacl[i,], type = "response") %>% as.numeric()
   pred2 <- predict(res2, datacl[i,], type = "response") %>% as.numeric()
   pred12 <- predict(res12, datacl[i,], type = "response") %>% as.numeric()
   
   total.loss.1[i] <- (datacl[i,2] - pred1)^2
   total.loss.2[i] <- (datacl[i,2] - pred2)^2
   total.loss.12[i] <- (datacl[i,2] - pred12)^2
}

print(sum(total.loss.1), sum(total.loss.2), sum(total.loss.12))



















