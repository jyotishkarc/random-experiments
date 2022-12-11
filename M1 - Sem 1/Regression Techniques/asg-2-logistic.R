
dist.codes <- school$District.Code %>% unique()
dist.names <- school$District.Name %>% unique()

logreg <- binreg <- list()
lambda <- c()

school.2 <- school <- school %>% na.omit()
school.2$count.sec[school.2$count.sec > 0] <- 1

# for(k in 1:length(dist.names)){
for(k in 7:8){
   
   logreg[[k]] <- glm(count.sec ~ Total.Pop + scst.Pop, 
                      data = school.2 %>% filter(District.Code == dist.codes[k]), 
                      family = binomial(link = "logit"))
   
   # binned_residuals(logreg[[k]]) %>% plot(main = "Hello")  
   
   x <- fitted(logreg[[k]])
   y <- residuals(logreg[[k]])
   binwhole <- binnedplot(x,y, xlab = "Fitted Values", ylab = "Pearson Residuals",
                          main = paste0("Binned Residual Plot for ", dist.names[k]))
   
   # binreg[[k]] <- binned_residuals(logreg[[k]])
   
   print(k)
}


rex <- glm(count.sec ~ Total.Pop + scst.Pop, 
           data = school.2, 
           family = binomial(link = "probit"))


x <- fitted(rex)
y <- residuals(rex)

binwhole <- binnedplot(x,y,xlab = "Fitted Values", ylab = "Residuals")

# binned_residuals(rex)
