
library(ggplot2)

dist.codes <- school$District.Code %>% unique()
dist.names <- school$District.Name %>% unique()

logreg <- binreg <- list()
lambda <- c()

school.2 <- school <- school %>% na.omit()
school.2$count.sec[school.2$count.sec > 0] <- 1

for(k in 1:length(dist.names)){
   
   logreg[[k]] <- glm(count.sec ~ Total.Pop + scst.Pop, 
                      data = school.2 %>% filter(District.Code == dist.codes[k]), 
                      family = "poisson")
   
   x <- fitted(logreg[[k]])
   
   y <- residuals(logreg[[k]], type = "pearson")
   A <- qplot(x,y) + 
      geom_point(size = 0.01, shape = ".") +
      ggtitle(paste0("Pearson Residual Plot ",dist.names[k])) + 
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Fitted Values") + ylab("Pearson Residuals")
   
   y <- residuals(logreg[[k]], type = "deviance")
   B <- qplot(x,y) + 
      geom_point(size = 0.01, shape = ".") +
      ggtitle(paste0("Deviance Residual Plot ",dist.names[k])) + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlab("Fitted Values") + ylab("Deviance Residuals")
   
   ggpubr::ggarrange(A,B)
   
   print(k)
}


rex <- glm(count.prim ~ Total.Pop + scst.Pop, 
           data = school, 
           family = "poisson")

x <- fitted(rex)

y <- residuals(rex, type = "pearson")
A <- qplot(x,y, ylim = c(-20,30), xlim = c(0,100)) + 
         geom_point(size = 0.01, shape = ".") +
         ggtitle("Pearson Residual Plot") + 
         theme(plot.title = element_text(hjust = 0.5)) +
         xlab("Fitted Values") + ylab("Pearson Residuals")

y <- residuals(rex, type = "deviance")
B <- qplot(x,y, ylim = c(-20,30), xlim = c(0,125)) + 
         geom_point(size = 0.01, shape = ".") +
         ggtitle("Deviance Residual Plot") + 
         theme(plot.title = element_text(hjust = 0.5)) + 
         xlab("Fitted Values") + ylab("Deviance Residuals")

ggpubr::ggarrange(A,B)



binwhole <- binnedplot(x,y,type = "pearson")
binned_residuals(rex)










