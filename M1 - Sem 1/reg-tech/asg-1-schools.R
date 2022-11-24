

resplots <- resplots.sc <- list()

for(i in 1:19){
   
   res <- lm(Total.Population.of.Village ~ Number.of.Govt..primary.Schools + Number.of.Govt...Secondary.Schools, data = school.df.clean %>% filter(District.Code == i+326))
   
   resplots[[i]] <- qplot(y = res$residuals, x = res$fitted.values)
}


for(i in 1:19){
   
   res.sc <- lm(Total.SC.ST.population.of.village ~ Number.of.Govt..primary.Schools + Number.of.Govt...Secondary.Schools, data = school.df.clean %>% filter(District.Code == i+326))
   
   resplots.sc[[i]] <- qplot(y = res.sc$residuals,x = res.sc$fitted.values)
}


library(cowplot)

pp <- ggplot(cbind("Fitted" = res$fitted.values, 
                   "Residuals" = res$residuals) %>% as.data.frame(),
             aes(x = Fitted, y = Residuals)) +
         geom_point(size = 0.6) +
         xlim(c(-10,30000)) +
         ylim(c(-20000,30000)) +
         ggtitle("Residual plot corr. to Total Population") +
         ggeasy::easy_center_title()

pp

pp.sc <- ggplot(cbind("Fitted" = res.sc$fitted.values, 
                      "Residuals" = res.sc$residuals) %>% as.data.frame(),
             aes(x = Fitted, y = Residuals)) +
   geom_point(size = 0.6) +
   xlim(c(-10,10000)) +
   ylim(c(-10000,20000)) +
   ggtitle("Residual plot corr. to SC-ST Population") +
   ggeasy::easy_center_title()

pp.sc

ggpubr::ggarrange(pp, pp.sc)
