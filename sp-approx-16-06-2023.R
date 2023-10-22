
library(magrittr)
library(ggplot2)
library(reshape2)

X <- seq(0,0.5,by = 1/1000) %>% setdiff(0)
length(X)

normal.approx <- 1-pnorm(sqrt(50)*(X - 0.1)/0.3)
true.prob <- 1 - pbinom(50*X, 50, 0.1)

sp.approx.func <- function(z,m=50,p=0.1){
   q <- 1-p
   phi.hat <- log((q*z)/(p*(1-z)))
   
   r <- sign(phi.hat) * sqrt(2*m*(phi.hat*z - log(q/(1-z))))
   v <- phi.hat * sqrt(m*z*(1-z))
   
   return(1 - (pnorm(r) + dnorm(r)*(1/r - 1/v)))
}

sp.approx <- X %>% sapply(sp.approx.func)

df <- data.frame(X = X, true.prob, normal.approx, sp.approx)
# df.log <- data.frame(X = X, log(true.prob), log(normal.approx), log(sp.approx))

df %>%
   reshape2::melt(id = 'X') %>%
   ggplot(aes(x = X)) +
   geom_line(aes(y = value, color = variable), linewidth = 0.6) +
   theme_minimal() +
   scale_y_log10() +
   annotation_logticks()

df %>%
   melt(id = 'X') %>%
   ggplot(aes(x = X)) +
   geom_line(aes(y = value, color = variable), linewidth = 0.6) +
   labs(x = "X", y = "Upper Tail Probability", color = " ") +
   scale_color_manual(labels = c("True Probability    ",
                                 "Normal Approximation    ",
                                 "SP Approximation    "), 
                      values = c("red","limegreen","blue")) +
   scale_y_log10() +
   annotation_logticks() +
   theme_light() +
   theme(text = element_text(family="", size=13),
         axis.title.y = element_text(size=15, face="bold", 
                                     margin=margin(r=15)),
         axis.title.x = element_text(size=15, face="bold", 
                                     margin=margin(t=15)),
         axis.text = element_text(size=11, face="bold", color="black"),
         legend.title = element_text(face="bold"),
         legend.title.align = 0.5,
         legend.key.size = unit(1,"line"),
         legend.position = 'bottom',
         legend.background = element_rect(fill = "white", 
                                          linewidth = 1, 
                                          linetype = "solid"),
         legend.box.background = element_rect(colour = "black", 
                                              linewidth = 1))


normal.approx.func <- function(z, m=50, p=0.1){
   return(1-pnorm(m*(z - p)/sqrt(m*p*(1-p))))
}

normal.approx <- X %>% sapply(normal.approx.func, 50, 0.1)



# plot(X,10^normal.approx, type = 'l', col = 'red',ylab = "")
# lines(X,10^true.prob, type = 'l')
# lines(X,10^sp.approx, type = 'l', col = 'blue')

