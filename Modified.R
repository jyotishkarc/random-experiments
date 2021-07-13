require(tidyverse)
require(gganimate)
require(ggplot2)
require(hrbrthemes)
require(tidyr)
require(magick)
require(readxl)

Model_2 <- function() {
  
  # SAIRD Model
  
  
  
  T <- 267
  TimeV <- seq(1, T)
  S <- rep(0, T)
  I <- rep(0, T)
  A <- rep(0, T)
  R <- rep(0, T)
  D <- rep(0, T)
  
  
  N <- 10000
  S[1] <- 0.995*N
  I[1] <- 0.005*N*100/140
  A[1] <- 40/100*I[1]
  
  lambda <-0.3096
  gamma_1 <- rep(0, T)
  gamma_2 <- rep(0, T)
  beta_1<-1/5.5
  beta_2 <- rep(0, T)
  beta_2[1]=0.0796
  data <- read_excel("D:/My Documents/WestBengal.Covid.xlsx")
  Real_R=(data$`R(t)`)
  Real_I=(data$`I(t)`)
  Real_D=(data$`D(t)`)
  #Real_R=rep(0, T)
  #Real_I=rep(0, T)
  #Real_D=rep(0, T)
  
  for (t in seq (1, T-1)) {

    gamma_1[t]=Real_R[t+1]-Real_R[t]
    gamma_2[t]=Real_D[t+1]-Real_D[t]
    
    
    DR=gamma_1[t]*I[t]+beta_2[t]*A[t]
    
    #Delta=matrix(S[t]*A[t]/N[t], -A[t], 0, A[t], nrow=2, ncol=2, byrow=TRUE)
    #epsilon=c(DA[t]+beta_1*A[t], Real_DR[t]-gamma_1[t]*Real_I[t])
    #omega=solve(Delta, epsilon)
    
    #lambda[t+1]=omega[1]
    beta_2[t+1]=(DR-gamma_1[t]*Real_I[t])/A[t]
    
    S[t+1]=S[t]-lambda*S[t]*A[t]/N
    A[t+1]=A[t]+lambda*S[t]*A[t]/N-(beta_1+beta_2[t])*A[t]
    I[t+1]=I[t]+beta_1*A[t]-(gamma_1[t]+gamma_2[t])*I[t]
    R[t+1]=R[t]+gamma_1[t]*I[t]+beta_2[t]*A[t]
    D[t+1]=D[t]+gamma_2[t]*I[t]
  }
  
  data_mat=data.frame(TimeV, S, A, I, R, D)
  colnames(data_mat)=c("Time", "S", "A", "I", "R", "D")
  data_mat=gather(data_mat, State, Number, -Time)
  
  # Creating a data frame for feeding data into the animation
  
  
  p = ggplot(data_mat, aes(x=Time))+
    geom_line(aes(x=Time, y=Number, color=State), size=0.5, alpha=1)+  
    geom_point(aes(x=Time, y=Number, color=State), size=2)+
    geom_vline(aes(xintercept=Time))+# Joining the origin and the bob with a straight line
    #scale_color_viridis(discrete=TRUE)+
    ggtitle("Progression of the Disease (Model 2)")+
    ylab("Number of people")+
    xlab("Time in Days")+# Displaying time in seconds
    transition_reveal(Time)
  
  p=p+theme_light(base_size=16)
  p=p+theme(panel.grid.major=element_blank(), 
            panel.grid.minor=element_blank())
  
  z <-animate(p, nframes=T, renderer=magick_renderer())						                      # Storing the animation object
  
  image_write_gif(z, 'Model_2.gif')
  z
}


