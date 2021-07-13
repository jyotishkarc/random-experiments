library(tidyverse)
library(gganimate)
library(ggplot2)
library(hrbrthemes)
library(tidyr)
library(magick)

pacman::p_load(tidyverse, gganimate, ggplot2, 
               hrbrthemes, tidyr, magick, av, gifski) 

Model_0 <- function() {
  
  # SIAR Model
  
  T <- 180
  TimeV <- seq(1, T)
  S <- rep(0, T)
  I <- rep(0, T)
  A <- rep(0, T)
  R <- rep(0, T)
  
  N <- 10000
  S[1] <- 0.995*N
  I[1] <- 0.005*N*100/140
  A[1] <- 40/100*I[1]
  
  lambda <- 5*0.1
  gamma <- 0.07
  beta <- 0.12
    
  for (t in seq (1, T-1)) {
    
    S[t+1]=S[t]-lambda*S[t]*(I[t]+A[t])/N
    I[t+1]=I[t]+lambda*S[t]*I[t]/N-gamma*I[t]
    A[t+1]=A[t]+lambda*S[t]*A[t]/N-beta*A[t]
    R[t+1]=R[t]+gamma*I[t]+beta*A[t]
  }
  
  data_mat=data.frame(TimeV, S, I, A, R)
  colnames(data_mat)=c("Time", "S", "I", "A", "R")
  data_mat=gather(data_mat, State, Number, -Time)
  
  # Creating a data frame for feeding data into the animation
  
  
  p =  ggplot(data_mat, aes(x=Time))+
       geom_line(aes(x=Time, y=Number, color=State), size=1, alpha=1)+  
       geom_point(aes(x=Time, y=Number, color=State), size=2)+
       geom_vline(aes(xintercept=Time))+# Joining the origin and the bob with a straight line
       #scale_color_viridis(discrete=TRUE)+
       ggtitle("Progression of the Disease (Model 0)")+
       ylab("Number of people")+
       xlab("Time in Days")+# Displaying time in seconds
       transition_reveal(Time)
  
  p=p+theme_light(base_size=16)
  p=p+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
      
  z <-animate(p, nframes=T, renderer=magick_renderer())						                      # Storing the animation object
  
  image_write_gif(z, 'Model_0.gif')
  z
}

Model_1 <- function() {
  
  # SAIR Model
  
  T <- 180
  TimeV <- seq(1, T)
  S <- rep(0, T)
  I <- rep(0, T)
  A <- rep(0, T)
  R <- rep(0, T)
  
  N <- 10000
  S[1] <- 0.995*N
  I[1] <- 0.005*N*100/140
  A[1] <- 40/100*I[1]
  
  lambda <- 5*0.1
  gamma <- 0.05
  beta_1 <- 0.2
  beta_2 <-0.07
  
  for (t in seq (1, T-1)) {
    
    S[t+1]=S[t]-lambda*S[t]*A[t]/N
    A[t+1]=A[t]+lambda*S[t]*A[t]/N-(beta_1+beta_2)*A[t]
    I[t+1]=I[t]+beta_1*A[t]-gamma*I[t]
    R[t+1]=R[t]+gamma*I[t]+beta_2*A[t]
  }
  
  data_mat=data.frame(TimeV, S, A, I, R)
  colnames(data_mat)=c("Time", "S", "A", "I", "R")
  data_mat=gather(data_mat, State, Number, -Time)
  
  # Creating a data frame for feeding data into the animation
  
  
  p = ggplot(data_mat, aes(x=Time))+
      geom_line(aes(x=Time, y=Number, color=State), size=0.5, alpha=1)+  
      geom_point(aes(x=Time, y=Number, color=State), size=2)+
      geom_vline(aes(xintercept=Time))+
      scale_size_manual(values = c(1, 1,1,2))+
      ylab("Number of people")+
      xlab("Time in Days")+# Displaying time in seconds
      transition_reveal(Time)
  
  p=p+theme_light(base_size=16)
  p=p+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
  z <-animate(p, nframes=T, renderer=magick_renderer())						                      # Storing the animation object
  
  image_write_gif(z, 'Model_1.gif')
  z
}

Model_2 <- function() {
  
  # SAIRD Model
  
  T <- 180
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
  
  lambda <- 5*0.1
  gamma_1 <- 0.04
  gamma_2 <-0.05
  beta_1 <- 0.2
  beta_2 <-0.07
    
    for (t in seq (1, T-1)) {
      
      S[t+1]=S[t]-lambda*S[t]*A[t]/N
      A[t+1]=A[t]+lambda*S[t]*A[t]/N-(beta_1+beta_2)*A[t]
      I[t+1]=I[t]+beta_1*A[t]-(gamma_1+gamma_2)*I[t]
      R[t+1]=R[t]+gamma_1*I[t]+beta_2*A[t]
      D[t+1]=D[t]+gamma_2*I[t]
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
  p=p+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
  z <-animate(p, nframes=T, renderer=magick_renderer())						                      # Storing the animation object
  
  image_write_gif(z, 'Model_2.gif')
  z
}

Model_3 <- function() {
  
  # SHAIRD Model
  
  T <- 180
  TimeV <- seq(1, T)
  S <- rep(0, T)
  H <- rep(0, T)
  I <- rep(0, T)
  A <- rep(0, T)
  R <- rep(0, T)
  D <- rep(0, T)
  
  N <- 10000
  S[1] <- 0.995*N
  H[1] <- 0.995*0.1*N
  I[1] <- 0.005*N*100/140
  A[1] <- 40/100*I[1]
  
  lambda_1 <- 5*0.1
  lambda_2 <- 5*0.1
  gamma_0 <- 0.07
  gamma_1 <- 0.07
  gamma_2 <-0.07
  
  
  beta_1 <- 0.2
  beta_2 <- 0.07
    
    for (t in seq (1, T-1)) {
      
      S[t+1]=S[t]-lambda_1*S[t]*A[t]/N
      H[t+1]=H[t]-lambda_2*(gamma_0*A[t]+I[t])/N
      A[t+1]=A[t]+lambda_1*S[t]*A[t]/N+lambda_2*H[t]*(gamma_0*A[t]+I[t])/N-(beta_1+beta_2)*A[t]
      I[t+1]=I[t]+beta_1*A[t]-(gamma_1+gamma_2)*I[t]
      R[t+1]=R[t]+gamma_1*I[t]+beta_2*A[t]
      D[t+1]=D[t]+gamma_2*I[t]
    }
  
  data_mat=data.frame(TimeV, S, H, A, I, R, D)
  colnames(data_mat)=c("Time", "S", "H", "A", "I", "R", "D")
  data_mat=gather(data_mat, State, Number, -Time)
  
  # Creating a data frame for feeding data into the animation
  
  
  p = ggplot(data_mat, aes(x=Time))+
    geom_line(aes(x=Time, y=Number, color=State), size=1, alpha=1)+  
    geom_point(aes(x=Time, y=Number, color=State), size=2)+
    geom_vline(aes(xintercept=Time))+# Joining the origin and the bob with a straight line
    #scale_color_viridis(discrete=TRUE)+
    ggtitle("Progression of the Disease (Model 3)")+
    ylab("Number of people")+
    xlab("Time in Days")+# Displaying time in seconds
    transition_reveal(Time)
  
  p=p+theme_light(base_size=16)
  p=p+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
  z <-animate(p, nframes=T, renderer=magick_renderer())						                      # Storing the animation object
  
  image_write_gif(z, 'Model_3.gif')
  z
}

