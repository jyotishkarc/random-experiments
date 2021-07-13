Time=WestBengal_Covid$t
I=WestBengal_Covid$`I(t)`
R=WestBengal_Covid$`R(t)`
D=WestBengal_Covid$`D(t)`
I1=WestBengal_Covid$`I'(t)`
R1=WestBengal_Covid$`R'(t)`
D1=WestBengal_Covid$`D'(t)`
Gamma2=D1/I
Gamma1=R1/I
gamma1=mean(Gamma1)
gamma2=mean(Gamma2)
#(1/beta1) is the incubation period for Covid-19
beta1=1/5.5
# We assume that sum of incubation time and recovery time for an infected person in hospital is same as the time of recovery for an asymptomatic people
beta2=1/((1/beta1)+1/gamma1 - 4)

#SIMULATION:
error<-function(lamda){
l=lamda
Dead=c()
Infected=c()
Asymp=c()
Recoverd=c()
S=c()
N=100000000
#We assume that 0.000005% of population is approximately infected initially 
S[1] <- 0.99999995*N
Infected[1] <- 0.00000005*N
Asymp[1] <- mean(I[1:14])
Dead[1]=0
for (t in 1:266) {
  S[t+1] = S[t]-l*S[t]*Asymp[t]/N
  Asymp[t+1]=Asymp[t] + l*S[t]*Asymp[t]/N - (beta1+beta2)*Asymp[t]
  Infected[t+1]=Infected[t] + beta1*Asymp[t]-(gamma1+gamma2)*Infected[t]
  Recoverd[t+1]=Recoverd[t] + gamma1*Infected[t] + beta2*Asymp[t]
  Dead[t+1]= gamma2*Infected[t]
}

e=sum((I-Infected)^2)
e
}
z=optimise(error,lower = 0,upper = 5,maximum = FALSE)
z
L=z$minimum
#Plotting:
Plot<-function(lamda){
  l=lamda
  Dead=c()
  Infected=c()
  Asymp=c()
  Recoverd=c()
  S=c()
  N=100000000
  S[1] <- 0.99999995*N
  Infected[1] <- 0.00000005*N
  Asymp[1] <- mean(I[1:14])
  Dead[1]=0
  for (t in 1:600) {
    S[t+1] = S[t]-l*S[t]*Asymp[t]/N
    Asymp[t+1]=Asymp[t] + l*S[t]*Asymp[t]/N - (beta1+beta2)*Asymp[t]
    Infected[t+1]=Infected[t] + beta1*Asymp[t]-(gamma1+gamma2)*Infected[t]
    Recoverd[t+1]=Recoverd[t] + gamma1*Infected[t] + beta2*Asymp[t]
    Dead[t+1]= gamma2*Infected[t]
  }
  
  plot(Infected,type = 'l')
  plot(Asymp,type = 'l')
}
Plot(L)

