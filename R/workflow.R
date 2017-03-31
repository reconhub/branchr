rm(list=ls(all.names=TRUE))

source('simulate_poisson.R')

R=.9
n=1e2
s=1
rho=.6
t_max=1e3

Sim_I <- simulate_poisson(R,n,s,rho,t_max)

hist(Sim_I$true_size)

