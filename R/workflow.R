rm(list=ls(all.names=TRUE))

source('simulate_poisson.R')

R=.9
n=1e2
s=1
rho=.6
t_max=1e3

Sim_I <- simulate_poisson(R,n,s,rho,t_max)

hist(Sim_I$true_size)

y_obs <- Sim_I$observed_size
source('alpha_poisson.R')
source('element_Lhood_poisson.R')
source('f_z_poiss.R')
source('proba_ext.R')
source('proba_observation.R')
source('profile_likelihood.R')
source('R_eff_poisson.R')
source('R_max_likelihood.R')


profile <- profile_likelihood(y_obs, rho, 1e3, 0.01, 20)

R_estimate <- R_max_likelihood(profile$R,profile$Likelihood,0.95)

c(R,R_estimate$R_max_likelihood,R_estimate$lower_R,R_estimate$upper_R)
