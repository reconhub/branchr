rm(list=ls(all.names=TRUE))

source('simulate_poisson.R')

R=.5
n=1e2
s=1
rho=.2
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
source('theta_max_likelihood.R')
source('import.R')


profile <- profile_likelihood(y_obs, rho, 1e3, 0.01, 20)

R_estimate <- theta_max_likelihood(profile$theta,profile$Likelihood,0.95)
c(R,R_estimate$theta_max_likelihood,R_estimate$lower_theta,R_estimate$upper_theta)

import<-import(y_obs,profile,1e3,1e3,0.95)
c(n,length(y_obs)+import$theta_max_likelihood,
  length(y_obs)+import$lower_theta,
  length(y_obs)+import$upper_theta)
