# rm(list=ls(all.names=TRUE))
#
# # source('simulate_poisson.R')
#
# R=.95
# n=50
# s=1
# rho=1
# t_max=1e3
#
# Nrep <- 200
# Rs <- rep(NA,Nrep)
#
# for (k in 1:Nrep){
#
#   print(k)
#
#   Sim_I <- simulate_poisson(R,n,s,rho,t_max)
#
#   # hist(Sim_I$true_size)
#   # mean(Sim_I$true_size)
#
#   y_obs <- Sim_I$observed_size
#   # source('alpha_poisson.R')
#   # source('element_Lhood_poisson.R')
#   # source('f_z_poiss.R')
#   # source('proba_ext.R')
#   # source('proba_observation.R')
#   # source('profile_likelihood.R')
#   # source('R_eff_poisson.R')
#   # source('theta_max_likelihood.R')
#   # source('import.R')
#
#
#   profile <- profile_likelihood(y_obs = y_obs,
#                                 rho = rho,
#                                 accuracy = 0.01,
#                                 max_R = 20)
#   # plot(profile$theta,profile$Likelihood)
#
#   R_estimate <- theta_max_likelihood(theta = profile$theta,
#                                      likelihood = profile$Likelihood,
#                                      threshold_CI = 0.95)
#
#   c(R,R_estimate$theta_max_likelihood,R_estimate$lower_theta,R_estimate$upper_theta)
#
#   Rs[k] <- R_estimate$theta_max_likelihood
#
#   # import<-import(y_obs = y_obs,
#   #                rho = rho,
#   #                profile = profile,
#   #                threshold_z = 1e3,
#   #                threshold_import = 1e3,
#   #                CI = 0.95)
#   #
#   # c(n,length(y_obs)+import$theta_max_likelihood,
#   #   length(y_obs)+import$lower_theta,
#   #   length(y_obs)+import$upper_theta)
#
#
# }
# hist(R-Rs)
# sum(R>Rs)/Nrep
