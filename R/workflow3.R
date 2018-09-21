# rm(list=ls(all.names=TRUE))
#
# # source('simulate_poisson.R')
#
# N <- 1e2
# res <- matrix(NA,N,2)
# rho <- c(.2,.5,.9)
# R <- c(.3,.5,.7,.9)
#
# d_res <- expand.grid(R,rho)
# names(d_res ) <- c('R','rho')
# d_res <- data.frame(d_res,
#                     R_accuracy=rep(NA,nrow(d_res)),
#                     import_accuracy=rep(NA,nrow(d_res)))
#
# res3 <- array(NA,c(2,nrow(d_res),N,3))
#
# for (j in 1:nrow(d_res)){
#   print(j)
#
#   n <- 1e2
#   s <- 1
#
#   t_max <- 1e3
#   R <- d_res$R[j]
#   rho <- d_res$rho[j]
#
#   for (i in 1:N){
#     # print(i)
#     Sim_I <- simulate_poisson(R,n,s,rho,t_max)
#
#     # hist(Sim_I$true_size)
#
#     y_obs <- Sim_I$observed_size
#     # source('alpha_poisson.R')
#     # source('element_Lhood_poisson.R')
#     # source('f_z_poiss.R')
#     # source('proba_ext.R')
#     # source('proba_observation.R')
#     # source('profile_likelihood.R')
#     # source('R_eff_poisson.R')
#     # source('theta_max_likelihood.R')
#     # source('import.R')
#
#
#     profile <- profile_likelihood(y_obs = y_obs, rho = rho, accuracy = 0.01, max_R = 20)
#
#     R_estimate <- theta_max_likelihood(profile$theta,profile$Likelihood,0.95)
#     c(R,R_estimate$theta_max_likelihood,R_estimate$lower_theta,R_estimate$upper_theta)
#
#     import<-import(y_obs = y_obs,rho = rho, profile = profile, threshold_z = 1e3,threshold_import = 1e3,CI = 0.95)
#     c(n,length(y_obs)+import$theta_max_likelihood,
#       length(y_obs)+import$lower_theta,
#       length(y_obs)+import$upper_theta)
#
#     res[i,1] <-  (R_estimate$lower_theta <= R && R_estimate$upper_theta >= R)
#     res[i,2] <-  ((length(y_obs)+import$lower_theta) <= n && ( length(y_obs)+import$upper_theta) >= n)
#
#     res3[1,j,i,1:3] <- c(R_estimate$theta_max_likelihood, R_estimate$lower_theta,R_estimate$upper_theta)
#     res3[2,j,i,1:3] <- c(import$theta_max_likelihood, import$lower_theta,import$upper_theta)
#
#
#   }
#   plot(cumsum(res[,1])/1:N)
#   plot(cumsum(res[,2])/1:N)
#   d_res[j,3:4] <- colSums(res)/N
# }
#
#
#
# ## cbind(100:200,100:200+qnbinom(0.999, 100:200,.2,lower.tail=TRUE)+1)
