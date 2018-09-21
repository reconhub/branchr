rm(list=ls(all.names=TRUE))

simulate_negbin <- function(R,n,s,rho,t_max,overdisp) {
  # check input
  if (length(R)!=1 && length(R)!=n) warning('R must be a vector, and its size must be either 1 or n')
  if (length(s)!=1 && length(s)!=n) warning('s must be a vector, and its size must be either 1 or n')
  if (length(rho)!=1 && length(rho)!=n) warning('rho must be a vector, and its size must be either 1 or n')
  if ((length(n)!=1 | n==0 | n!=round(n))[1]) warning('n must be a positive integer')
  if ((length(t_max)!=1 | t_max==0 | t_max!=round(t_max))[1]) warning('t_max must be a positive integer')
  
  # declare incidence matrices
  N <- matrix(NA,n,t_max)
  N_obs_full <- matrix(NA,n,t_max)
  
  # initial values
  N[,1] <- s
  N_obs_full[,1] <- rbinom(n,N[,1],rho)
  
  # simulate the Poisson branching process and reporting
  for (i in 2:t_max){
    # N[,i] <- rpois(n,R*N[,i-1])
    N[,i] <- rnbinom(n,mu = R*N[,i-1],size = overdisp)
    N_obs_full[,i] <- rbinom(n,N[,i],rho)
  }
  # check all simulated outbreaks are extinct
  n_ongoing <- n - sum(N[,t_max]==0)
  if (n_ongoing>0) warning(
    paste0('There are ',n_ongoing,' outbreaks that are not extinct,
           consider increasing the number of time-steps (especially if R<1).'))
  
  # calculate final sizes, true and observed
  z <- rowSums(N)
  z_obs_full <- rowSums(N_obs_full)
  
  # correct for unobserved outbreaks
  f <- which(z_obs_full==0)
  if (length(f)>0){
    N_obs <- N_obs_full[-f,]
    z_obs <- z_obs_full[-f]
  }else{
    N_obs <- N_obs_full
    z_obs <- z_obs_full
  }
  
  # output
  return( list(true_incidence = N, reported_incidence = N_obs_full, observed_incidence = N_obs,
               true_size = z, reported_size = z_obs_full, observed_size = z_obs) )
}


# source('simulate_poisson.R')

R=.92
s=1
rho=.2
t_max=5e3

Nrep <- 1e2
# Sizes <- rep(NA,Nrep)

# imp <- Rs
# imp_in_95CI <- rep(0,nrep)

n_imp <- c(30,50,70)
nrep <- length(n_imp)

Rs <- matrix(NA,nrep,Nrep)
R_in_95CI <- rep(0,nrep)

imp <- Rs
imp_in_95CI <- R_in_95CI

for (n in 1:nrep){
  print(n)
  for (k in 1:Nrep){

    # print(k)

    # Sim_I <- simulate_negbin(R,n_imp[n],s,rho,t_max,overdisp = .5)
    Sim_I <- simulate_poisson(R,n_imp[n],s,rho,t_max)
    Sizes <- Sim_I$true_size
    
    # hist(Sim_I$true_size)
    # mean(Sim_I$true_size)

    y_obs <- Sim_I$observed_size
    # source('alpha_poisson.R')
    # source('element_Lhood_poisson.R')
    # source('f_z_poiss.R')
    # source('proba_ext.R')
    # source('proba_observation.R')
    # source('profile_likelihood.R')
    # source('R_eff_poisson.R')
    # source('theta_max_likelihood.R')
    # source('import.R')


    profile <- profile_likelihood(y_obs = y_obs,
                                  rho = rho,
                                  accuracy = 0.01,
                                  max_R = 20)
    # plot(profile$theta,profile$Likelihood)

    R_estimate <- theta_max_likelihood(theta = profile$theta,
                                       likelihood = profile$Likelihood,
                                       threshold_CI = 0.95)

    # c(R,R_estimate$theta_max_likelihood,R_estimate$lower_theta,R_estimate$upper_theta)

    Rs[n,k] <- R_estimate$theta_max_likelihood
    R_in_95CI[n] <- R_in_95CI[n] +
      ((R_estimate$upper_theta>=R) &
         (R_estimate$lower_theta<=R))

    import <- import(y_obs = y_obs,
                   rho = rho,
                   profile = profile,
                   threshold_z = 1e3,
                   threshold_import = 1e3,
                   CI = 0.95)

    # c(n,length(y_obs)+import$theta_max_likelihood,
    #   length(y_obs)+import$lower_theta,
    #   length(y_obs)+import$upper_theta)

    imp[n,k] <- length(y_obs)+import$theta_max_likelihood
    imp_in_95CI[n] <- imp_in_95CI[n] +
      (( (length(y_obs)+import$upper_theta) >=n_imp[n]) &
         ( (length(y_obs)+import$lower_theta)<=n_imp[n]))


  }
  # hist(R-Rs[n,])
  # c(sum(R>Rs[n,])/Nrep, R_in_95CI/Nrep)
  # c(median(Rs[n,]), mean(Rs[n,]))
  # 
  # x <- seq(0,max(Sizes))
  # Px <- s*x^(x-s-1)*R^(x-s)*exp(-x*R)/factorial(x-s)
  # hist(Sizes,x+.5,freq = FALSE)
  # lines(x,Px)


  #
  #
  # hist(imp-rho)
  # c(sum(imp>rho)/Nrep, imp_in_95CI/Nrep)
  #
  # sum(profile$theta*exp(profile$Likelihood)/sum(exp(profile$Likelihood)))
}
# meanRs <- apply(Rs,1,mean)
# plot(1:n,meanRs,ylim = c(0,1))
# lines(c(0,n),c(R,R),col=rgb(1,0,0))
layout(matrix(1:2,1,2))
medianRs <- apply(Rs,1,quantile,c(.5,.025,.975))
library(Hmisc)
errbar(n_imp,medianRs[1,], medianRs[2,],medianRs[3,],ylim = c(0,1),
       ylab = 'distribution of R estimates',xlab = 'nb imported cases')
lines(c(0,n_imp[n]),c(R,R),col=rgb(1,0,0))

plot(n_imp,R_in_95CI/Nrep,pch=16,
     ylim = c(0,1),ylab = 'Proportion of 95%CI including true R',
     xlab = 'nb imported cases')

apply((Rs-R)/R,1,median)


# importations
layout(matrix(1:2,1,2))
medianimp <- apply(imp,1,quantile,c(.5,.025,.975))
library(Hmisc)
errbar(n_imp,medianimp[1,], medianimp[2,],medianimp[3,],ylim = c(0,100),
       ylab = 'distribution of importation estimates',xlab = 'nb imported cases')
lines(n_imp,n_imp,col=rgb(1,0,0))

plot(n_imp,imp_in_95CI/Nrep,pch=16,
     ylim = c(0,1),ylab = 'Proportion of 95%CI including true importation',
     xlab = 'nb imported cases')

apply((imp-matrix(n_imp,3,Nrep))/matrix(n_imp,3,Nrep),1,median)

save.image('Pois_check.Rdata')
# # check fat-tail
# hist(Sizes,x+.5,freq = FALSE)
# lines(x,Px)
# lines(1+x,dnbinom(x = x,size = 1, mu = R),col=rgb(1,0,0))
