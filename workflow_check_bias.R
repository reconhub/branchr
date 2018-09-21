rm(list=ls(all.names=TRUE))

# source('simulate_poisson.R')

R=.75
nrep=20
s=1
rho=1
t_max=1e3

Nrep <- 100
Rs <- matrix(NA,nrep,Nrep)
R_in_95CI <- 0
Sizes <- rep(NA,Nrep)

imp <- Rs
imp_in_95CI <- 0

for (n in 1:nrep){
  print(n)
  for (k in 1:Nrep){

    # print(k)

    Sim_I <- simulate_poisson(R,n,s,rho,t_max)
    Sizes[k] <- Sim_I$true_size
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
    R_in_95CI <- R_in_95CI +
      ((R_estimate$upper_theta>=R_estimate$theta_max_likelihood) &
         (R_estimate$lower_theta<=R_estimate$theta_max_likelihood))

    # import<-import(y_obs = y_obs,
    #                rho = rho,
    #                profile = profile,
    #                threshold_z = 1e3,
    #                threshold_import = 1e3,
    #                CI = 0.95)
    #
    # # c(n,length(y_obs)+import$theta_max_likelihood,
    # #   length(y_obs)+import$lower_theta,
    # #   length(y_obs)+import$upper_theta)
    #
    # imp[k] <- import$theta_max_likelihood
    # imp_in_95CI <- imp_in_95CI +
    #   ((import$upper_theta>=import$theta_max_likelihood) &
    #      (import$lower_theta<=import$theta_max_likelihood))


  }
  hist(R-Rs[n,])
  c(sum(R>Rs[n,])/Nrep, R_in_95CI/Nrep)
  c(median(Rs[n,]), mean(Rs[n,]))

  x <- seq(0,max(Sizes))
  Px <- s*x^(x-s-1)*R^(x-s)*exp(-x*R)/factorial(x-s)
  hist(Sizes,x+.5,freq = FALSE)
  lines(x,Px)


  #
  #
  # hist(imp-rho)
  # c(sum(imp>rho)/Nrep, imp_in_95CI/Nrep)
  #
  # sum(profile$theta*exp(profile$Likelihood)/sum(exp(profile$Likelihood)))
}
meanRs <- apply(Rs,1,mean)
plot(1:n,meanRs,ylim = c(0,1))
lines(c(0,n),c(R,R),col=rgb(1,0,0))

medianRs <- apply(Rs,1,median)
plot(1:n,medianRs,ylim = c(0,1))
lines(c(0,n),c(R,R),col=rgb(1,0,0))


# check fat-tail
hist(Sizes,x+.5,freq = FALSE)
lines(x,Px)
lines(1+x,dnbinom(x = x,size = 1, mu = R),col=rgb(1,0,0))
