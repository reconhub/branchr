#' # Likelihood of observing outbreaks of size y given parameters
#'
#'
#'  The function internally relies on pre-computing:
#'  1) g: the probability of observing size y given z,rho; with z the true number of
#'  cases (reported and un-reported cases) and rho the reporting probability
#'
#' 2) g0: the probability of not observing an outbreak (i.e. y=0 or no cases) given z and rho.
#'
#' g and g0 are obtained using the function proba_observation.
#'
#'
#' @author Pierre Nouvellet (\email{p.nouvellet@imperial.ac.uk})
#'
#' @export
#'
#' @param R is the reproduction number, i.e. the average number of secondary cases due to a single case.
#' This can be any positive number.
#'
#' @param z is the true potential outbreak sizes. It is precomputed by proba_observation.
#'
#' @param g: the probability of observing size y given z,rho; with z the true number of
#'  cases (reported and un-reported cases) and rho the reporting probability. It is precomputed by proba_observation.
#'
#' @param g0: the probability of not observing an outbreak (i.e. y=0 or no cases) given z and rho.
#' It is precomputed by proba_observation.
#'
#'
#' @return
#'  The function returns the likelihood of the observations for a given {R,rho}.
#'
#'
#' @examples
#'
#' x <- element_Lhood_poisson(.5,x$possible_size,x$p_y_z,x$p_0_z)
#' x
#'
import<-function(y_obs,rho,profile,threshold_z,threshold_import,CI){

  z <- matrix(seq(1,threshold_z),threshold_z,1)
  g0 <- dbinom(0,matrix(z,1,threshold_z,byrow=TRUE),rho)

  profile$import <- 0:threshold_import
  profile$Lk_import <- rep(0,threshold_import+1)



  for (i in 1:length(profile$theta)){
    R_eff <- R_eff_poisson(profile$theta[i])
    f <- f_z_poiss(z,1,R_eff$R_effective)*R_eff$P_extinction
    p_obs<- (1-g0 %*% f)


    temp <- dnbinom(profile$import, length(y_obs), p_obs, log = TRUE) +
      profile$Likelihood[i]
    profile$Lk_import <- profile$Lk_import + exp(temp)
  }
  profile$Lk_import <- log(profile$Lk_import)

  max_likelihood <- theta_max_likelihood(profile$import,profile$Lk_import,CI)

  return ( list (theta_max_likelihood = max_likelihood$theta,
                 max_likelihood = max_likelihood$likelihood,
                 lower_theta =  max_likelihood$lower_theta,
                 upper_theta = max_likelihood$upper_theta) )
}
