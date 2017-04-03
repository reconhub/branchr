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


element_Lhood_poisson<-function(R,z,g,g0){

  R_eff <- R_eff_poisson(R)
  f <- f_z_poiss(z,1,R_eff$R_effective)*R_eff$P_extinction
  # get the likelihood
  Likelihood <- (sum(log((g %*% f)))-nrow(g)*log(1-g0 %*% f))
  # correction for threshold?
  # L2 <- -log( sapply(yobs,
  #                    function(x) correct.tail(x,R,k,p,threshold))  )
  return( Likelihood )
}
