#' Compute the probability of observing an outbreak of size z given an R<1 and assuming a Poisson offspring distribution
#'
#'
#' @author Pierre Nouvellet (\email{p.nouvellet@imperial.ac.uk})
#'
#' @export
#'
#' @param R is the reproduction number, i.e. the average number of secondary cases due to a single case.
#' This can be any positive number.
#'
#' @return
#'  The function returns a list including:
#' \itemize{
#'
#' \item R_eff: the effective reproduction number. Note that if R<1, then R_eff=R.
#'
#' \item alpha given the reproduction number and assuming a Poisson offspring distribution. Note that if R<1, then alpha=0.
#'
#' \item proba_ext: the probability of extinction given alpha. Note that if R<1, then proba_ext=1.
#'
#' }
#'#'
#'
#' @examples
#'
#' x <- R_eff_poisson(.5)
#' x
#' x <- R_eff_poisson(1.5)
#' x
#'
#
f_z_poiss <- function(z,s,R){
  logf<-(z-s-1)*log( s*z)+(z-s)*log(R)+ (-z*R) - lgamma(z-s +1)
  f<-exp(logf)
  return(proba_z=f)
}
