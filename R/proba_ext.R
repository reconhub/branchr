#' Compute the probability of extinction for a given alpha
#'
#' If the transmission can be charaterised by a Poisson offspring distribution, see the function
#' alpha.poisson(R) to compute alpha for a given R.
#' The probability of extinction is computed assuming a single initial case. To obtain the probability
#' extinction associated with n initial case, simply raise the probability returned by this function to the power of n.
#'
#'
#' @author Pierre Nouvellet (\email{p.nouvellet@imperial.ac.uk})
#'
#' @export
#'
#' @param alpha: a value related to the reproduction number and the offspring distribution.
#'
#' @return
#'  The function returns the probability of extinction for a given alpha. Formally, Pext=exp(-alpha)
#'
#'
#' @examples
#'
#' x <- proba.ext(c(.5,1))
#' x
#'
#
#
proba_ext <- function(alpha){
  proba_ext <- exp(-alpha)
  return(proba_ext)
}
