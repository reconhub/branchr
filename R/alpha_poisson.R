#' Obtain alpha for a Poisson offspring distribution
#'
#' If the transmission can be charaterised by a Poisson offspring distribution, the function
#' alpha.poisson(R) compute alpha for a given R.
#' alpha is linked to probability of extinction and allow us, for any Rl>1, to find an Rs<1 guaranteeing that
#' conditional on extinction, outbreak properties starting from Rs or Rl are exactly the same.
#'
#'
#' @author Pierre Nouvellet (\email{p.nouvellet@imperial.ac.uk})
#'
#' @export
#'
#' @param R is the reproduction number, i.e. the average number of secondary cases due to a single case.
#' This can be any positive number. if R is a vector, then the length of R must be 'n' (see below).
#'
#' @return
#'  The function returns alpha assuming a Poisson offspring distribution.
#'
#'
#' @examples
#'
#' x <- alpha_poisson(c(1.5,.5))
#' x
#'

alpha_poisson <- function(R){
  f <- which(R<=1)
  alpha <- R+lamW::lambertW0(-R*exp(-R))
  alpha[f] <- 0
  return(alpha=alpha)
}
