#' Obtain an effective R when conditionning on extinction and assuming poisson
#'
#' For any Rl>1, we can find an Rs<1 guaranteeing that
#' conditional on extinction, outbreak properties starting from Rs or Rl are exactly the same.
#' We call this Rs an effective R.
#' The funtion compute the effetive R as well as alpha (see alpha_poisson) and the probability of extinction (see proba_ext)
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
R_eff_poisson <- function(R){

  if (R>1){
    alpha <- alpha_poisson(R)
    p_ext <- proba_ext(alpha)
    R_eff <- R*p_ext
  }else{
    alpha <- 0
    p_ext <- 1
    R_eff <- R
  }
  return( list(R_effective = R_eff,
               P_extinction = p_ext,
               alpha = alpha) )
}
