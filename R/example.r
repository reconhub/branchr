#'
#' #' Simulate a partially observed Poisson branching process with discrete generations
#' #'
#' #' For a given reproduction number, this function simulate a Poisson branching and return observed incidence
#' #' (per generation) accounting for under-reporting.
#' #' . This is \code{some code}.
#' #' Bullet list:
#' #'
#' #' \itemize{
#' #'
#' #' \item toto: il est beau
#' #'
#' #' \item tata: il est caca
#' #'
#' #' }
#' #'
#' #'
#' #' @author Pierre Nouvellet (\email{p.nouvellet@@imperial.ac.uk})
#' #'
#' #' @export
#' #'
#' #' @param x this is what x is
#' #'
#' #' @param y this is what y is
#' #'
#' #' @return
#' #'  The function returns ....
#' #'
#' #' @examples
#' #'
#' #' x <- simulate_poisson(1, 2)
#' #' x
#' #'
#' simulate_poisson <- function(R.eff,n,rho,t.max) {
#'
#'   R <- R.eff$R.effective
#'
#'   N <- matrix(1,n,t.max)
#'   for (i in 2:t.max){
#'     N[,i] <- rpois(n,R*N[,i-1])
#'   }
#'   n.ongoing <- n - sum(N[,t.max]==0)
#'   if (n.ongoing>0) warning(
#'     paste0('There are ',n.ongoing,' outbreaks that are not extinct,
#'            consider increasing the number of time-steps (especially if R<1).'))
#'
#'   z.sim <- rowSums(N)
#'   y.obs.full <- rbinom(n,z.sim,rho)
#'   f <- which(y.obs.full==0)
#'   if (length(f)>0){
#'     y.obs <- y.obs.full[-f]
#'   }else{
#'     y.obs <- y.obs.full
#'   }
#'   return( list(outbreak.size = z.sim, observed.size.full = y.obs.full,
#'                observed.size = y.obs) )
#' }
