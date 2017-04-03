#' Profile likelihood for R given observations, rho, threshold, accuracy maximum value for R
#'
#'
#' @author Pierre Nouvellet (\email{p.nouvellet@imperial.ac.uk})
#'
#' @export
#'
#' @param y_obs: a vector of outbreak sizes observations.
#' @param rho: the probability of a single case being detected.
#' @param accuracy: R will take potential values in a grid spaning from accuracy up to max_R with increments 'accuracy'
#' @param max_R: R will take potential values in a grid spaning from accuracy up to max_R with increments 'accuracy'
#'
#'
#' @return
#'  The function returns a list including:
#' \itemize{
#'
#' \item theta: a vector of potential parameter values for which the likelihood has been evaluated.
#'
#' \item Likelihood: a vector of likelihood evaluated for the potential R values.
#'
#' }
#'#'
#' @examples
#'
#' x <- profile_likelihood(c(1,2,3),.5,1e3,0.01,10)
#' x
#'
#'
profile_likelihood<-function(y_obs,rho,accuracy,max_R){

  #define threshold for calculation, 99.9% that inclde all true size
  threshold <- max(y_obs)+qnbinom(0.999, max(y_obs),rho,lower.tail=TRUE)+1
  # check
  # pbinom(max(y_obs),threshold+max(y_obs),rho) # rounding error??
  
  interim_res <- proba_observation(y_obs,rho,threshold)
  R_grid <- seq(accuracy,max_R,by=accuracy)
  size_R_grid <- length(R_grid)
  Likelihood <- rep(NA,size_R_grid)
  for (k in 1:size_R_grid){
    Likelihood[k] <- element_Lhood_poisson(R_grid[k],interim_res$possible_size,
                                   interim_res$p_y_z,interim_res$p_0_z)
  }
  return( list( theta = R_grid, Likelihood = Likelihood) )

}
