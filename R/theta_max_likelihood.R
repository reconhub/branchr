#' Obatin the maximium likleihood from a vector of parameter and its likelihood profile
#'
#' Also gives confidence interval
#'
#'
#' @author Pierre Nouvellet (\email{p.nouvellet@imperial.ac.uk})
#'
#' @export
#'
#' @param theta is a vector containing parameter's values for which the likelihood has been evaluated
#'
#' @param likelihood at the paraneter's value (log-likelihood)
#'
#' @param threshold_CI is the threshold to estimate the confidence interval, i.e. to obtain
#' a 95% confidence interval threshold_CI=95.
#'
#'
#' @return
#'  The function returns a list including:
#' \itemize{
#'
#' \item theta_max_likelihood: the maximum likelihood estimated with the rabge of theta values evaluated.
#'
#' \item max_likelihood: the log of the maximum likelihood.
#'
#' \item lower_theta,upper_theta: lower and upper bound of the 'threshold_CI'% confidence interval.
#'
#'
#' }
#'
#'
#'
#'
# obatin the maximium likleihood for R and confidence interval
theta_max_likelihood <- function(theta,likelihood,threshold_CI){

  max_likelihood <- list( likelihood = max(likelihood) )
  max_likelihood$index <- which(likelihood %in% max_likelihood$likelihood)
  max_likelihood$theta <- theta[max_likelihood$index]

  limit <- qchisq(threshold_CI, df=1)/2

  CI_profile<-(likelihood-max_likelihood$likelihood+limit)^2
  lower_CI <- theta[ which( CI_profile[1:max_likelihood$index]
                          %in% min(CI_profile[1:max_likelihood$index])) ]

  upper_CI <- theta[ max_likelihood$index - 1 +
                     which( CI_profile[max_likelihood$index:length(CI_profile)]
                            %in% min(CI_profile[max_likelihood$index:length(CI_profile)])) ]

  return ( list (theta_max_likelihood = max_likelihood$theta,
                      max_likelihood = max_likelihood$likelihood,
                      lower_theta = lower_CI, upper_theta = upper_CI) )
}
