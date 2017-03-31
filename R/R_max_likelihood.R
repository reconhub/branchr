#' Obatin the maximium likleihood from a vector of parameter and its likelihood profile
#'
#' Also gives confidence interval
#'
#'
#' @author Pierre Nouvellet (\email{p.nouvellet@imperial.ac.uk})
#'
#' @export
#'
#' @param theta is a vector for which the likelihood has been evaluated
#'
#' @param Likelihood is the log-likelihood values evaluated
#'
#' @param s is the number individual(s) initially infected. if s is a vector, then the length of s must be 'n' (see above).
#'
#' @param rho is the probability (between 0 and 1) that an incident case is observed, i.e. reporting probability.
#' if rho is a vector, then the length of rho must be 'n' (see below)
#'
#' @param t_max is the maximum number of generations simulated. 't_max' must be a positive integer.
#'
#' @return
#'  The function returns a list including:
#' \itemize{
#'
#' \item true_incidence: the true incidence over time.
#'
#' \item reported_incidence: the observed incidence over time accounting for under-reporting (rho).
#'
#' \item observed_incidence: same as reported_incidence, but only including simulated outbreaks for which at least a
#' single case was observed (i.e. outbreak for which the observed final size is 0, see below reported_size, are removed).
#'
#' \item true_size: the final outbreak sizes for each simulated outbreaks. Note that for outbreaks that are not extinct
#' 'final' by the end of the simulation, z_sim correspond to the cumulative incidence by time t_max.
#'
#' \item reported_size: the observed final outbreak sizes accounting for under-reporting (rho).
#'
#' \item observed_size: same as reported_size, but only including simulated outbreaks for which at least a
#' single case was observed.
#'
#' }
#'
#' Note that if some simulated outbreaks are not extinct by the end of the simulation, a warning is displayed.
#'
#' @examples
#'
#'
# obatin the maximium likleihood for R and confidence interval
R_max_likelihood <- function(theta,likelihood,threshold_CI){

  max_likelihood <- list( likelihood = max(likelihood) )
  max_likelihood$index <- which(likelihood %in% max_likelihood$likelihood)
  max_likelihood$R <- theta[max_likelihood$index]

  limit <- qchisq(threshold_CI, df=1)/2

  CI_profile<-(likelihood-max_likelihood$likelihood+limit)^2
  lower_CI <- theta[ which( CI_profile[1:max_likelihood$index]
                          %in% min(CI_profile[1:max_likelihood$index])) ]

  upper_CI <- theta[ max_likelihood$index - 1 +
                     which( CI_profile[max_likelihood$index:length(CI_profile)]
                            %in% min(CI_profile[max_likelihood$index:length(CI_profile)])) ]

  R_estimate <- list (R_max_likelihood = max_likelihood$R,
                      max_likelihood = max_likelihood$likelihood,
                      lower_R = lower_CI, upper_R = upper_CI)
}
