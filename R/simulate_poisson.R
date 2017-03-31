#' Simulate a partially observed Poisson branching process with discrete generations
#'
#' For a given reproduction number, this function simulate the incidence of a disease at each generation.
#' The offspring distribution of each individual infected is assumed to follow a Poisson distribution with mean
#' equal to the reproduction number. To simulate under-reporting, users can specify a probability to observe case,
#'  this reporting probability is assumed constant through the simulation.
#'
#'
#' @author Pierre Nouvellet (\email{p.nouvellet@imperial.ac.uk})
#'
#' @export
#'
#' @param R is the reproduction number, i.e. the average number of secondary cases due to a single case.
#' This can be any positive number. if R is a vector, then the length of R must be 'n' (see below).
#'
#' @param n is the number of simulations performed. Each simulation start with 's'  individual(s) infected.
#' 'n' must be a positive integer.
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
#' x <- simulate_poisson(.9, 5, 1, .5, 1e2)
#' x
#'
simulate_poisson <- function(R,n,s,rho,t_max) {
	# check input
	if (length(R)!=1 && length(R)!=n) warning('R must be a vector, and its size must be either 1 or n')
	if (length(s)!=1 && length(s)!=n) warning('s must be a vector, and its size must be either 1 or n')
	if (length(rho)!=1 && length(rho)!=n) warning('rho must be a vector, and its size must be either 1 or n')
	if ((length(n)!=1 | n==0 | n!=round(n))[1]) warning('n must be a positive integer')
	if ((length(t_max)!=1 | t_max==0 | t_max!=round(t_max))[1]) warning('t_max must be a positive integer')

	# declare incidence matrices
  N <- matrix(NA,n,t_max)
  N_obs_full <- matrix(NA,n,t_max)

	# initial values
  N[,1] <- s
  N_obs_full[,1] <- rbinom(n,N[,1],rho)

	# simulate the Poisson branching process and reporting
  for (i in 2:t_max){
    N[,i] <- rpois(n,R*N[,i-1])
	  N_obs_full[,i] <- rbinom(n,N[,i],rho)
  }
	# check all simulated outbreaks are extinct
  n_ongoing <- n - sum(N[,t_max]==0)
  if (n_ongoing>0) warning(
    paste0('There are ',n_ongoing,' outbreaks that are not extinct,
           consider increasing the number of time-steps (especially if R<1).'))

	# calculate final sizes, true and observed
  z <- rowSums(N)
  z_obs_full <- rowSums(N_obs_full)

	# correct for unobserved outbreaks
  f <- which(z_obs_full==0)
  if (length(f)>0){
    N_obs <- N_obs_full[-f,]
    z_obs <- z_obs_full[-f]
  }else{
    N_obs <- N_obs_full
    z_obs <- z_obs_full
  }

	# output
  return( list(true_incidence = N, reported_incidence = N_obs_full, observed_incidence = N_obs,
               true_size = z, reported_size = z_obs_full, observed_size = z_obs) )
}
