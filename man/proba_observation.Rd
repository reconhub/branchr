% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proba_observation.R
\name{proba_observation}
\alias{proba_observation}
\title{Obtain intermediate useful quantities}
\usage{
proba_observation(y, rho, threshold)
}
\arguments{
\item{y:}{a vector of outbreak sizes observations.}

\item{rho:}{the probability of a single case being detected.}

\item{threshold:}{the maximum true final size of an outbreak. this need to be much higher
than the largest observed outbreak to account for reporting and properly evaluate the distribution of outbreak sizes}
}
\value{
The function returns a list including:
\itemize{

\item approx_R: a naive approximation of the reproduction number.

\item y_obs_reformat: the observed incidence over time accounting for under-reporting (rho).

\item possible_size: same as reported_incidence, but only including simulated outbreaks for which at least a
single case was observed (i.e. outbreak for which the observed final size is 0, see below reported_size, are removed).

\item p_y_z: the probability of observing y given z,rho (reporting rate), with z the true number of cases (reported and un-reported cases)

\item p_0_z: the probability of not observing an outbreak (i.e. y=0 or no cases) given z and rho.

}
#'
}
\description{
1) Rough expectation for R ($approx_R)
2) Possible outbreak sizes ($possible_size)
3) new format for observations ($y_obs_reformat)
4) g: the probability of observing y given z,rho (reporting rate), with z the true number of cases (reported and un-reported cases)  ($p_y_z)
5) g0: the probability of not observing an outbreak (i.e. y=0 or no cases) given z and rho.
}
\examples{

x <- proba_observation(c(1,2,3),.5,1e3)
x


}
\author{
Pierre Nouvellet (\email{p.nouvellet@imperial.ac.uk})
}
