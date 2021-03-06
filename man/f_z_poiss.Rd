% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_z_poiss.R
\name{f_z_poiss}
\alias{f_z_poiss}
\title{Compute the probability of observing an outbreak of size z given an R<1 and assuming a Poisson offspring distribution}
\usage{
f_z_poiss(z, s, R)
}
\arguments{
\item{R}{is the reproduction number, i.e. the average number of secondary cases due to a single case.
This can be any positive number.}
}
\value{
The function returns a list including:
\itemize{

\item R_eff: the effective reproduction number. Note that if R<1, then R_eff=R.

\item alpha given the reproduction number and assuming a Poisson offspring distribution. Note that if R<1, then alpha=0.

\item proba_ext: the probability of extinction given alpha. Note that if R<1, then proba_ext=1.

}
#'
}
\description{
Compute the probability of observing an outbreak of size z given an R<1 and assuming a Poisson offspring distribution
}
\examples{

x <- R_eff_poisson(.5)
x
x <- R_eff_poisson(1.5)
x

}
\author{
Pierre Nouvellet (\email{p.nouvellet@imperial.ac.uk})
}
