
## These functions are not exported, and are meant for internal use only.


# Proba extinction given R and poisson
proba.ext.poisson <- function(alpha){
  proba.ext <- exp(-alpha)
  return(proba.ext)
}
