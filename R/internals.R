
## These functions are not exported, and are meant for internal use only.


## Probabilty of extinction given R and assuming a Poisson process

proba_ext_poisson <- function(alpha){
  proba_ext <- exp(-alpha)
  return(proba_ext)
}
