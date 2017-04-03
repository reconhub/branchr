#
# ## These functions are not exported, and are meant for internal use only.
#
#
# <<<<<<< HEAD
# ## Proba extinction given R and poisson
# proba.ext.poisson <- function(alpha){
#   proba.ext <- exp(-alpha)
#   return(proba.ext)
# =======
# ## Probabilty of extinction given R and assuming a Poisson process
#
# proba_ext_poisson <- function(alpha){
#   proba_ext <- exp(-alpha)
#   return(proba_ext)
# }
#
#
#
#
#
#
# ## Function to test validity of alpha: must be strictly positive float, non-NA,
# ## finite
#
# check_alpha <- function(alpha) {
#
#   if (length(alpha) != 1L) {
#     stop("alpha must be exatly 1 value")
#   }
#
#   if (!is.numeric(alpha)) {
#     stop("alpha must be numeric")
#   }
#
#   if (!is.finite(alpha)) {
#     stop("alpha must be finite")
#   }
#
#   if (alpha < 0) {
#     stop("alpha must be strictly positive")
#   }
#
#   return(alpha)
# >>>>>>> origin/master
# }
